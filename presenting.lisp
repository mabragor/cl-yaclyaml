;;;; This file is one of components of CL-YACLYAML system, licenced under GPL, see COPYING for details

(in-package #:cl-yaclyaml)

;;; Present serialization tree as raw text.

(eval-always
  (define-emit-env yaclyaml))

(eval-always
  (register-yaclyaml-emit-context context block-out block-in flow-out flow-in block-key flow-key)
  (register-yaclyaml-emit-context n nil)
  (setf n -1)
  (register-yaclyaml-emit-context indent-style determined autodetect)
  (register-yaclyaml-emit-context block-scalar-chomping clip keep strip)
  (register-yaclyaml-emit-context block-scalar-style literal folded))

(define-emit-rule indent () ((make-string n :initial-element #\space)) ((>= n 0)))
(define-emit-rule b-break () ((list #\newline (descend 'indent))))

(define-emit-rule double-b-break x
  ((if (or (block-key-context-p x) (flow-key-context-p x))
       "\\n"
       (descend 'b-break)))
  ((char= x #\newline)))

(define-emit-rule double-esc-char x
  ((list #\\ x))
  ((member x '(#\\ #\") :test #'char=)))

(define-emit-rule single-esc-char x
  ((declare (ignore x)) "''")
  ((char= x #\')))

(define-emit-rule double-np-char x
  ((let ((code (char-code x)))
     (cond ((< code (expt 2 8)) (format nil "\\x~2,'0x" code))
	   ((< code (expt 2 16)) (format nil "\\u~4,'0x" code))
	   ((< code (expt 2 32)) (format nil "\\U~8,'0x" code))
	   (t (fail-emit "Char code ~a too big to express as 32-bit escaped char" code)))))
  ((not (c-printable-p x))))

(define-emit-rule double-printable-char x
  (x)
  ((c-printable-p x)))

(define-emit-rule double-line-breaks x
  ((if (not (or (block-key-context-p x) (flow-key-context-p x)))
       (incf x))
   (iter (for i from 1 to x)
	 (collect (descend 'double-b-break #\newline)))))

(defparameter *min-line-length* 10)
(defparameter *max-line-length* 80)

(defparameter *line-breaking-style* :none)

(defmacro tackle-newlines (char-var)
  `(if (char= ,char-var #\newline)
       (let ((nchars 1))
	 (iter (while (< pos (1- length)))
	       (if (equal (and (setf char-picked-p t)
			       (in outer (next ,char-var)))
			  #\newline)
		   (incf nchars)
		   (terminate)))
	 (setf current-line-length 0)
	 (descend 'double-line-breaks nchars))
       (fail-emit)))

(defmacro next-char-if-not-picked (char-var)
  `(if (not char-picked-p)
       (next ,char-var)
       (setf char-picked-p nil)))


(define-emit-rule double-quoted-scalar str
  ((let (char-picked-p
	 (length (length str))
	 (current-line-length (1+ n)) ; 1+ is for the preceding doule quote
	 (*line-breaking-style* (if (or (flow-key-context-p t) (block-key-context-p t))
				    :none
				    *line-breaking-style*)))
     (iter outer (generate char in-string str with-index pos)
	   (next-char-if-not-picked char)
	   (let ((next-portion (text (|| ;; probably this would be factored out into macro
				      (tackle-newlines char)
				      (descend 'double-esc-char char)
				      (descend 'double-printable-char char)
				      (descend 'double-np-char char)))))
	     (case *line-breaking-style*
	       (:none nil)
	       (:simple (when (and (>= current-line-length (+ *min-line-length* n))
				   (>= current-line-length *max-line-length*))
			  ;; very-very simple - correct is to analyze if NEXT-PORTION begins with whitespace
			  ;; and to act accordingly
			  (collect #\\ into res)
			  (collect (descend 'double-b-break #\newline) into res)
			  (setf current-line-length n)))
	       (:strict (fail-emit ":STRICT line breaking style is not yet implemented."))
	       (:word-wise (fail-emit ":WORD-WISE line breaking style is not yet implemented."))
	       (t (fail-emit "Unexpected line-breaking-style ~a" *line-breaking-style*)))
	     (collect next-portion into res)
	     (if (find #\newline next-portion)
		 (setf current-line-length n)
		 (incf current-line-length (length next-portion))))
	   (finally (return-from outer (text `(#\" ,. res #\")))))))
  ((stringp str)))


(define-emit-rule single-quoted-scalar str
  ((let (char-picked-p
	 (length (length str))
	 (current-line-length (1+ n)) ; 1+ is for the preceding doule quote
	 (*line-breaking-style* (if (or (flow-key-context-p t) (block-key-context-p t))
				    :none
				    *line-breaking-style*)))
     (iter outer (generate char in-string str with-index pos)
	   (next-char-if-not-picked char)
	   (case *line-breaking-style*
	     (:none nil)
	     ((:simple :strict :word-wise) ; we really have only one option for line folding here
	      (when (and (>= current-line-length (+ *min-line-length* n))
			 (>= current-line-length *max-line-length*)
			 (char= char #\space)
			 (not (find (char str (1- pos)) '(#\newline #\space #\tab) :test #'char=))
			 (and (< pos (1- length))
			      (not (find (char str (1+ pos)) '(#\newline #\space #\tab) :test #'char=))))
		(collect (descend 'double-b-break #\newline) into res)
		(setf current-line-length n)
		(next-iteration)))
	     (t (fail-emit "Unexpected line-breaking-style ~a" *line-breaking-style*)))
	   (multiple-value-bind (next-portion new-length)
	       (|| (let ((content (tackle-newlines char)))
		     (values content n))
		   (values (descend 'single-esc-char char) (+ 2 current-line-length))
		   (values (descend 'double-printable-char char) (+ 1 current-line-length)))
	     (collect next-portion into res)
	     (setf current-line-length new-length))
	   (finally (return-from outer (text `(#\' ,. res #\')))))))
  ((cond ((not (every #'c-printable-p str)) (fail-emit "can only contain printable characters"))
	 ((and (or (flow-key-context-p str)
		   (block-key-context-p str))
	       (some (lambda (x) (char= x #\newline)) str))
	  (fail-emit "newlines are not allowed inside implicit keys"))
	 (t t))))

(define-emit-rule plain-scalar str
  ((handler-case (yaclyaml-parse 'emit-ns-plain str)
     ;; emit errors are just propagated to the top
     (parse-error () (fail-emit "plain scalar has wrong structure"))))
  ((stringp str)))

(defun restructure-plain-line (first rest)
  (let ((cur-word (list first))
	cur-word-bunch
	word-bunches)
    (macrolet ((push-reset-list-acc (acc where reset-with)
		 `(progn (push (nreverse ,acc) ,where)
			 (setf ,acc ,reset-with))))
      (iter (for (white char) in rest)
	    (if (not white)
		(push char cur-word)
		(if (and (equal 1 (length white)) (equal " " (car white)))
		    ;; we may break the scalar here
		    (progn (push-reset-list-acc cur-word cur-word-bunch (list char))
			   (push-reset-list-acc cur-word-bunch word-bunches nil))
		    ;; we just record current whitespace into the word bunch
		    (progn (push-reset-list-acc cur-word cur-word-bunch (list char))
			   (push (text white) cur-word-bunch))))
	    (finally (push (nreverse cur-word) cur-word-bunch)
		     (push (nreverse cur-word-bunch) word-bunches)
		     (return (mapcar #'text (nreverse word-bunches))))))))
  
(defun joinl (joinee lst)
  (format nil (strcat "~{~a~^" joinee "~}") lst))

(defun emit-maybe-with-folding (bunches)
  (if (or (flow-key-context-p t) (block-key-context-p t)) ; T is just a placeholder for a argument
      (joinl " " bunches)
      (case *line-breaking-style*
	(:none (joinl " " bunches))
	(:simple (let ((current-line-length -1)
		       cur-line
		       lines
		       (line-break (text (descend 'b-break)))) ; will this work right?
		   (iter (for bunch in bunches)
			 (push bunch cur-line)
			 (incf current-line-length (+ 1 (length bunch)))
			 (when (and (>= current-line-length (+ *min-line-length* n))
				    (>= current-line-length *max-line-length*))
			   (push (joinl " " (nreverse cur-line)) lines)
			   (setf cur-line nil))
			 (finally (if cur-line
				      (push (joinl " " (nreverse cur-line)) lines))
				  (format t "~s~%" lines)
				  (return (joinl line-break (nreverse lines)))))))
	((:strict :word-wise) (fail-emit "Sorry, not yet implemented."))
	(t (fail-emit "Unexpected line-breaking-style ~a" *line-breaking-style*)))))
  

(define-rule emit-ns-plain-one-line (and ns-plain-first nb-ns-plain-in-line)
  (:destructure (first rest)
		(emit-maybe-with-folding (restructure-plain-line first rest))))
			
(define-rule emit-s-flow-folded (+ #\newline)
  (:lambda (lst)
    (iter (for i from 0 to (length lst))
	  (collect (descend 'b-break)))))

(define-rule emit-s-ns-plain-next-line (and emit-s-flow-folded ns-plain-char nb-ns-plain-in-line)
  (:destructure (newlines first rest)
		(let ((bunches (restructure-plain-line first rest)))
		  (list newlines (emit-maybe-with-folding bunches)))))
		
(define-rule emit-ns-plain-multi-line (and emit-ns-plain-one-line
					   (* emit-s-ns-plain-next-line))
  (:text t))
		

(define-rule emit-ns-plain (cond ((or flow-out-context
				      flow-in-context
				      block-out-context
				      block-in-context) emit-ns-plain-multi-line)
				 ((or block-key-context flow-key-context) emit-ns-plain-one-line)))

