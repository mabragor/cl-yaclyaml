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

(defmacro with-in-string-iter (str-var &body body)
  `(let (char-picked-p
	 (length (length ,str-var))
	 (current-line-length (1+ n)) ; 1+ is for the preceding doule quote
	 (*line-breaking-style* (if (or (flow-key-context-p t) (block-key-context-p t))
				    :none
				    *line-breaking-style*)))
     (iter outer (generate char in-string ,str-var with-index pos)
	   (next-char-if-not-picked char)
	   ,@body)))


(define-emit-rule double-quoted-scalar str
  ((with-in-string-iter str
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
     (finally (return-from outer (text `(#\" ,. res #\"))))))
  ((stringp str)))

(define-emit-rule single-quoted-scalar str
  ((with-in-string-iter str
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
     (finally (return-from outer (text `(#\' ,. res #\'))))))
  ((cond ((not (every #'c-printable-p str)) (fail-emit "can only contain printable characters"))
	 ((and (or (flow-key-context-p str)
		   (block-key-context-p str))
	       (some (lambda (x) (char= x #\newline)) str))
	  (fail-emit "newlines are not allowed inside implicit keys"))
	 (t t))))

(defun b-char-p (x)
  (char= x #\newline))

(defun nb-char-p (x)
  (and (c-printable-p x)
       (not (b-char-p x)
	    ;; TODO not byte-order-mark
	    )))

(defun s-white-p (x)
  (or (char= x #\tab) (char= x #\space)))

(defun ns-char-p (x)
  (and (nb-char-p x) (not (s-white-p x))))

(defun c-indicator-p (x)
  (find x '(#\- #\? #\: #\, #\[ #\] #\{ #\} #\# #\& #\* #\! #\| #\> #\' #\" #\% #\@ #\`) :test #'char=))

;; Will do this some other day.
;; Now I want to try to tackle non-string data, such as flow collections.
;; (define-emit-rule plain-scalar str
;;   ((if-first-time (if (or (and (ns-char-p char) (not (c-indicator-p char)))
;; 			  (and (find char '(#\? #\: #\-) :test #'char=)
;; 			       (strictly-followed-by #'ns-plain-safe)))
;; 		      (collect char into res)
;; 		      (fail-emit "Invalid character ~a as a first character of plain scalar"))
;; 		  (multiple-value-bind (next-portion new-length)
;; 		      (|| (let ((content (tackle-newlines char)))
;; 			    (values content n))
;; 			  (values (descend 'double-printable-char char) (+ 1 current-line-length)))
;; 		    (collect next-portion into res)
;; 		    (setf current-line-length new-length))
;; 		  (finally (return-from outer (text res)))))
;;   ((cond ((not (stringp str)) nil)
;; 	 ((equal 0 (length str)) (fail-emit "Empty plain scalars are so far not supported."))
;; 	 (t t))))

(define-emit-rule c-flow-sequence (lst)
  ((list #\[
	 (let ((context (in-flow context)))
	   (iter (for node in lst)
		 (|| (descend 'ns-flow-pair node)
		     (descend 'ns-flow-node node))))
	 #\]))
  ((consp lst)))

(define-emit-rule c-flow-mapping (lst)
  ((list #\{
	 (let ((context (in-flow context)))
	   (iter (for node in lst)
		 (|| (descend 'ns-flow-map-implicit-entry node)
		     (descend 'ns-flow-map-explicit-entry node))))
	 #\}))
  ((and (consp lst) (eql (car lst) :mapping))))

