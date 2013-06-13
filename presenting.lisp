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

(define-emit-rule double-quoted-scalar str
  ((let (char-picked-p
	 (length (length str))
	 (current-line-length (1+ n)) ; 1+ is for the preceding doule quote
	 (*line-breaking-style* (if (or (flow-key-context-p t) (block-key-context-p t))
				    :none
				    *line-breaking-style*)))
     (iter outer (generate char in-string str with-index pos)
	   (if (not char-picked-p)
	       (next char)
	       (setf char-picked-p nil))
	   (let ((next-portion (text (|| ;; probably this would be factored out into macro
				      (if (char= char #\newline)
					  (let ((nchars 1))
					    (iter (while (< pos (1- length)))
						  (if (equal (and (setf char-picked-p t)
								  (in outer (next char)))
							     #\newline)
						      (incf nchars)
						      (terminate)))
					    (setf current-line-length 0)
					    (descend 'double-line-breaks nchars))
					  (fail-emit))
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
