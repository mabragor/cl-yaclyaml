;;;; This file is one of components of CL-YACLYAML system, licenced under GPL, see COPYING for details
;;;; Presenting constructed tree as a text.

(in-package #:cl-yaclyaml)

;; implicit string coercion?

(define-condition emit-error (simple-error)
  ())

(defmacro fail-emit ()
  `(error 'emit-error))

(define-emit-rule double-quote-scalar (x)
  (emit-assert (stringp x))
  (list #\"
	(iter (for char in-string x)
	      (if (not (c-printable-p char))
		  (progn (collect #\\)
			 (a:aif (simple-escape char)
				(collect it)
				(appending (complex-escape char))))
		  (collect char)))
	#\"))

;; TODO: smart line-breaking

(define-emit-rule single-quote-scalar (x)
  (emit-assert (stringp x))
  (list #\'
	(iter (for char in-string x)
	      (cond ((not (c-printable-p char)) (fail-emit))
		    ((char= char #\') (progn (collect #\')
					     (collect #\')))
		    (t (collect char))))
	#\'))

(defun safe-aref (array index)
  (handler-case (aref array index)
    (t () nil)))

(define-emit-rule plain-scalar (x)
  (emit-assert (and (stringp x)
		    (has-no-bounding-whitespace-p x)
		    (safely-begins-p x)))
  (iter (for char in-string x with-index i)
	(cond ((not (c-printable-p char)) (fail-emit))
	      ((and (or (char= #\: char) (char= #\# char))
		    (char= #\space (safe-aref x (1+ i))))
	       (fail-emit))
	      (t (collect char)))))



