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

(define-emit-rule double-quoted-scalar str
  ((text (iter (for char in-string str)
	       (collect (|| (descend 'double-esc-char char)
			    (descend 'double-printable-char char)
			    (descend 'double-np-char char))
		 into res)
	       (finally (return `(#\" ,. res #\"))))))
  ((stringp str)))
