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
