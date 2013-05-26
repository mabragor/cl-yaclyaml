;;;; This file is one of components of CL-YACLYAML system, licenced under GPL, see COPYING for details

(in-package #:cl-yaclyaml)

;;; Present serialization tree as raw text.

(define-emit-env yaclyaml)

(register-yaclyaml-emit-context context block-out block-in flow-out flow-in block-key flow-key)
