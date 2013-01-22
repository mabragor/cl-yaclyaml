;;;; package.lisp

(defpackage #:cl-yaclyaml
  (:use #:cl #:iterate #:rutils.string)
  (:shadowing-import-from #:cl-test-more
                          :ok :plan :finalize :is :isnt :is-expand :diag))
