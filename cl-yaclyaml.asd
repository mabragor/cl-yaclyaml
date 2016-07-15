;;;; cl-yaclyaml.asd

(defpackage :cl-yaclyaml-system
  (:use :cl :asdf))

(in-package cl-yaclyaml-system)

(defsystem #:cl-yaclyaml
  :version "1.1"
  :serial t
  :description "Yet Another Common Lisp YaML processor."
  :author "Alexander Popolitov <popolit@itep.ru>"
  :license "GPL"
  :depends-on (#:iterate #:rutils #:cl-test-more #:cl-interpol
			 #:esrap-liquid #:alexandria
			 #:cl-ppcre #:parse-number)
  :components ((:file "package")
               ;; (:file "esrap-parser-basics")
               (:file "macro-utils")
               (:file "parsing-macro")
               (:file "parsing-macro-2")
               (:file "parsing")
               (:file "composing")
               (:file "convert")
               (:file "schema")
               (:file "constructing")
               (:file "representing")
               (:file "serializing")
               ;; (:file "cl-yaclyaml-new")
               ;; (:file "cl-yaclyaml")
               ;; (:file "cl-yaclyaml-t")))
               ))

(defsystem :cl-yaclyaml-tests
  :description "Tests for CL-YACLYAML."
  :licence "GPL"
  :depends-on (:cl-yaclyaml :fiveam :cl-interpol)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-yaclyaml))))
  (load-system :cl-yaclyaml-tests)
  (funcall (intern "RUN-TESTS" :cl-yaclyaml-tests)))
