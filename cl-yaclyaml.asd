;;;; cl-yaclyaml.asd

(defpackage :cl-yaclyaml-system
  (:use :cl :asdf))

(in-package cl-yaclyaml-system)

(defsystem #:cl-yaclyaml
  :version "0.2"
  :serial t
  :description "Yet Another Common Lisp YaML processor."
  :author "Alexander Popolitov <popolit@itep.ru>"
  :license "GPL"
  :depends-on (#:iterate #:rutils #:cl-test-more #:cl-interpol
			 #:esrap #:alexandria #:yaclanapht #:cl-ppcre #:defmacro-enhance #:parse-number)
  :components ((:file "package")
	       (:file "esrap-parser-basics")
	       (:file "parsing")
	       (:file "composing")
	       (:file "constructing")
	       ;; (:file "cl-yaclyaml-new")
               ;; (:file "cl-yaclyaml")
               ;; (:file "cl-yaclyaml-t")))
	       ))

(defsystem :cl-yaclyaml-tests
  :description "Tests for CL-YACLYAML."
  :licence "GPL"
  :depends-on (:cl-yaclyaml :eos :cl-interpol)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-yaclyaml))))
  (load-system :cl-yaclyaml-tests)
  (funcall (intern "RUN-TESTS" :cl-yaclyaml-tests)))
