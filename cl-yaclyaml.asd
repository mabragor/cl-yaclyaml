;;;; cl-yaclyaml.asd

(asdf:defsystem #:cl-yaclyaml
  :serial t
  :description "Yet Another Common Lisp YaML processor."
  :author "Alexander Popolitov <popolit@itep.ru>"
  :license "GPL"
  :depends-on (#:iterate #:rutils #:cl-test-more #:cl-interpol
			 #:esrap #:alexandria)
  :components ((:file "package")
	       (:file "esrap-parser-basics")
	       (:file "cl-yaclyaml-new")
               ;; (:file "cl-yaclyaml")
               ;; (:file "cl-yaclyaml-t")))
	       ))

