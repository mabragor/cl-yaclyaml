;;;; cl-yaclyaml.asd

(asdf:defsystem #:cl-yaclyaml
  :serial t
  :description "Yet Another Common Lisp YaML processor."
  :author "Alexander Popolitov <popolit@itep.ru>"
  :license "GPL"
  :depends-on (#:iterate #:rutils #:cl-test-more #:cl-interpol)
  :components ((:file "package")
               (:file "cl-yaclyaml")
               (:file "cl-yaclyaml-t")))

