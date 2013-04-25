(in-package :cl-user)

(defpackage :cl-yaclyaml-tests
  (:use :alexandria :cl :cl-yaclyaml :eos)
  (:export #:run-tests))

(in-package :cl-yaclyaml-tests)

(cl-interpol:enable-interpol-syntax)

(def-suite yaclyaml)
(in-suite yaclyaml)

(defun run-tests ()
  (let ((results (run 'yaclyaml)))
    (eos:explain! results)
    (unless (eos:results-status results)
      (error "Tests failed."))))

(test block-scalar-header
  (is (equal '((:block-indentation-indicator . "3") (:block-chomping-indicator . "-"))
	     (yaclyaml-parse 'c-b-block-header #?"3- #asdf\n"))))

