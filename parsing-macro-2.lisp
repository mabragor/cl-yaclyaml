(in-package #:cl-yaclyaml)

(enable-read-macro-tokens)

(defmacro!! define-alias-rules (clauses)
    ()
  `(progn ,@(mapcar (lambda (x)
		      `(define-yaclyaml-rule ,(car x) ()
			 (progn ,(cadr x)
				nil)))
		    clauses)))

(defmacro!! define-context-forcing-rule (context-name forsee-name &optional (expression forsee-name))
    ()
  `(define-yaclyaml-rule ,(symbolicate context-name "-" forsee-name) ()
       (let ((context ,(zs3::keywordify context-name)))
	 ,expression)))
