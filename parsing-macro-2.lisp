(in-package #:cl-yaclyaml)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-alias-rule (alias rule)
    `(define-yaclyaml-rule ,alias ()
			   (progn ,(maybe-wrap-in-descent rule)
				  nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-context-forcing-rule (context-name forsee-name &optional (expression forsee-name))
    `(define-yaclyaml-rule ,(symbolicate context-name "-" forsee-name) ()
       (let ((context ,(make-keyword  context-name)))
	 ,(maybe-wrap-in-descent expression)))))

;; KLUDGE: better to learn how to define alias names for ESRAP contexts
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-yy-rule (symbol args &body body)
    `(define-yaclyaml-rule ,symbol ,args ,@body))
  (defmacro yy-parse (expression text &key (start nil start-p)
					(end nil end-p)
					(junk-allowed nil junk-allowed-p))
    `(yaclyaml-parse ,expression ,text
		     ,@(if start-p `(:start ,start))
		     ,@(if end-p `(:end ,end))
		     ,@(if junk-allowed-p
			   `(:junk-allowed ,junk-allowed)))))
