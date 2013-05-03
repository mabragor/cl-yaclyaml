;;; Basic constructions needed to start writing an ESRAP-based parser
;;; Pilfered from cl-closure-templates

(in-package #:esrap-parser-basics)

(defmacro define-esrap-env (symbol)
  `(progn (eval-always
	    (defvar ,(sb-int:symbolicate symbol "-RULES") (make-hash-table))
	    (defvar ,(sb-int:symbolicate symbol "-CONTEXTS") nil))
	  (defmacro ,(sb-int:symbolicate "WITH-" symbol "-RULES") (&body body)
	    `(let ((esrap::*rules* ,',(sb-int:symbolicate symbol "-RULES")))
	       ,@body))
	  (defmacro ,(sb-int:symbolicate "WITH-" symbol "-CONTEXTS") (&body body)
	    `(let ((esrap::contexts ,',(sb-int:symbolicate symbol "-CONTEXTS")))
	       ,@body))
	  (defmacro ,(sb-int:symbolicate 'define-rule) (symbol expression &body options)
	    `(,',(sb-int:symbolicate "WITH-" symbol "-RULES")
		 (,',(sb-int:symbolicate "WITH-" symbol "-CONTEXTS")
		     (defrule ,symbol ,expression ,@options))))
	  (defmacro ,(sb-int:symbolicate "REGISTER-" symbol "-CONTEXT")
	      (context-var &rest plausible-contexts)
	    `(progn (defparameter ,context-var ,(sb-int:keywordicate (format nil "~a" (car plausible-contexts))))
		    ,@(mapcar (lambda (context-name)
				(let ((pred-name (sb-int:symbolicate context-name
								     "-"
								     context-var
								     "-P"))
				      (rule-name (sb-int:symbolicate context-name
								     "-"
								     context-var)))
				  `(progn
				     (defun ,pred-name (x)
				       (declare (ignore x))
				       (equal ,context-var ,(sb-int:keywordicate context-name)))
				     (,(sb-int:symbolicate 'define-rule) ,rule-name (,pred-name "")
				       (:constant nil)))))
			      (mapcar (lambda (x) (format nil "~a" x)) plausible-contexts))
		    (push ',context-var ,',(sb-int:symbolicate symbol "-CONTEXTS"))))
	  (defmacro ,(sb-int:symbolicate symbol "-PARSE")
	      (expression text &key (start nil start-p)
				 (end nil end-p)
				 (junk-allowed nil junk-allowed-p))
	    `(,',(sb-int:symbolicate "WITH-" symbol "-RULES")
		 (,',(sb-int:symbolicate "WITH-" symbol "-CONTEXTS")
		     (parse ,(if (and (consp expression)
				      (eql (car expression) 'quote)
				      (equal (length expression) 2)
				      (symbolp (cadr expression))
				      (not (keywordp (cadr expression))))
				 `',(intern (string (cadr expression))
					    ',*package*)
				 expression)
			    ,text
			    ,@(if start-p `(:start ,start))
			    ,@(if end-p `(:end ,end))
			    ,@(if junk-allowed-p
				  `(:junk-allowed ,junk-allowed))))))))

			

;; This is the example of macroexpansion
#+nil
(define-esrap-env yaclyaml)
