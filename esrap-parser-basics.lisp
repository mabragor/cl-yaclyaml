;;; Basic constructions needed to start writing an ESRAP-based parser
;;; Pilfered from cl-closure-templates

(in-package #:esrap-parser-basics)

(defmacro define-esrap-env (symbol)
  `(progn (eval-always
	    (defvar ,(sb-int:symbolicate symbol "-RULES") (make-hash-table)))
	  (defmacro ,(sb-int:symbolicate "WITH-" symbol "-RULES") (&body body)
	    `(let ((esrap::*rules* ,',(sb-int:symbolicate symbol "-RULES")))
	       ,@body))
	  (defmacro ,(sb-int:symbolicate 'define-rule) (symbol expression &body options)
	    `(,',(sb-int:symbolicate "WITH-" symbol "-RULES")
	       (defrule ,symbol ,expression ,@options)))
	  (defmacro ,(sb-int:symbolicate symbol "-PARSE")
	      (expression text &key (start nil start-p)
				 (end nil end-p)
				 (junk-allowed nil junk-allowed-p))
	    `(,',(sb-int:symbolicate "WITH-" symbol "-RULES")
		 (parse ,(if (and (consp expression)
				  (eql (car expression) 'quote)
				  (equal (length expression) 2)
				  (symbolp (cadr expression))
				  (not (keywordp (cadr expression))))
			     `',(intern (string (cadr expression)) ',*package*)
			     expression)
			,text
			,@(if start-p `(:start ,start))
			,@(if end-p `(:end ,end))
			,@(if junk-allowed-p `(:junk-allowed ,junk-allowed)))))))
			

;; This is the example of macroexpansion
#+nil
(define-esrap-env yaclyaml)
