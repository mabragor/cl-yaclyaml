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
	       (defrule ,symbol ,expression ,@options)))))

;; This is the example of macroexpansion
(define-esrap-env yaclyaml)
