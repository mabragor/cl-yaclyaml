;;;; package.lisp

(defpackage #:cl-yaclyaml
  (:nicknames #:cl-yy)
  (:use #:cl #:iterate #:rutils.string #:esrap-liquid #:parse-number #:cl-ppcre)
  ;; (:shadowing-import-from #:cl-test-more
  ;;                         :ok :plan :finalize :is :isnt :is-expand :diag)
  (:shadowing-import-from #:alexandria #:flatten #:symbolicate #:make-keyword #:with-gensyms #:once-only)
  (:export #:yaclyaml-parse #:yy-parse
	   #:ncompose-representation-graph #:construct #:yaml-load #:hash->assoc #:yaml-simple-load
	   #:yaml-load-file #:define-yaml-config
	   #:nserialize #:represent-node))
