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
           #:nserialize #:represent-node
           ;; For custom schemas:
           #:yaml-schema #:install-converters
           #:yaml-schema-unspecific-mixin #:convert-non-specific-scalar
           #:failsafe-schema #:json-schema #:core-schema
           #:register-schema
           ;; Conversion of parsed data
           #:install-scalar-converter #:install-sequence-converter #:install-mapping-converter
           #:install-sequence-list-converter #:install-mapping-hashtable-converter
           #:convert-node #:convert-scalar #:convert-sequence #:convert-mapping
           #:convert-sequence-to-list #:convert-mapping-to-hashtable))
