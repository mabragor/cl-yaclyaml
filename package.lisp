;;;; package.lisp

(defpackage #:cl-yaclyaml
  (:nicknames #:cl-yy)
  (:use #:cl #:iterate #:rutils.string
	#:esrap-liquid #:defmacro-enhance #:cl-read-macro-tokens
	#:parse-number #:cl-ppcre)
  (:shadowing-import-from #:cl-test-more
                          :ok :plan :finalize :is :isnt :is-expand :diag)
  (:shadowing-import-from #:alexandria #:flatten #:symbolicate #:make-keyword)
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
           #:convert-scalar #:convert-sequence #:convert-mapping
           #:convert-sequence-to-list #:convert-mapping-to-hashtable))
