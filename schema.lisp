;;;; This file is one of components of CL-YACLYAML system, licenced under GPL, see COPYING for details
(in-package :cl-yaclyaml)

(defclass yaml-schema () ()
  (:documentation "Base class for yaml schemas."))

(defgeneric install-converters (schema)
  (:method-combination progn :most-specific-last)
  (:documentation "Install converters for a schema before parsing."))

(defclass yaml-schema-unspecific-mixin (yaml-schema) ()
  (:documentation "Mixin to add a scalar-converter for :non-specific that
calls CONVERT-NON-SPECIFIC-SCALAR with the schema and the content."))


(defgeneric convert-non-specific-scalar (schema content)
  (:documentation "Convert the content of a non-specific scalar node based
on the schema. Default implementation just calls CONVERT-SCALAR with the str tag.

Implementations should call CALL-NEXT-METHOD for contents it doesn't know how to process.")
  (:method (schema content)
    (declare (ignore schema))
    (convert-scalar content "tag:yaml.org,2002:str")))

(defmethod install-converters progn ((schema yaml-schema-unspecific-mixin))
  (install-scalar-converter :non-specific
                            (lambda (content)
                              (convert-non-specific-scalar schema content))))


(defmacro with-schema (schema &body body)
  "Execute BODY in a context where SCALAR-CONVERTERS, SEQUENCE-CONVERTERS, and
MAPPING-CONVERTERS have been bound to new hash tables with converters
installed by the schema."
  `(let ((scalar-converters (make-hash-table :test #'equal))
         (sequence-converters (make-hash-table :test #'equal))
         (mapping-converters (make-hash-table :test #'equal)))
     (install-converters ,schema)
     ,@body))

(defclass failsafe-schema (yaml-schema) ()
  (:documentation "Implementation of the failsafe schema for yaml."))

(defmethod install-converters progn ((schema failsafe-schema))
  (install-scalar-converter "tag:yaml.org,2002:str" #'trivial-scalar-converter)
  (install-sequence-converter "tag:yaml.org,2002:seq" #'convert-sequence-to-list)
  (install-mapping-converter "tag:yaml.org,2002:map" #'convert-mapping-to-hashtable))

(defclass json-schema (failsafe-schema yaml-schema-unspecific-mixin) ()
  (:documentation "Implementation of the json schema for ymal."))

(defmethod install-converters progn ((schema json-schema))
  (install-scalar-converter "tag:yaml.org,2002:null" (lambda (content)
                                                       (declare (ignore content))))
  (install-scalar-converter "tag:yaml.org,2002:bool"
                               (lambda (content)
                                 (if (equal content "true")
                                     t
                                     (if (equal content "false")
                                         nil
                                         (error "Expected 'true' or 'false' but got ~a." content)))))
     (install-scalar-converter "tag:yaml.org,2002:int"
                               (lambda (content)
                                 (parse-integer content)))
     (install-scalar-converter "tag:yaml.org,2002:float"
                               (lambda (content)
                                 (parse-real-number content)))
     (install-sequence-converter :non-specific
                                 (lambda (content)
                                   (convert-sequence content "tag:yaml.org,2002:seq")))
     (install-mapping-converter :non-specific
                                (lambda (content)
                                  (convert-mapping content "tag:yaml.org,2002:map"))))

(defmethod convert-non-specific-scalar ((schema json-schema) content)
  (cond ((or (equal content :empty)
             (all-matches "^null$" content))
         (convert-scalar content "tag:yaml.org,2002:null"))
        ((all-matches "^(true|false)$" content) (convert-scalar content
                                                                "tag:yaml.org,2002:bool"))
        ((all-matches "^-?(0|[1-9][0-9]*)$" content) (convert-scalar content
                                                                     "tag:yaml.org,2002:int"))
        ((all-matches "^-?(0|[1-9][0-9]*)(\\.[0-9]*)?([eE][-+]?[0-9]+)?$" content)
         (convert-scalar content "tag:yaml.org,2002:float"))
        (t (error "Could not resolve scalar: ~a" content))))

(defclass core-schema (json-schema yaml-schema-unspecific-mixin) ()
  (:documentation "Implementation of the core schema for yaml."))

(defmethod convert-non-specific-scalar ((schema core-schema) content)
  (cond ((or (equal content :empty)
             (all-matches "^(null|Null|NULL|~)$" content))
         (convert-scalar content "tag:yaml.org,2002:null"))
        ((all-matches "^(true|True|TRUE|false|False|FALSE)$" content)
         (convert-scalar (string-downcase content) "tag:yaml.org,2002:bool"))
        ((all-matches "^[-+]?(0|[1-9][0-9]*)$" content)
         (convert-scalar content "tag:yaml.org,2002:int"))
        ((all-matches "^0o[0-7]+$" content)
         (parse-integer content :start 2 :radix 8))
        ((all-matches "^0x[0-9a-fA-F]+$" content)
         (parse-integer content :start 2 :radix 16))
        ((all-matches "^[-+]?(\\.[0-9]+|[0-9]+(\.[0-9]*)?)([eE][-+]?[0-9]+)?$" content)
         (convert-scalar content "tag:yaml.org,2002:float"))
        ((all-matches "^[-+]?(\\.inf|\\.Inf|\\.INF)$" content)
         :infinity)
        ((all-matches "^[-+]?(\\.nan|\\.NaN|\\.NAN)$" content)
         :nan)
        (t
         (convert-scalar content "tag:yaml.org,2002:str"))))

(defparameter schemas (make-hash-table :test 'eq)
  "Hash table of available schemas. The keys are symbols and the
values are instances of YAML-SCHEMA.")

(defun register-schema (name class-name &rest init-args)
  (declare (type symbol name class-name))
  "Register a schema with a symbol. After registering, the symbol can be passed
as the SCHEMA argument to the parsing functions.

This function creates a new instance of CLASS-NAME with the arguments passed as
INIT-ARGS and registers it with the symbol NAME.

NAME: The symbol for the schema to be registered.
CLASS-NAME: The symbol for the name of the class of the schema to create and register.
INIT-ARGS: Additional arguments to pass to MAKE-INSTANCE."
  (setf (gethash name schemas)
        (apply #'make-instance class-name init-args)))

;;; Initially populate the schemas table.
(register-schema :failsafe 'failsafe-schema)
(register-schema :json 'json-schema)
(register-schema :core 'core-schema)
