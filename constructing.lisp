;;;; This file is one of components of CL-YACLYAML system, licenced under GPL, see COPYING for details

(in-package #:cl-yaclyaml)

(defun construct (representation-graph &key (schema :core))
  (let ((visited-nodes (make-hash-table :test #'eq))
	(converted-nodes (make-hash-table :test #'eq))
	(initialization-callbacks (make-hash-table :test #'eq)))
    (let ((schema (if (symbolp schema)
                      (gethash schema schemas)
                      schema)))
      (with-schema schema
        (%depth-first-traverse (list representation-graph))))
    (gethash representation-graph converted-nodes)))

(defun yaml-load (string &key (schema :core))
  (iter (for (document content) in (ncompose-representation-graph (yaclyaml-parse 'l-yaml-stream string)))
	(collect `(:document ,(construct content :schema schema)))))


(defun yaml-simple-load (string &key (schema :core))
  (let ((result (yaml-load string :schema schema)))
    (if (equal (length result) 1)
	(cadar result)
	(error "Simple form assumed, but multiple documents found in the input."))))

(define-condition yaml-load-file-error (simple-error)
  ((message :initarg :message :reader yaml-load-file-error-message)))

(defun yaml-load-file (path &key (schema :core) (size-limit 4096) (on-size-exceed :error)
			      (simple t))
  (when (probe-file path)
    (let ((file-length (with-open-file (stream path :element-type '(unsigned-byte 8))
			 (file-length stream))))
      (if (< size-limit file-length)
	  (cond
	    ((eq :error on-size-exceed)
	     (error 'yaml-load-file-error :message "Size of file exceeds the limit set in SIZE-LIMIT"))
	    ((eq :warn on-size-exceed)
	     (warn "Size of file exceeds the limit set in SIZE-LIMIT, ignoring config"))
	    ((eq nil on-size-exceed) nil))
	  (with-open-file (stream path)
	    (let ((seq (make-string file-length)))
	      (read-sequence seq stream)
	      (if simple
		  (yaml-simple-load seq :schema schema)
		  (yaml-load seq :schema schema))))))))

(defmacro define-yaml-config (reader-name (path var
						&key schema size-limit (on-size-exceed :warn))
			      &rest variable-specs)
  (multiple-value-bind (decls var-names field-names)
      (iter (for spec in variable-specs)
	    (if (atom (car spec))
		(progn (collect spec into declarations)
		       (collect (car spec) into var-names)
		       (collect (string-trim '(#\* #\+) (string-downcase (car spec))) into field-names))
		(progn (collect `(,(caar spec) ,@(cdr spec)) into declarations)
		       (collect (caar spec) into var-names)
		       (collect (cadar spec) into field-names)))
	    (finally (return (values declarations var-names field-names))))
    `(progn ,@(mapcar (lambda (x)
			`(defvar ,@x))
		      decls)
	    (defvar ,var nil)
	    (defun ,reader-name ()
	      (setf ,var (yaml-load-file ,path
					 ,@(if schema `(:schema ,schema))
					 ,@(if size-limit `(:size-limit ,size-limit))
					 :on-size-exceed ,on-size-exceed))
	      ,@(iter (for var-name in var-names)
		      (for field-name in field-names)
		      (collect `(setf ,var-name (gethash ,field-name ,var))))
	      ,var))))
