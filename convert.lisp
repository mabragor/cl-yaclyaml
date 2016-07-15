;;;; This file is one of components of CL-YACLYAML system, licenced under GPL, see COPYING for details

(in-package #:cl-yaclyaml)
;;; Generating native structures from the nodes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter visited-nodes (make-hash-table :test #'eq))
  (defparameter converted-nodes (make-hash-table :test #'eq))
  (defparameter scalar-converters (make-hash-table :test #'equal))
  (defparameter sequence-converters (make-hash-table :test #'equal))
  (defparameter mapping-converters (make-hash-table :test #'equal))
  (defparameter initialization-callbacks (make-hash-table :test #'eq)))

(defun mark-node-as-visited (node)
  (setf (gethash node visited-nodes) t))

(defun scalar-p (node)
  (atom (cdr (assoc :content node))))

(defun mapping-p (node)
  (equal :mapping (car (cdr (assoc :content node)))))

(defun converted-p (node)
  (gethash node converted-nodes))

(defun visited-p (node)
  (gethash node visited-nodes))

(defun install-scalar-converter (tag converter)
  "Install a scalar converter in the current scheme.
Should be called from an INSTALL-CONVERTERS method.

TAG is a string containing the yaml tag for the content.
CONVERTER is a function which takes a string containing the scalar
input and returns the converted value."
  (setf (gethash tag scalar-converters) converter))
(defun install-sequence-converter (tag converter)
  "Install a sequence converter in the current scheme.
Should be called from an INSTALL-CONVERTERS method.

TAG is a string containing the yaml tag for the content.
CONVERTER is a function which takes a list of raw nodes
as input and returns the converted value.

The converter function should convert the nodes, either with
CONVERT-SEQUENCE-TO-LIST, or CONVERT-NODE on each node."
  (setf (gethash tag sequence-converters) converter))
(defun install-mapping-converter (tag converter)
  "Install a mapping converter in the current scheme.
Should be called from an INSTALL-CONVERTERS method.

TAG is a string containing the yaml tag for the content.
CONVERTER is a function which takes a raw mapping node
as input and returns the converted value.

The content is a list where the CAR is the keyword :MAPPING
and the CDR is an alist of key-value pairs, where both the key and
value are raw nodes.

The converter function should convert the sub-nodes, either with
CONVERT-SEQUENCE-TO-LIST, or CONVERT-NODE on each node."
  (setf (gethash tag mapping-converters) converter))
(defun trivial-scalar-converter (content)
  (copy-seq content))

(defun install-sequence-list-converter (tag converter)
  "Install a sequence converter in the current scheme.
Should be called from an INSTALL-CONVERTERS method.

TAG is a string containing the yaml tag for the content.
CONVERTER is a function that takes a list of converted values
and returns the converted value from the list.

This is probably preferable to INSTALL-SEQUENCE-CONVERTER in user code."
  (setf (gethash tag sequence-converters)
        (lambda (content)
          (funcall converter (convert-sequence-to-list content)))))

(defun install-mapping-hashtable-converter (tag converter)
  "Install a mapping converter in the current scheme.
Should be called from an INSTALL-CONVERTERS method.

TAG is a string containing the yaml tag for the content.
CONVERTER is a function that takes a hash-table of converted values
and returns the converted value from the list.

This is probably preferable to INSTALL-MAPPING-CONVERTER in user code."
  (setf (gethash tag mapping-converters)
        (lambda (content)
          (funcall converter (convert-mapping-to-hashtable content)))))

(defun convert-scalar (content tag)
  (let ((converter (gethash tag scalar-converters)))
    (if converter
	(funcall converter content)
	;; fallback behaviour is to convert nothing
	;; yet, we strip unneeded :PROPERTIES list, leaving only :TAG property.
	`((:content . ,content) (:tag . ,tag)))))

(defun convert-sequence (content tag)
  (let ((converter (gethash tag sequence-converters)))
    (if converter
	(funcall converter content)
	`((:content . ,(convert-sequence-to-list content)) (:tag . ,tag)))))

(defun convert-mapping (content tag)
  (let ((converter (gethash tag mapping-converters)))
    (if converter
	(funcall converter content)
	`((:content . ,(convert-mapping-to-hashtable content)) (:tag . ,tag)))))

(defun convert-node (node)
  "Convert a parsed node into a user object."
  (when (not (nth-value 1 (gethash node converted-nodes)))
      (let* ((content (cdr (assoc :content node)))
             (properties (cdr (assoc :properties node)))
             (tag (cdr (assoc :tag properties))))
        (setf (gethash node converted-nodes)
              (cond ((scalar-p node)
                     (convert-scalar content tag))
                    ((mapping-p node)
                     (convert-mapping content tag))
                    (t (convert-sequence content tag)))))
      (iter (for callback in
                 (gethash node initialization-callbacks))
            (funcall callback))
      (remhash node initialization-callbacks))
  (gethash node converted-nodes))

(defun convert-sequence-to-list (nodes)
  "Convert a raw sequence node to a list of converted values."
  (let (result last-cons)
    (macrolet! ((collect-result (o!-node)
		 `(if result
		      (progn (setf (cdr last-cons) (list ,o!-node))
			     (setf last-cons (cdr last-cons)))
		      (progn (setf result (list ,o!-node))
			     (setf last-cons result)))))
      (iter (for subnode in nodes)
	    (multiple-value-bind (it got) (gethash subnode converted-nodes)
	      (if got
		  (progn (collect-result it))
		  (progn (collect-result nil)
			 ;; LET block here is extremely important, since it results in
			 ;; LAMBDA capturing current values of SUBNODE and LAST-CONS and not the
			 ;; ones they have upon last iteration.
			 (push (let ((encap-subnode subnode)
				     (place last-cons))
				 (lambda ()
				   (setf (car place) (gethash encap-subnode converted-nodes))))
			       (gethash subnode initialization-callbacks))))))
      result)))

(defun convert-mapping-to-hashtable (content)
  "Convert a raw mapping node to a hash-table of converted values."
  (let ((result (make-hash-table :test #'equal)))
    (iter (for (key . val) in (cdr content)) ; CAR of content is :MAPPING keyword
	  (multiple-value-bind (conv-key got-key) (gethash key converted-nodes)
	    (multiple-value-bind (conv-val got-val) (gethash val converted-nodes)
	      ;; LET blocks here are extremely important, since they result in
	      ;; LAMBDAs capturing current values of KEY and VAL and not the
	      ;; ones they have upon last iteration.
	      ;; ENCAP- stands for "encaptured"
	      (if got-key
		  (if got-val
		      (setf (gethash conv-key result) conv-val)
		      (push (let ((encap-key conv-key)
				  (encap-val val))
			      (lambda ()
				(setf (gethash encap-key result)
				      (gethash encap-val converted-nodes))))
			    (gethash val initialization-callbacks)))
		  (if got-val
		      (push (let ((encap-key got-key)
				  (encap-val val))
			      (lambda ()
				(setf (gethash encap-key result)
				      (gethash encap-val converted-nodes))))
			    (gethash key initialization-callbacks))
		      (let (key-installed
			    val-installed
			    (encap-key key)
			    (encap-val val))
			(flet ((frob-key ()
				 (if val-installed
				     (setf (gethash key-installed result) val-installed)
				     (setf key-installed (gethash encap-key converted-nodes))))
			       (frob-val ()
				 (if key-installed
				     (setf (gethash key-installed result) val-installed)
				     (setf val-installed (gethash encap-val converted-nodes)))))
			  (push #'frob-key (gethash key initialization-callbacks))
			  (push #'frob-val (gethash val initialization-callbacks)))))
		  ))))
    result))

(defmacro define-bang-convert (name converter-name)
  `(defun ,name (node)
     (multiple-value-bind (it got) (gethash node converted-nodes)
       (if (not got)
	   (let* ((content (cdr (assoc :content node)))
		  (properties (cdr (assoc :properties node)))
		  (tag (cdr (assoc :tag properties))))
	     (setf (gethash node converted-nodes)
		   (,converter-name content tag))
	     (iter (for callback in
			(gethash node initialization-callbacks))
		   (funcall callback)))
	   it))))
(define-bang-convert convert-scalar! convert-scalar)
(define-bang-convert convert-sequence! convert-sequence)
(define-bang-convert convert-mapping! convert-mapping)

(defun %depth-first-traverse (cur-level &optional (level 0))
  (macrolet ((add-node-if-not-visited (node)
	       `(when (not (visited-p ,node))
		  (mark-node-as-visited ,node)
		  (push ,node next-level))))
    (let (next-level)
      (iter (for node in cur-level)
	    (cond ((scalar-p node) nil)
		  ((mapping-p node) (iter (for (key . val) in (cdr (cdr (assoc :content node))))
					  (add-node-if-not-visited key)
					  (add-node-if-not-visited val)))
		  (t (iter (for subnode in (cdr (assoc :content node)))
			   (add-node-if-not-visited subnode))))
	    (finally (if next-level
			 (%depth-first-traverse next-level (1+ level)))
		     (iter (for node in cur-level)
			   (convert-node node))
		     )))))
