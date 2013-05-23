;;;; This file is one of components of CL-YACLYAML system, licenced under GPL, see COPYING for details

(in-package #:cl-yaclyaml)

;;; Converting representation graph into representation tree.

(defparameter node-aliases (make-hash-table :test #'eq))
(defparameter alias-count 0)
(defparameter reference-hash (make-hash-table :test #'eq))

(defun %serialize (node)
  (or (gethash node node-aliases)
      (cond ((scalar-p node) node)
	    ((mapping-p node) ...)
	    (t ...))))

(defun serialize (representation-graph)
  "Converts representation graph (which may contain loops) to representation tree,
where all shared-ness is represented via alias-nodes."
  (let ((node-aliases (make-hash-table :test #'eq))
	(reference-hash (make-hash-table :test #'eq))
	(alias-count 0))
    (%serialize representation-graph)))

  
