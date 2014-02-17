;;;; This file is one of components of CL-YACLYAML system, licenced under GPL, see COPYING for details

(in-package #:cl-yaclyaml)

;;; Converting representation graph into representation tree.

(defparameter node-aliases (make-hash-table :test #'eq))
(defparameter alias-count 0)
(defparameter encountered-nodes (make-hash-table :test #'eq))

(defun %nserialize (node)
  ;; (format t "encountered nodes: ~a~%" (hash->assoc encountered-nodes))
  ;; (format t "node: ~a~%" node)
  ;; (format t "~a~%" (multiple-value-list (gethash node encountered-nodes)))
  (a:aif (gethash node node-aliases)
	 (values it t)
	 (if (gethash node encountered-nodes)
	     ;; need to create an alias node
	     (progn ;; (format t "Encountered node for at least second time.~%")
		    (let ((alias-name (format nil "a~a" (incf alias-count))))
		      ;; KLUDGE: here we assume that :PROPERTIES is the first assoc-element
		      ;; and content is the second
		      (setf (car node) `(:properties (:anchor . ,alias-name) ,.(cdar node)))
		      (values (setf (gethash node node-aliases) `(:alias . ,alias-name))
			      t)))
	     (progn ;; (format t "Encountered node for the first time. ")
		    (setf (gethash node encountered-nodes) t)
		    (cond ((scalar-p node)
			   ;; (format t "scalar~%")
			   nil)
			  ((mapping-p node)
			   ;; (format t "mapping~%")
			   (iter (for map-entry in (cddr (assoc :content node)))
				 (a:acond-got ((%nserialize (car map-entry)) (setf (car map-entry) it)))
				 (a:acond-got ((%nserialize (cdr map-entry)) (setf (cdr map-entry) it)))))
			  ;; sequence
			  (t
			   ;; (format t "sequence~%")
			   (iter (for subnode on (cdr (assoc :content node)))
				 (a:acond-got ((%nserialize (car subnode)) (setf (car subnode) it))))))
		    (values node nil)))))

(defun nserialize (representation-graph)
  "Destructively convert representation graph (which may contain loops) to representation tree,
where all shared-ness is represented via alias-nodes."
  (let ((node-aliases (make-hash-table :test #'eq))
	(encountered-nodes (make-hash-table :test #'eq))
	(alias-count 0))
    (%nserialize representation-graph)))

  
