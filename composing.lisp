(in-package #:cl-yaclyaml)

;;; Composing representation graph

(defparameter anchors (make-hash-table :test #'equal))

(defun property-node-p (node)
  (handler-case (assoc :properties node)
    (error () nil)))

(defun mapping-node-p (node)
  (eql (car node) :mapping))

(defun alias-p (node)
  (and (consp node) (eql (car node) :alias)))


(defun ncompose-representation-graph (representation-tree)
  "Destructively composes representation graph from representation tree."
  (let ((anchors (make-hash-table :test #'equal)))
    (flet ((update-anchors-if-present (node)
	     (let ((anchor (cdr (assoc :anchor (cdr (assoc :properties node))))))
	       (if anchor
		   (setf (gethash anchor anchors) node
			 (cdr (assoc :properties node)) (remove-if (lambda (x)
								     (eql (car x) :anchor))
								   (cdr (assoc :properties node))))))))
      (macrolet! ((glue-alias (place)
			      `(let ((,g!-node (gethash (cdr ,place) anchors)))
				 (if ,g!-node
				     (setf ,place ,g!-node)
				     (error "Alias ~a referenced before anchoring." (cdr ,place)))))
		  (glue-if-alias-rec-otherwise (place)
					       `(if (alias-p ,place)
						    (glue-alias ,place)
						    (rec ,place))))
	(labels ((rec (node)
		   ;; (format t "analyzing: ~a~%" node)
		   (if (consp node)
		       (cond ((property-node-p node) (progn (update-anchors-if-present node)
							    (rec (cdr (assoc :content node)))))
			     ((mapping-node-p node) (iter (for cell in (cdr node))
							  ;; (format t "analyzing cell: ~a~%" cell)
							  (glue-if-alias-rec-otherwise (car cell))
							  (glue-if-alias-rec-otherwise (cdr cell))))
			     (t (iter (for seq-entry on node)
				      (glue-if-alias-rec-otherwise (car seq-entry))))))))
	  (rec representation-tree)
	  representation-tree)))))
				  

      

