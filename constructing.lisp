(in-package #:cl-yaclyaml)

;;; Generating native structures from the nodes
 
;; (defun find-parents (representation-graph)
;;   (let ((parents (make-hash-table :test #'eq))
;; 	(visited (make-hash-table :test #'eq)))
;;     (declare (special parents))
;;     (labels ((rec (node)
;; 	       (format t "processing node: ~a~%" node)
;; 	       (if (not (property-node-p node))
;; 		   (error "All nodes at find parents stage ~a"
;; 			  "should be :PROPERTIES-:CONTENT-like.")
;; 		   (let ((content (cdr (assoc :content node))))
;; 		     (macrolet ((push-n-descend (parent child)
;; 				  `(progn (push ,parent (gethash ,child parents))
;; 					  (when (not (gethash ,child visited))
;; 					    (setf (gethash ,child visited) t)
;; 					    (rec ,child)))))
;; 		       (cond ((atom content) nil)
;; 			     ((mapping-node-p content)
;; 			      (iter (for (key . val) in (cdr content))
;; 				    (push-n-descend node key)
;; 				    (push-n-descend node val)))
;; 			     (t (iter (for subnode in content)
;; 				      (push-n-descend node subnode)))))))))
;;       (rec representation-graph)
;;       parents)))

;; (defun spawn-empty-str ()
;;   "")
;; (defun spawn-empty-seq () (list))
;; (defun spawn-empty-map (kwd)
;;   (case kwd
;;     (:small (list))
;;     (:large (make-hash-table :test #'equal))))

;; (defparameter tag-fresh-replacements (make-hash-table :test #'equal))
;; (defparameter node-replacements (make-hash-table :test #'eq))
;; (defparameter visited-nodes (make-hash-table :test #'eq))

;; (defmacro with-failsafe-tag-schema (&body body)
;;   `(let ((tag-fresh-replacements (make-hash-table :test #'equal)))
;;      (declare (special tag-fresh-replacements))
;;      (setf (gethash "tag:yaml.org,2002:str" tag-fresh-replacements)
;; 	   (lambda (node)
;; 	     "String is simply a string. Generated immediately"
;; 	     (let ((res (copy-seq (cdr (assoc :content node)))))
;; 	       (setf (gethash node node-replacements) res)
;; 	       res))
;; 	   (gethash "tag:yaml.org,2002:seq" tag-fresh-replacements)
;; 	   (lambda (node)
;; 	     "Sequence is link to cons list. Populated gradually, initially empty. At the final step, we'll
;; 'evaporate' additional linking layer."
;; 	     (let ((res (cons nil nil)))
;; 	       (setf (gethash node node-replacements) res)
;; 	       res))
;; 	   (gethash "tag:yaml.org,2002:map" tag-fresh-replacements)
;; 	   (lambda (node)
;; 	     "Mapping is for now a hash-table. I dunno how to populate it for now."
;; 	     (let ((res (make-hash-table :test #'equal)))
;; 	       (setf (gethash node node-replacements) res)
;; 	       res))
;; 	   (gethash t tag-fresh-replacements)
;; 	   (lambda (node)
;; 	     "Unknown nodes are not replaced."
;; 	     node))
;;      ,@body))
	    
;; (defun node-replacement (node)
;;   (gethash (cdr (assoc :tag (cdr (assoc :properties node)))) tag-fresh-replacements))

;; (defmacro visited-p (node)
;;   `(gethash ,node visited-nodes))

;; (defun merge-new-node (node new-node)
;;   (let ((tag (cdr (assoc :tag (cdr (assoc :properties node)))))
;; 	(content (cdr (assoc :content node))))
;;     (cond ((equal tag "tag:yaml.org,2002:str") nil) ; for string nothing should be done
;; 	  ((equal tag "tag:yaml.org,2002:seq") ...) ; something clearly could be done
;; 	  ((equal tag "tag:yaml.org,2002:map")
;; 	   (error "Maps are so far not implemented."))
;; 	  (t (cond ((typep content 'string) nil) ; strings do not require merging
;; 		   ((consp content) (if (mapping-node-p content)
;; 					(error "Mapping nodes are not impliemented yet.")
;; 					(iter (for subnode on content)
;; 					      (let ((it (gethash (car subnode) node-replacements)))
;; 						(if it
;; 						    (setf (car subnode) it)
;; 						    (setf it (funcall (node-replacement subnode) (car subnode))))
;; 						(when (not (visited-p it))
;; 						  (merge-new-node (car subnode) it))))))
;; 		   (t (error "Strange content ~a with tag ~a" content tag)))))
;;     (setf (visited-p new-node) t)))
	  
	   
  

;; (defun nativicate (representation-graph)
;;   "Reincarnate some of the nodes as native language structures."
;;   (let ((parents (find-parents representation-graph)))
;;     (labels ((rec (node)
	       
;;     (rec representation-graph)))


;; I need to do the depth-first traversing of the graph

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter visited-nodes `(,(make-hash-table :test #'eq)))
  (defparameter converted-nodes (make-hash-table :test #'eq))
  (defparameter scalar-converters (make-hash-table :test #'equal))
  (defparameter initialization-callbacks (make-hash-table :test #'eq)))

(defmacro with-fresh-visited-level (&body body)
  `(let ((visited-nodes `(,(make-hash-table :test #'eq) ,visited-nodes)))
     ,@body))

(defmacro mark-node-as-visited (node)
  (setf (gethash node (car visited-nodes)) t))

(defun scalar-p (node)
  (atom (cdr (assoc :content node))))

(defun mapping-p (node)
  (equal :mapping (car (cdr (assoc :content node)))))

(defun converted-p (node)
  (gethash node converted-nodes))

(defun visited-p (node)
  (iter (for level in visited-nodes)
	(if (gethash node level)
	    (return t))
	(finally (return nil))))

(defun install-scalar-converter (tag converter)
  (setf (gethash tag scalar-converters) converter))

(defun trivial-scalar-converter (content)
  (copy-seq content))

(defun convert-scalar (content tag)
  (let ((converter (gethash tag scalar-converters)))
    (if converter
	(funcall converter content)
	;; fallback behaviour is to convert nothing
	;; yet, we strip unneeded :PROPERTIES list, leaving only :TAG property.
	`((:content . ,content) (:tag . ,tag)))))

(defmacro with-failsafe-schema (&body body)
  `(let ((scalar-converters (make-hash-table :test #'equal)))
     (install-scalar-converter "tag:yaml.org,2002:str" #'trivial-scalar-converter)
     ,@body))

(defmacro with-json-schema (&body body)
  `(with-failsafe-schema
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
     (install-scalar-converter :non-specific
			       (lambda (content)
				 (cond ((or (equal content :empty)
					    (all-matches "^null$" content))
					(convert-scalar content "tag:yaml.org,2002:null"))
				       ((all-matches "^(true|false)$" content) (convert-scalar content
											       "tag:yaml.org,2002:bool"))
				       ((all-matches "^-?(0|[1-9][0-9]*)$" content) (convert-scalar content
											       "tag:yaml.org,2002:int"))
				       ((all-matches "^-?(0|[1-9][0-9]*)(\\.[0-9]*)?([eE][-+]?[0-9]+)?$" content)
					(convert-scalar content "tag:yaml.org,2002:float"))
				       (t (error "Could not resolve scalar: ~a" content)))))
     ,@body))


(defmacro with-core-schema (&body body)
  `(with-json-schema
     (install-scalar-converter :non-specific
			       (lambda (content)
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
				       (t (convert-scalar content "tag:yaml.org,2002:str")))))
     ,@body))

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
			 (depth-first-traverse next-level (1+ level)))
		     (iter (for node in cur-level)
			   (when (not (gethash node converted-nodes))
			     (let* ((content (cdr (assoc :content node)))
				    (properties (cdr (assoc :properties node)))
				    (tag (cdr (assoc :tag properties))))
			       (setf (gethash node converted-nodes)
				     (cond ((scalar-p node)
					    (convert-scalar content tag))
					   ((mapping-p node)
					    (error "Mapping types are not implemented so far."))
					   (t ...))))
			     (iter (for callback in
					(gethash node initialization-callbacks))
				   (funcall callback))))
		     )))))
		   

(defun construct (representation-graph &key (schema :core))
  (let ((visited-nodes `(,(make-hash-table :test #'eq)))
	(converted-nodes (make-hash-table :test #'eq))
	(initialization-callbacks (make-hash-table :test #'eq)))
    (case schema
      (:failsafe (with-failsafe-schema
		   (%depth-first-traverse `(,representation-graph))))
      (:json (with-json-schema
	       (%depth-first-traverse `(,representation-graph))))
      (:core (with-core-schema
	       (%depth-first-traverse `(,representation-graph)))))
    (gethash representation-graph converted-nodes)))
  
    
    
