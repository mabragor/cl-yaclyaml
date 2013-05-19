;;;; This file is one of components of CL-YACLYAML system, licenced under GPL, see COPYING for details

(in-package #:cl-yaclyaml)

;;; (macro-) utilities, that should be put into separate file to ensure proper loading/compiling order.

(defmacro! crunch-tag-into-properties (props-var crunch-if-absent crunch-if-vanilla)
  `(if ,props-var
       (let ((it (assoc :tag (cdr ,props-var))))
	 (if it
	     (if (equal (cdr it) :vanilla)
		 (setf (cdr it) ,crunch-if-vanilla))
	     (setf (cdr ,props-var) `((:tag . ,,crunch-if-absent) ,.(cdr ,props-var)))))
       (setf ,props-var `(:properties (:tag . ,,crunch-if-absent)))))
