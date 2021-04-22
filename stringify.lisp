(in-package #:cl-yaclyaml)

;;; customization
(defparameter *indent* "  ")
(defparameter *indent-level* 0)
(defparameter *current-indent* "")
(defparameter *document-separator*
  "

---

")
(defparameter *newline*
  #-windows
  (string (code-char 10))
  #+windows
  (coerce (list (code-char 13)
                (code-char 10))
          'string))

(defun calculate-current-indent ()
  (strjoin "" (loop :repeat *indent-level*
                    :collect *indent*)))

;;; special
(defmethod stringify ((yaml (eql :null)))
  "Always return \"null\".
This relies on the parser parsing yaml null as :null instead of nil."
  "null")

(defmethod stringify ((yaml (eql :nan)))
  ".nan")

(defmethod stringify ((yaml (eql :infinity)))
  ".inf")

;;; boolean
(defmethod stringify ((yaml (eql t)))
  "true")

(defmethod stringify ((yaml (eql :false)))
  "false")

;;; number
(defmethod stringify ((yaml integer))
  (princ-to-string yaml))

(defmethod stringify ((yaml float))
  (princ-to-string yaml))

;;; string
(defmethod stringify ((yaml string))
  (if (stringp (handler-case
                   (yaml-simple-load yaml)
                 (t () nil)))
    ;; yaml cannot be misinterpreted as another type:
    ;; quotes not needed
     yaml
    ;; single quotes needed
     (format nil "'~a'"
             ;; escape single quotes
             (regex-replace-all "'" yaml "''"))))

;;; list
(defmethod stringify ((yaml null))
  "[]")

(defun stringify-toplevel (yaml)
  (strjoin
   *document-separator*
   (map 'list 'stringify yaml)))

(defun stringify-document (yaml)
  (strjoin
   (concatenate 'string *newline* *newline*)
   (map 'list 'stringify (rest yaml))))

(defun stringify-list (yaml)
  (strjoin
   (concatenate 'string *newline* *current-indent*)
   (let* ((*indent-level* (+ *indent-level* 1))
          (*current-indent* (calculate-current-indent)))
     (loop :for yaml :in yaml
           :collect
           (format nil "- ~a"
                   (stringify yaml))))))

(defmethod stringify ((yaml cons))
  (cond
    ;; top level
    ((every (lambda (element)
               (and (consp element)
                    (eq :document (first element))))
            yaml)
     (stringify-toplevel yaml))
    ;; document
    ((eq :document (first yaml))
     (stringify-document yaml))
    ;; regular list
    (t (stringify-list yaml))))

;;; hash-table
(defmethod stringify ((yaml hash-table))
  (if (= 0 (hash-table-count yaml))
      "{}"
      (strjoin
       (concatenate 'string *newline* *current-indent*)
       (let* ((*indent-level* (+ *indent-level* 1))
              (*current-indent* (calculate-current-indent)))
         (loop :for key :being :the :hash-keys :of yaml
                 :using (:hash-value value)
               :if (typep value '(or cons hash-table))
                 :collect (format nil "~a:~a~a~a"
                                  key *newline*
                                  *current-indent*
                                  (stringify value))
               :else
                 :collect (format nil "~a: ~a"
                                  key (stringify value)))))))

;;; dump to file
(defun dump-file (yaml path &key if-exists (if-does-not-exist :create))
  (with-open-file (out path
                       :direction :output
                       :if-exists if-exists
                       :if-does-not-exist if-does-not-exist)
    (write-sequence (stringify yaml) out)))
