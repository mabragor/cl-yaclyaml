;;;; cl-yaclyaml.lisp

(in-package #:cl-yaclyaml)

;;; "cl-yaclyaml" goes here. Hacks and glory await!

(defun str-* (n str)
  (iter (for i from 0 below n)
        (collect str into res)
        (finally (return (apply #'strcat res)))))

(defmacro fart (&body args)
  `(format t ,(strcat (str-* (- (length args) 1) "~a ") "~a~%") ,@args))

(define-condition simple-reader-error (simple-condition error)
  ()
  (:documentation "Condition typically signalled from YaML reader."))

(defmacro signal-reader-error (format-control &rest format-arguments)
  "Like CL-INTERPOL's signal-reader-error, but signals
SIMPLE-READER-ERROR for stream STREAM instead of *STREAM*."
  `(error 'simple-reader-error
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))

;;; For now we will stick to UTF-8 (basic streams)
;;; and will use lispbuilder lexing/YaCC-ing facilities extensively.
;;; With other encodings, idea would be to use Babel system.
;;; But one must figure out, howto tweak it to recognize encodings based
;;; on first byte and not on explicit BOM, which, I guess, is tricky.


;; ;;; Lexer for basic YaML context.
;; (deflexer base-context-yaml-lexer
;;   ;; Block structure indicators
;;   ("-" (return (values 'c-sequence-entry nil)))
;;   ("?" (return (values 'c-mapping-key nil)))
;;   (":" (return (values 'c-mapping-value nil)))
;;   ;; Flow collection indicators
;;   ("," (return (values 'c-collect-entry nil)))
;;   ("\\[" (return (values 'c-sequence-start nil)))
;;   ("\\]" (return (values 'c-sequence-end nil)))
;;   ("\\{" (return (values 'c-mapping-start nil)))
;;   ("\\}" (return (values 'c-mapping-end nil)))
;;   ;; Comment indicator
;;   ("#" (return (values 'c-comment nil)))
;;   ;; Node Property Indicators
;;   ("&" (return (values 'c-anchor nil)))
;;   ("*" (return (values 'c-alias nil)))
;;   ("!" (return (values 'c-tag nil)))
;;   ;; Block scalar indicators
;;   ("|" (return (values 'c-literal)))
;;   (">" (return (values 'c-folded)))
;;   ;; Quoted scalar indicators
;;   ("'" (return (values 'c-single-quote nil)))
;;   ("\"" (return (values 'c-double-quote nil)))
;;   ;; Directive Indicator
;;   ("%" (return (values 'c-directive-line nil)))
;;   ;; Reserved indicators
;;   ("@|`" (return (values 'c-reserved nil)))

;;   ;; Line break characters
;;   ;; Whether or not they can be specified as that, remain an open question.
;;   ((string #\linefeed) (return (values 'b-line-feed nil)))
;;   ((string #\return) (return (values 'b-carriage-return nil)))

;;   ;; Whitespace characters
;;   ((string #\space) (return (values 's-space nil)))
;;   ((string #\tab) (return (values 's-tab nil)))

;;   ("0-9" (return (values ns-dec-digit %0)))
;;   ("[A-Fa-f]" (return (values ns-hex-letter %0)))
;;   ("[A-Za-z]" (return (values ns-ascii-letter %0)))
;;   )

;; (define-parser base-context-yaml-parser
;;     <....>
;;   (c-indicator c-sequence-entry
;;                c-mapping-key
;;                c-mapping-value
;;                c-collect-entry
;;                c-sequence-start
;;                c-sequence-end
;;                c-mapping-start
;;                c-mapping-end
;;                c-comment
;;                c-anchor
;;                c-alias
;;                c-tag
;;                c-literal
;;                c-folded
;;                c-single-quote
;;                c-double-quote
;;                c-directive-line
;;                c-reserved)
;;   (с-flow-indicator c-collect-entry
;;                     c-sequence-start
;;                     c-sequence-end
;;                     c-mapping-start
;;                     c-mapping-end)

;;   (b-char b-line-feed
;;           b-carriage-return)

;;   ;; Something should be done with this rule
;;   (nb-char c-printable
;;            - b-char
;;            - c-byte-order-mark)
  
;;   (b-break (b-carriage-return b-line-feed)
;;            b-carriage-return
;;            b-line-feed)

;;   ;; Discarding of different line break characters in favor of newline.
;;   (b-as-line-feed b-break)

;;   ;; Whilespace
;;   (s-white s-space s-tab)

;;   ;; non-(white)space characters
;;   (ns-char nb-char - s-white)

;;   ;; Misc char classes
;;   (ns-hex-digit ns-dec-digit
;;                 ns-hex-letter)
;;   (ns-word-char ns-dec-digit
;;                 ns-ascii-letter
;;                 c-sequence-entry) ; KLUDGE, hints that some other language
;;                                         ;should be used.

;;   (ns-uri-char ns-hex-digit
;;                ns-word-char ...) ; something should be done here also

;; ;; Escaping scheme should be modified also
;; ;; standard CLone is of no use.


;; Leading whitespace on very first line
;; and trailing whitespace on very last line
;; is treated specially

(defmacro apif (test-p form then &optional else)
  `(let ((it ,form))
     (if (funcall ,test-p it)
         ,then
         ,@(if else `(,else)))))

(defmacro-driver (for var in-non-closing-stream stream
                      &optional using (reader #'read))
  (sb-int:with-unique-names (eof)
    "Like in-stream, but does not close the stream upon finish of iteration."
    `(,(if generate 'generate 'for) ,var
       next (let ((it (funcall ,reader ,stream nil ',eof t)))
              (if (eql it ',eof)
                  (progn (setf ,var it)
                         (terminate))
                  it)))))

(defmacro def-yaml-reader (name args &body body)
  `(defun ,name ,args
     (macrolet ((b-break-p (char &optional (char1 char))
                  ;; Creepy code to ensure that all acceptable b-break char
                  ;; sequences are read as #\newline character.
                  `(or (char= ,char #\newline)
                       (and (char= ,char #\return)
                            (if (char= (peek-char nil stream nil t t) #\newline)
                                       (next ,char1)
                                       (setf ,char1 #\newline))))))
       (flet ((whitespace-p (char) (or (char= char #\space)
                                       (char= char #\tab))))
         (macrolet
             ((iter-main-loop (&body body)
                (sb-int:with-unique-names (g!-next-char-read-p)
                  `(macrolet ((descend-with-reader (name &body body)
                                (sb-int:with-unique-names (g!-next-char)
                                  `(multiple-value-bind (value ,g!-next-char)
                                       (,name stream char)
                                     (if ,g!-next-char
                                         (setf ,',g!-next-char-read-p t
                                               char ,g!-next-char))
                                     ,@body))))
                     (iter main-loop
                           (with ,g!-next-char-read-p = nil)
                           (generate char in-non-closing-stream
                                     stream using #'read-char)
                           (if (not ,g!-next-char-read-p)
                               (next char)
                               (setf ,g!-next-char-read-p nil))
                           ;;(format t "~`a~%" char)
                           ,@body))))
	      (yaml-return (value next-char)
		`(return-from main-loop (values ,value ,next-char)))
              (iml-collect (&rest args)
                `(in main-loop (collect ,@args))))
           ,@body)))))

(defmacro def-yaml-scalar-reader (name args &body body)
  `(def-yaml-reader ,name ,args
     ;; (declare (special anchor tag))
     (macrolet ((use-whitespace (&optional (res 'res))
                  `(progn (nconcing (nreverse whitespace) into ,res)
                          (setf whitespace nil)))
                (discard-whitespace ()
                  `(setf whitespace nil))
                (fold-newlines (&optional (res 'res))
                  `(progn
                     (cond ((equal new-line-count 0) nil)
                           ((equal new-line-count 1)
                            (iml-collect #\space into ,res))
                           (t (iml-collect (make-string (- new-line-count 1)
                                                    :initial-element #\newline)
                                into ,res)))
                     (setf new-line-count 0))))
       (macrolet ((process-nws-char (&key (char 'char) (res 'res)
                                          discard-leading-whitespace)
                    `(if begin-of-first-line-p
                         (progn ,(if discard-leading-whitespace
                                     `(discard-whitespace)
                                     `(use-whitespace ,res))
                                (iml-collect ,char into ,res)
                                (setf begin-of-first-line-p nil))
                         (if begin-of-line-p
                             (progn (discard-whitespace)
                                    (fold-newlines ,res)
                                    (iml-collect ,char into ,res)
                                    (setf begin-of-line-p nil))
                             (progn (use-whitespace ,res)
                                    (iml-collect ,char into ,res)))))
                  (process-b-break-char ()
                    `(if (find yaml-context '(flow-key block-key))
                         (signal-reader-error
                          "Implicit key should be single lined.")
                         (progn (discard-whitespace)
                                (incf new-line-count)
                                (setf begin-of-first-line-p nil
                                      begin-of-line-p t)))))
         (macrolet ((return-scalar (&key discard-trailing-whitespace
                                         (next-char-read-p t))
                      `(return-from main-loop
                         (values `(scalar
                                   ,@(if (and (boundp 'anchor) anchor)
                                         `(:anchor ,anchor))
                                   ,@(if (and (boundp 'tag) tag)
                                         `(:tag ,tag) '(:tag !))
                                   ,(apply #'strcat
                                           ,(if discard-trailing-whitespace
                                                'res
                                                `(if begin-of-line-p
                                                     res
                                                     (nconc res
                                                            whitespace)))))
                                 ,(if next-char-read-p
                                      `(if (characterp char) char nil))))))
           (let (whitespace
                 (begin-of-first-line-p t)
                 (begin-of-line-p t)
                 (new-line-count 0))
             ,@body))))))

;; First we define assoc-list of escapes ...
(let ((escapes
       `((#\0 . #\nul)
         (#\a . #\bel)
         (#\b . #\backspace)
         (#\t . #\tab)
         (#\tab . #\tab)
         (#\n . #\newline)
         (#\v . #\vt)
         (#\f . #\page)
         (#\r . #\return)
         (#\e . #\esc)
         (,(code-char 32) . ,(code-char 32))
         (#\" . #\")
         (#\/ . #\/)
         (#\\ . #\\)
         (#\N . #\next-line)
         (#\_ . #\no-break_space)
         (#\L . #\line_separator)
         (#\P . #\paragraph_separator))))
         ;;(#\x . (lambda (stream) ...))
         ;;(#\u . (lambda (stream) ...))
         ;;(#\U . (lambda (stream) ...)))))
  (defun get-escape-of (char)
    (assoc char escapes))
  ;; ... and then function, which reads double quoted scalars
  (def-yaml-scalar-reader double-quote-reader (stream double-quote-char)
    (iter-main-loop
     (until (char= char double-quote-char))
     (cond
       ((whitespace-p char) (push char whitespace))
       ((char= char #\\)
        (if (char= (next char) #\newline)
            (progn (use-whitespace)
                   (setf begin-of-first-line-p nil
                         begin-of-line-p t))
            (let ((esc (assoc char escapes :test #'char=)))
              (if (not esc)
                  (signal-reader-error "Invalid escape character: ~s ~s"
                         char
                         (char-code char))
                  (let ((char (apif #'characterp (cdr esc)
                                    it
                                    (funcall it stream))))
                    (process-nws-char))))))
       ((b-break-p char) (process-b-break-char))
       (t (process-nws-char)))
     (finally (if (characterp char)
                  (return-scalar :next-char-read-p nil)
                  (signal-reader-error
                   "Double quote scalar did not end with \""))))))

        
(def-yaml-scalar-reader single-quote-reader (stream single-quote-char)
  (iter-main-loop
   (cond
     ((char= char single-quote-char)
      (let ((next-char (peek-char nil stream nil nil t)))
        (cond
          ((not next-char) (setf char next-char) (finish))
          ((char= next-char single-quote-char)
           (iml-collect (next char) into res))
          (t (setf char next-char) (finish)))))
     ((whitespace-p char) (push char whitespace))
     ((b-break-p char) (process-b-break-char))
     (t (process-nws-char)))
   (finally
    (cond ((or (not char) (characterp char)) (return-scalar))
          (t (signal-reader-error "Single quote scalar did not end with '"))))))

;;; Read token-like things
;; (defmacro def-token-reader (name args terminators)
;;   `(let ((terminators ',terminators))
;;      (defun ,name ,args
;;        (iter (generate char in-stream stream using #'read-char)
;;         (next char)
;;         (until (find char terminators :test #'char=))
;;         (collect char into res)
;;         (finally (return (values (apply #'strcat res)
;;                                  (if (characterp char) char nil))))))))


;(s-white #\space #\tab)
;(b-char #\newline #\return)
;(c-flow-indicator #\[ #\] #\{ #\} #\,)

;; Is triggered by #\& character after whitespace
;(def-token-reader yaml-anchor-reader (stream)
;  (s-white b-char c-flow-indicator))

;; Is triggered by #\* character after whitespace
;(def-token-reader yaml-alias-node-reader (stream)
;  (s-white b-char c-flow-indicator))

;;; Building block for reading directives
;(def-token-reader yaml-nws#-token-reader (stream)
;  (s-white b-char #\#))

;;; Questions to answer:
;;; 1) what happens, when no characters are present in the stream?
;;; (should return (nil nil)
;;; 2) should add passing of invokation character (or, in general, string),

;;; And I really want to sleep, it seems

;(defun yaml-directive-reader (stream)
;  (iter 

;; Can hold values 'flow-in 'flow-out 'flow-key 'block-key
(defparameter yaml-context 'flow-out)

(def-yaml-scalar-reader read-comment (stream prev-char)
  ;; (fart "Comment:" char)
  (iter-main-loop
   (if (b-break-p char) (finish))
   (finally (return-from main-loop (values nil
                                           (if (characterp char) char nil))))))

(def-yaml-scalar-reader plain-scalar-reader (stream already-read-chars)
  ;; (fart char)
  ;; First we need to do iteration among already-read-chars
  (iter-main-loop
   (if-first-time
    ;; Whitespace characters and newlines cannot trigger
    ;; plain scalar filling, so ALREADY-READ-CHARS are necessary NWS-CHARS
    ;; So the folliwing hopefully words correctly.
    (dolist (char already-read-chars)
      (process-nws-char)))
   (cond
     ((whitespace-p char) (push char whitespace))
     ((b-break-p char) (process-b-break-char))
     ((char= char #\:) (if (whitespace-p (peek-char nil stream t t))
                           (finish)
                           (progn (process-nws-char)
                                  (next char)
                                  (process-nws-char))))
     ((char= char #\-) (if (and (or whitespace begin-of-line-p)
                                (whitespace-p (peek-char nil stream t t)))
                           (finish)
                           (progn (process-nws-char)
                                  (next char)
                                  (process-nws-char))))
     ((char= char #\#) (if (or whitespace begin-of-line-p)
                            ;; Maybe we should retain comments in
                            ;; serialization tree?
                            (multiple-value-bind (str char1)
                                (read-comment stream #\#)
                              (declare (ignore str))
                              ;; Comment should end only with
                              ;; line-break or EOF
                              (if (and char1
                                       (b-break-p char1 char))
                                  (process-b-break-char)))
                            (iml-collect char into res)))
     ((find char '(#\, #\[ #\] #\{ #\}) :test #'char=)
      (if (find yaml-context '(flow-in flow-key))
          (finish)
          (process-nws-char)))
     (t (process-nws-char :discard-leading-whitespace t)))
   (finally (let ((tag (or (and (boundp 'tag) tag) '?)))
              (declare (special tag))
              (return-scalar :discard-trailing-whitespace t)))))

(defparameter c-flow-indicators '(#\{ #\} #\[ #\] #\,))

(def-yaml-reader read-anchor (stream prev-char)
  "Reader both for anchors and aliases."
  (iter-main-loop
   (cond
     ((or (whitespace-p char) (b-break-p char)
          (find char c-flow-indicators :test #'char=)) (finish))
     (t (collect char into res)))
   (finally (return-from main-loop
              (values `(,(cond ((char= prev-char #\&) 'anchor)
                               ((char= prev-char #\*) 'alias))
                         ,(apply #'strcat res))
                      (if (characterp char) char nil))))))

(defparameter uri-char-class
  (strcat "(?:[a-zA-Z0-9-;/?:@&=+$,_.!~*'()]" "|" "%[0-9a-fA-F]{2})"))

(defun valid-uri-p (str)
  "Returns true if string is valid URI according to definition in RFC2396"
  (cl-ppcre:all-matches (strcat "^" "[a-zA-Z][a-zA-Z0-9-+.]*" ":"
                                uri-char-class "*" "$")
                        str))
(defmacro evalid-uri-p (str)
  "Version of VALID-URI-P, that signals error on predicate failure."
  (sb-int:with-unique-names (g!-str)
    `(let ((,g!-str ,str))
       (or (valid-uri-p ,g!-str)
           (signal-reader-error
            "Verbatim global tag ~s is not a valid URI" ,g!-str)))))

(defun local-tag-p (str)
  "T if str is potentially a local tag (begins with !)"
  (cl-ppcre:all-matches "^!" str))
(defun valid-local-tag-p (str)
  "T if str looks like valid local YaML tag.
Rather sloppy (first version)."
  (cl-ppcre:all-matches (strcat "^!" uri-char-class "+$") str))
(defmacro evalid-local-tag-p (str)
  (sb-int:with-unique-names (g!-str)
    `(let ((,g!-str ,str))
       (or (valid-local-tag-p ,g!-str)
           (signal-reader-error "Verbatim local tag ~s is not valid.
Rather does not begin with #\! or contains no characters after it." ,g!-str)))))

(def-yaml-reader read-verbatim-tag (stream prev-char)
  (iter-main-loop
   (cond
     ((char= char #\>) (finish))
     (t (collect char into res)))
   (finally (if (characterp char)
                (let ((str (apply #'strcat res)))
                  (if (or (and (local-tag-p str)
                               (evalid-local-tag-p str))
                          (evalid-uri-p str))
                      (return-from main-loop (values `(tag ,str)
                                                     nil))))

                (signal-reader-error
                 "Verbatim tag did not terminate with #\>")))))

(def-yaml-reader read-obscure-tag (stream prev-char)
  (iter-main-loop
   (cond
     ((char= char #\!) (if res
                           (let ((it (strcat #\! res #\!)))
                             (if (valid-tag-handle-p it)
                                 (return-from main-loop (values it char))
                                 (signal-reader-error
                                  "Invalid tag handle: ~a" it)))
                           (return-from main-loop (values "!!" #\!))))
     ((or (whitespace-p char) (b-break-p char)) (finish))
     (t (collect char into res)))
   (finally (return-from main-loop
              (values (strcat res)
                      (if (characterp char) char nil))))))

;; Thus ! tag is left unresolved
;; !asdf tags are resolved 
                
;; (defun resolve-tag (handle tag)
;;   (declare (special tag-handles))
;;   (if (and (equal handle "!") (not tag))
;;       "!"
;;       (let ((it (assoc handle tag-handles)))
;;         (if it
;;             (if (equal handle (cdr it))
;;                 (strcat (cdr it) tag)
;;                 (cl-ppcre:register-groups-bind (exclams rest)
;;                     ("^(!*)(.*)$" (cdr it))
;;                   (resolve-tag exclams (strcat rest tag))))
;;             (signal-reader-error "Unknown tag handle: ~a" handle)))))


;; (def-yaml-reader read-tag (stream prev-char)
;;   (iter-main-loop
;;    (if (char= char #\<)
;;        (descend-with-reader read-verbatim-tag
;;          (return-from main-loop (values value char)))
;;        (descend-with-reader read-obscure-tag
;;          (cond
;;            ((char= char #\!)
;;             (let ((handle value))
;;               (descend-with-reader read-obscure-tag
;;                 (cond
;;                   ((char= char #\!) (signal-reader-error "Unexpected exclam!"))
;;                   (t (resolve-tag handle value) char)))))
;;            (t (return-from main-loop
;;                 (values (resolve-tag "!" value)
;;                         char))))))))
   
   

;; We need to
;; 1) distinguish special cases of tags - !! and !, and not try
;; to resolve them immediately
;; 2) 

   
;; alias || (tag? & anchor?)

;; (defparameter brackets
;;   `((#\( . #\))
;;     (#\[ . #\])
;;     (#\{ . #\})
;;     (#\< . #\>)))
 
;; (def-yaml-reader flow-node-props-reader (stream prev-char)
;;   (let (res)
;;     (iter-main-loop
;;      (cond
;;        ((or (whitespace-p char) (b-break-p char)) nil)
;;        ((char= char #\&) (if (assoc 'anchor res)
;; 			     (signal-reader-error "There should be only one anchor per node.")
;; 			     (descend-with-reader read-anchor
;; 			       (setf (cdr (assoc 'anchor res)) value))))
;;        ((char= char #\!) (if (assoc 'tag res)
;; 			     (signal-reader-error "There should be only one tag per node.")
;; 			     (descend-with-reader read-tag
;; 			       (setf (cdr (assoc 'tag res)) value))))
;;        ((char= char #\*) (if (assoc 'alias res)
;; 			     (signal-reader-error "There should be only one alias per node.")
;; 			     (descend-with-reader read-anchor
;; 			       (setf (cdr (assoc 'alias res)) value))))
;;        (t (finish)))
;;      (finally (if (assoc 'alias res)
;; 		  (if (or (assoc 'tag res) (assoc 'anchor res))
;; 		      (signal-reader-error "Tag and anchor cant be specified for alias node.")
;; 		      (if (not (or (char= char #\,) (char= char (cdr (assoc prev-char brackets :test #'char=)))))
;; 			  (signal-reader-error "Alias node cant introduce new content.")
;; 			  (return-from main-loop (values (cdr (assoc 'alias res)) char))))
;; 		  (let ((anchor (cdr (assoc 'anchor res)))
;; 			(tag (cdr (assoc 'tag res))))
;; 		    (declare (special tag anchor))
;; 		    (descend-with-reader flow-node-content-reader
;; 		      (return-from main-loop (values value char)))))))))

;; (def-yaml-reader flow-node-content-reader (stream prev-char)
;;   (if prev-char
;;       (unread-char prev-char stream))
;;   (iter-main-loop
;;    (cond
;;      ((or (whitespace-p char) (b-break-p char)) nil)
;;      ((char= char #\") (descend-with-reader double-quote-reader
;; 			 (let ((real-value value))
;; 			   (descend-with-reader flow-node-whitespace-reader
;; 			     (return-from main-loop
;; 			       (values real-value char))))))
;;      ((char= char #\') (descend-with-reader single-quote-reader
;; 			 (let ((real-value value))
;; 			   (descend-with-reader flow-node-whitespace-reader
;; 			     (return-from main-loop
;; 			       (values real-value char)))))))))
					    
;; (def-yaml-reader flow-node-whitespace-reader (stream prev-char)
;;   "Flow node reader, that expects nothing but whitespace or b-breaks."
;;   (iter-main-loop
;;    (cond
;;      ((or (whitespace-p char) (b-break-p char)) nil)
;;      ((or (char= char #\,) (char= char (cdr (assoc prev-char brackets :test #'char=))))
;;       (return-from main-loop (values nil char)))
;;      ;; We should also read comments here correctly!
;;      (t (signal-reader-error "Non whitespace character ~c found where only whitespace expected." char)))))

;; (def-yaml-reader flow-sequence-reader (stream prev-char)
;;   (let ((yaml-context (case yaml-context
;;                         ((flow-out flow-in) 'flow-in)
;;                         ((block-key flow-key) 'flow-key))))
;;     (declare (special yaml-context))
;;       (iter-main-loop
;;           (with elt-nooked-p = nil)

;;        (cond
;;          ((char= char #\]) (finish))
;;          ((char= char #\*) (collect-with-reader yaml-read-alias))
;;          ((char= char #\&) (collect-with-reader yaml-read-anchor))

;;          ((char= char #\[) (collect-with-reader yaml-flow-sequence-reader))
;;          ((char= char #\{) (collect-with-reader yaml-flow-mapping-reader))
;;          ((char= char #\,) (if (not elt-nooked-p)
;;                                (warn "No sequence element between commas.")
;;                                (setf elt-nooked-p nil)))
;;          ((char= char #\?) (collect-with-reader yaml-explicit-key-value-reader))
;;          ;; Sometimes ? sign is omitted. Then we must apply a bit of magic
;;          ((char= char #\:) (if elt-nooked-p
;;                                (let* ((last-elt (uncollect res)))
;;                                  (collect-with-reader yaml-flow-value-reader
;;                                    (setf value
;;                                          `(flow-mapping ,last-elt
;;                                                         ,value))))
;;                                (collect-with-reader yaml-flow-value-reader
;;                                  (setf value
;;                                        `(flow-mapping (scalar ""
;;                                                               :tag "!!null")
;;                                                       ,value)))))
;;          ;; whitespace and line breaks are ignored

;;          (t (collect-with-reader plain-scalar-reader)))
;;                    (if (char= char #\:)
;;                        (let* ((end-of-res (last res 2))
;;                               (impl-key
;;        (finally
;;         (if (and char (not (characterp char)))
;;             (signal-reader-error "Flow sequence not terminated by ].")
;;             (return-from main-loop `(flow-sequence ,@res))))))))


;; ;; (collect-with-reader yaml-flow-mapping-reader))

;; (defun yaml-flow-node-reader (stream finish-char)


;; (defun test-uncollect (str)
;;   (iter (for char in-string str)
;;         (cond ((char= char #\a) (setf res (butlast res)))
;;               (t (collect char into res)))
;;         (finally (return res))))

;;; So far so good.
;;; Up to uncertainty with '-, we've covered all scalar styles.
        

;(defun yaml-reader (stream)
;  "Central function of all module. Analog of read-token in standard Lisp reader."
;  ...)


(def-yaml-reader yaml-ns-token-reader (stream prev-char)
  ;; Lets first assume that prev-char is ns-char
  (iter-main-loop
   (if-first-time (collect prev-char into res))
   (cond
     ((or (whitespace-p char) (b-break-p char)) (finish))
     (t (collect char into res)))
   (finally (return-from main-loop (values (apply #'strcat res)
                                           (if (characterp char) char nil))))))

(defparameter document-version nil)

(defun yaml-directive-handler (version)
  (declare (special document-version))
  (cl-ppcre:register-groups-bind (major minor)
      ("^(\\d+)\\.(\\d+)$" version)
    (let ((major (parse-integer major))
          (minor (parse-integer minor)))
      (if (not (equal major 1))
          (error "Declared major version ~a of the document
differs from major version 1 of the processor." major)
          (if (> minor 2)
            (warn "Minor version ~a of document is greater than
minor version 2 of the processor. Attempting to parse as version 1.2" minor)
            ;; Some adjustments should be made when minor version is
            ;; less than minor version of the processor.
            (setf document-version `(,major ,minor)))))))

(defparameter tag-handles
  '(("!" . "!") ("!!" . "tag:yaml.org,2002:")))

(defun valid-tag-prefix-p (prefix)
  ;; YaML 1.2 specification says, that prefix should be either local,
  ;; Or valid URI, containing at least scheme and authority
  ;; However, YaML basic tag prefix "tag:yaml.org,2002:" does not conform to
  ;; the rule <scheme>://<authority>
  ;; Hence, we require, that string contains only URI characters and begins
  ;; with a <scheme>
  (or (and (local-tag-p prefix) (valid-local-tag-p prefix))
      (valid-uri-p prefix)))

(defun valid-tag-handle-p (handle)
  (cl-ppcre:all-matches "^!([a-zA-Z0-9-]*!)?$" handle))

;; (defsetf assoc (place assoc-lst) (new-value)
;;   "It is assumed, that ASSOC-LST is the name of the assoc list,
;; not a form evaluating to it.
;; It contrary, place is evaluated."
;;   (sb-int:once-only (place)
;;     `(let ((it (assoc ,place ,assoc-lst)))
;;        (if it
;;            (setf (cdr it) ,new-value)
;;            (push `(,',prefix . ,',new-value) ,assoc-lst)))))

(defun tag-directive-handler (handle prefix)
  (declare (special tag-handles))
  (if (valid-tag-prefix-p prefix)
      (if (valid-tag-handle-p handle)
          (setf (cdr (assoc handle tag-handles)) prefix)
          (error "Tag handle ~a is invalid." handle))
      (error "Tag prefix ~a is invalid." prefix)))

(defparameter directives
  `(("YAML" . #'yaml-directive-handler)
    ("TAG" . #'tag-directive-handler)))

(def-yaml-reader yaml-directive-reader (stream prev-char)
  (iter-main-loop
   (cond
     ((whitespace-p char) nil)
     ((b-break-p char) (finish))
     ((if-first-time nil (char= char #\#))
      (descend-with-reader read-comment))
     (t (descend-with-reader yaml-ns-token-reader (collect value into res))))
   (finally
    (if res
        (let ((handler (assoc (car res) directives :test #'equal)))
          (if handler
              (apply (cdr handler) (cdr res))
              (warn "Unknown directive ~a, ignoring and hoping for good."
                    (car res))))
        (signal-reader-error "Empty directive specification line!")))))


;; (def-yaml-reader bare-document-reader (stream prev-char))


;; (def-yaml-reader read-mark (mark-char n stream prev-char)
;;   "Tries to read a mark from stream.
;; Mark is a N-ple of MARK-CHAR characters, followed by whitespace, b-break or eof."
;;   (if (char= prev-char mark-char)
;;       (decf n))
;;   (let (mark-obtained)
;;     (iter-main-loop
;;      (cond ((char= char mark-char)

;; (defun read-end-of-declarations (stream prev-char)
;;   (read-mark #\- 3 stream prev-char))

;; (defun read-end-of-document (stream prev-char)
;;   (read-mark #\. 3 stream prev-char))

;; (def-yaml-reader document-stream-reader (stream prev-char)
;;   (let ((begin-of-line-p t))
;;     (iter-main-loop
;;      (cond
;;        ((whitespace-p char) (setf begin-of-line-p nil))
;;        ((b-break-p char) (setf begin-of-line-p t))
;;        ((char= char #\#) (descend-with-reader read-comment))
;;        ((char= char #\%) (if begin-of-line-p
;;                              (descend-with-reader directive-reader)
;;                              (descend-with-reader bare-document-reader)))
;;        ((char= char #\-) (if begin-of-line-p
;;                              (descend-with-reader read-end-of-declarations
;;                                                   ;; kludgy, but we will find\
;;                                                   ;; a better solution
;;                                ...)
;;                              (descend-with-reader bare-document-reader)))


;;; OK, now we will try to sturm block scalar styles
;;; (not having been able to fully grasp flow maps and sequences, that is)
;;; But nevertheless.

(let ((indent-indicators '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
      (chomping-indicators '(#\- #\+)))
  (def-yaml-reader block-scalar-header-reader (stream prev-char)
    (let (indent-perk chomp-perk)
      (iter-main-loop
       (cond
	 ((find char indent-indicators :test #'char=)
	  (if indent-perk
	      (error "Indentation indicator should occur only once.")
	      (setf indent-perk char)))
	 ((find char chomping-indicators :test #'char=)
	  (if chomp-perk
	      (error "Chomping indicator should occur only once.")
	      (setf chomp-perk char)))
	 ((whitespace-p char) (if (char= (peek-char nil stream nil t t) #\#)
				  (progn (next char)
					 (descend-with-reader read-comment))))
	 ((b-break-p char) (finish)))
	 (finally (return-from main-loop
		    (values `((indent . ,(parse-integer (string (or indent-perk #\0))))
			      (chomp . ,(cdr (assoc chomp-perk '((#\- . strip)
								 (nil . clip)
								 (#\+ . keep))))))
			    nil)))))))

(def-yaml-reader left-spaces-read (stream prev-char)
  (let ((n-spaces (if (char= prev-char #\space) 1 0)))
    (iter-main-loop
     (cond ((char= char #\space) (incf n-spaces))
	   ((b-break-p char) (finish))
	   (t (finish)))
     (finally (yaml-return n-spaces (if (characterp char) char))))))

(def-yaml-reader line-reader (stream prev-char)
  (iter-main-loop
   (cond ((b-break-p char) (finish))
	 (t (collect char into res)))
   (finally (yaml-return (apply #'strcat `(,prev-char ,@res)) (if (characterp char) char)))))

(def-yaml-reader block-scalar-reader (stream prev-char)
  (let ((perks (block-scalar-header-reader stream prev-char))
	my-indentation exit-indentation)
    (fart perks)
    (iter-main-loop
     (if exit-indentation
	 (finish)
	 (descend-with-reader left-spaces-read
           (cond ((and my-indentation (< value my-indentation))
		  (setf exit-indentation value))
		 ((and (not my-indentation) (not (char= char #\newline)))
		  (setf my-indentation value)
		  (descend-with-reader line-reader
                    (collect value into res)
		    (when (char= char #\newline)
		      (collect char into res)
		      (next char))))
		 (my-indentation
		  (let ((norm-indentation (- value my-indentation)))
		    (descend-with-reader line-reader
                      (collect (strcat (make-string norm-indentation
						    :initial-element #\space)
				       value) into res)
		      (when (char= char #\newline)
			(collect char into res)
			(next char))))))))
     (finally (yaml-return (apply #'strcat res) (if (characterp char) char))))))

;; Now, how do we actually treat newlines and so on?
;; And there is this issue with empty lines also.


					     
	 
(defparameter indentation-level -1
  "Each document starts as if having indentation level -1.")
