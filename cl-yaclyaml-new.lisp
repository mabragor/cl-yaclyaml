(in-package #:cl-yaclyaml)

;;; Implementation of YaML parsing with help of ESRAP.
;;; Hopefully, this will allow me to finish faster, than with
;;; Lisp-reader-like approach.

(define-esrap-env yaclyaml)

;;; Indicator characters

(defmacro define-alias-rules (clauses)
  "When actual content is discarded anyways."
  `(progn ,@(mapcar (lambda (x)
		      `(define-rule ,(car x) ,(cadr x)
			 (:constant nil)))
		    clauses)))

(define-rule foo "foo"
  (:constant "foo"))

(define-rule goo "foo"
  (:constant "foo"))

(defmacro define-context-rules (context-var &rest contexts)
  "Define rules, that consume nothing, but pass only if CONTEXT-VAR is equal to
something particular.
For cases, when you do not want to keep a bunch of sub-rules with different :WHEN's around."
  `(progn (defparameter ,context-var ,(sb-int:keywordicate (car contexts)))
	  ,@(mapcar (lambda (context-name)
		      `(progn
			 (defun ,(sb-int:symbolicate context-name "-CONTEXT-P") (x)
			   (declare (ignore x))
			   (eql ,context-var ,(sb-int:keywordicate context-name)))
			 (define-rule ,(sb-int:symbolicate context-name "-CONTEXT")
			     (,(sb-int:symbolicate context-name "-CONTEXT-P") "")
			   (:constant nil))))
		    contexts)))


(define-alias-rules (;; block structure indicators
		     (c-sequence-entry #\-)
		     (c-mapping-key #\?)
		     (c-mapping-value #\:)
		     ;; flow collection indicators
		     (c-colon-entry #\,)
		     (c-sequence-start #\[)
		     (c-sequence-end #\])
		     (c-mapping-start #\{)
		     (c-mapping-end #\})
		     ;; comments
		     (c-comment #\#)
		     ;; tags and aliases
		     (c-anchor #\&)
		     (c-alias #\*)
		     (c-tag #\!)
		     ;; block-scalar style
		     (c-literal #\|)
		     (c-folded #\>)
		     ;; quoted scalars
		     (c-single-quote #\')
		     (c-double-quote #\")
		     (c-directive #\%)
		     (c-reserved (or #\@ #\`))))

(defun c-printable-p (char)
  (let ((code (char-code char)))
    (or (equal code 9)
	(equal code #xa)
	(equal code #xd)
	(and (>= code #x20) (<= code #x7e))
	(equal code #x85)
	(and (>= code #xa0) (<= code #xd7ff))
	(and (>= code #xe000) (<= code #xfffd))
	(and (>= code #x10000) (<= code #x10ffff)))))
(define-rule c-printable (c-printable-p character))
(defun nb-json-p (char)
  (let ((code (char-code char)))
    (or (equal code 9)
	(and (>= code #x20) (<= code #x10ffff)))))
(define-rule nb-json (nb-json-p character))

(define-rule c-indicator (or c-sequence-entry c-mapping-key c-mapping-value
			     c-colon-entry c-sequence-start c-sequence-end c-mapping-start c-mapping-end
			     c-comment
			     c-anchor c-alias c-tag
			     c-literal c-folded
			     c-single-quote c-double-quote
			     c-directive
			     c-reserved))

(define-rule c-flow-indicator (or c-colon-entry c-sequence-start c-sequence-end c-mapping-start c-mapping-end))

(define-rule b-line-feed #\newline)
(define-rule b-carriage-return #\return)
(define-rule b-char (or b-line-feed b-carriage-return))

(define-rule b-break (or (and b-carriage-return b-line-feed)
			 b-carriage-return
			 b-line-feed)
  (:constant #\newline))

;; TODO - include BOM
(define-rule nb-char (and (! b-char) character)
  (:lambda (x) (cadr x)))

(define-rule s-space #\space)
(define-rule s-tab #\tab)
(define-rule s-white (or s-space s-tab)
  (:constant #\space))

(define-rule ns-char (and (! s-white) nb-char)
  (:lambda (x) (cadr x)))

(define-rule ns-dec-digit (character-ranges (#\0 #\9)))

(define-rule ns-hex-digit (or ns-dec-digit (~ #\a) (~ #\b) (~ #\c) (~ #\d) (~ #\e) (~ #\f)))

(define-rule ns-ascii-letter (alpha-char-p character))

(define-rule ns-word-char (or ns-dec-digit ns-ascii-letter #\-))

(define-rule ns-uri-char (or (and #\% ns-hex-digit ns-hex-digit)
			     ns-word-char
			     #\# #\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\,
			     #\_ #\. #\! #\~ #\* #\' #\( #\) #\[ #\])
  (:text t))
			     
(define-rule ns-tag-char (and (! #\!) (! c-flow-indicator) ns-uri-char)
  (:text t))

;; Escaped characters

(define-rule c-escape #\\)

(define-rule ns-esc-null #\0 (:constant (code-char 0)))
(define-rule ns-esc-bell #\a (:constant (code-char 7)))
(define-rule ns-esc-backspace #\b (:constant (code-char 8)))
(define-rule ns-esc-horizontal-tab #\t (:constant (code-char 9)))
(define-rule ns-esc-line-feed #\n (:constant (code-char 10)))
(define-rule ns-esc-vertical-tab #\v (:constant (code-char 11)))
(define-rule ns-esc-form-feed #\f (:constant (code-char 12)))
(define-rule ns-esc-carriage-return #\r (:constant (code-char 13)))
(define-rule ns-esc-escape #\e (:constant (code-char 27)))
(define-rule ns-esc-space #\space)
(define-rule ns-esc-double-quote #\")
(define-rule ns-esc-slash #\/)
(define-rule ns-esc-backslash #\\)
(define-rule ns-esc-next-line #\N (:constant (code-char 133)))
(define-rule ns-esc-non-breaking-space #\_ (:constant (code-char 160)))
(define-rule ns-esc-line-separator #\L (:constant (code-char 8232)))
(define-rule ns-esc-paragraph-separator #\P (:constant (code-char 8233)))
(define-rule ns-esc-8-bit (and "x" ns-hex-digit ns-hex-digit)
  (:destructure (x decs ones)
		(declare (ignore x))
		(code-char (parse-integer (format nil "~a~a" decs ones) :radix 16))))

(define-rule ns-esc-16-bit (and "u" ns-hex-digit ns-hex-digit ns-hex-digit ns-hex-digit)
  (:destructure (u 3rd 2nd 1st 0th)
		(declare (ignore u))
		(code-char (parse-integer (format nil "~a~a~a~a" 3rd 2nd 1st 0th) :radix 16))))

(define-rule ns-esc-32-bit (and "U"
				ns-hex-digit ns-hex-digit ns-hex-digit ns-hex-digit
				ns-hex-digit ns-hex-digit ns-hex-digit ns-hex-digit)
  (:lambda (lst)
    (code-char (parse-integer (text (cdr lst)) :radix 16))))

(define-rule c-ns-esc-char (and #\\ (or ns-esc-null
					ns-esc-bell
					ns-esc-backspace
					ns-esc-horizontal-tab
					ns-esc-line-feed
					ns-esc-vertical-tab
					ns-esc-form-feed
					ns-esc-carriage-return
					ns-esc-escape
					ns-esc-space
					ns-esc-double-quote
					ns-esc-slash
					ns-esc-backslash
					ns-esc-next-line
					ns-esc-non-breaking-space
					ns-esc-line-separator
					ns-esc-paragraph-separator
					ns-esc-8-bit
					ns-esc-16-bit
					ns-esc-32-bit))
  (:destructure (start char)
		(declare (ignore start))
		char))

;; Indentation

(define-context-rules n autodetect)
(define-context-rules c block-out block-in flow-out flow-in block-key flow-key)

(define-rule s-indent-<n (cond (autodetect-context (* s-space))
			       (t (* (- n 1) s-space)))
  (:lambda (x) (length x)))
(define-rule s-indent-<=n (cond (autodetect-context (* s-space))
				(t (* n s-space)))
  (:lambda (x) (length x)))
(define-rule s-indent-=n (cond (autodetect-context (* s-space))
			       (t (* n n s-space)))
  (:lambda (x) (length x)))

(define-rule s-separate-in-line (+ s-white) ; TODO or /* Start line */
  (:constant " "))

(define-rule s-block-line-prefix s-indent-=n)
(define-rule s-flow-line-prefix (and s-indent-=n (? s-separate-in-line)))
(define-rule s-line-prefix (cond ((or block-out-context block-in-context) s-block-line-prefix)
				 ((or flow-out-context flow-in-context) s-flow-line-prefix)))

(define-rule l-empty (cond ((or s-line-prefix s-indent-<n) b-break)))

(define-rule b-l-trimmed (and b-break (+ l-empty))
  (:destructure (bb lst)
		(declare (ignore bb))
		(make-string (length lst) :initial-element #\newline)))

(define-rule b-as-space (and b-break (! l-empty))
  (:constant " "))

(define-rule b-l-folded (or b-l-trimmed b-as-space))

(define-rule s-flow-folded (and (? s-separate-in-line) b-l-folded s-flow-line-prefix)
  (:destructure (sep0 content pref0)
		(declare (ignore sep0 pref0))
		content))

;; comments

(define-rule c-nb-comment-text (and #\# (* nb-char))
  (:constant nil))

(define-rule b-comment b-char) ; TODO or end-of-file

(define-rule s-b-comment (and (? (and s-separate-in-line (? c-nb-comment-text))) b-comment)
  (:constant nil))

(define-rule l-comment (and s-separate-in-line (? c-nb-comment-text) b-comment)
  (:constant nil))

(define-rule s-l-comments (and s-b-comment (* l-comment)) ; TODO or start-of-line
  (:constant nil))

;; separation lines

(define-rule s-separate-lines (or (and s-l-comments s-flow-line-prefix)
				  s-separate-in-line)
  (:constant " "))

(define-rule s-separate (cond ((or block-out-context
				   block-in-context
				   flow-out-context
				   flow-in-context) s-separate-lines)
			      ((or block-key-context flow-key-context) s-separate-in-line)))

;; Directives (finally, something non-trivial)

(define-rule l-directive (and #\% (or ns-yaml-directive
				      ns-tag-directive
				      ns-reserved-directive) s-l-comments)
  (:destructure (start dir end)
		(declare (ignore start end))
		dir))

(define-rule ns-reserved-directive (and ns-directive-name (* (and s-separate-in-line ns-directive-parameter))))
(define-rule ns-directive-name (+ ns-char))
(define-rule ns-directive-parameter (+ ns-char))

;; YAML directive
;;; Lets hack logic of %YAML directive, since this is simple enough for me to understand now.

(defparameter yaml-version nil)
(define-rule ns-yaml-directive (and "YAML" s-separate-in-line ns-yaml-version)
  (:destructure (yaml sep version)
		(declare (ignore yaml sep))
		(if yaml-version
		    (error "The YAML directive must be only given once per document.")
		    (if (not (equal (car version) 1))
			(error "Major version ~a differs from processor's version." (car version))
			(progn (if (> (cadr version) 2)
				   (warn "Minor version is greater than that of the processor, attempt to parse anyway."))
			       (setf yaml-version version)
			       nil)))))
			    

(define-rule ns-yaml-version (and (+ ns-dec-digit) #\. (+ ns-dec-digit))
  (:destructure (major pt minor)
		(declare (ignore pt))
		`(,(parse-integer (text major)) ,(parse-integer (text minor)))))

;; TAG directive
(defparameter tag-handles (make-hash-table :test #'equal))

(define-rule ns-tag-directive (and "TAG" s-separate-in-line c-tag-handle
				   s-separate-in-line ns-tag-prefix)
  (:destructure (tag sep handle sep1 prefix)
		(declare (ignore tag sep sep1))
		(if (gethash handle tag-handles)
		    (error "The TAG directive must be given at most once per handle in the same document.")
		    (setf (gethash handle tag-handles) prefix))))
(define-rule c-tag-handle (or c-named-tag-handle
			      c-secondary-tag-handle
			      c-primary-tag-handle) (:text t))
(define-rule c-primary-tag-handle #\!)
(define-rule c-secondary-tag-handle "!!")
(define-rule c-named-tag-handle (and #\! (+ ns-word-char) #\!))

(define-rule ns-tag-prefix (or c-ns-local-tag-prefix
			       ns-global-tag-prefix))
(define-rule c-ns-local-tag-prefix (and #\! (* ns-uri-char))
  (:destructure (start name)
		(declare (ignore start))
		(make-instance 'tag-prefix :name (text name) :local t)))
(define-rule ns-global-tag-prefix (and ns-tag-char (* ns-uri-char))
  (:lambda (lst)
    (make-instance 'tag-prefix :name (text (flatten name)))))
(defclass tag-prefix ()
  ((name :initarg :name :initform nil :accessor tag-name)
   (local :initarg :local :initform nil :accessor tag-local-p)))

  
;;; Node properties

(define-rule c-ns-properties (or (and c-ns-tag-property (? (and s-separate c-ns-anchor-property)))
				 (and c-ns-anchor-property (? (and s-separate c-ns-tag-property)))))

(define-rule c-ns-tag-property (or c-verbatim-tag c-ns-shorthand-tag c-non-specific-tag))
(define-rule c-verbatim-tag (and "!<" (+ ns-uri-char) ">"))
(define-rule c-ns-shorthand-tag (and c-tag-handle (+ ns-tag-char)))
(define-rule c-non-specific-tag "!")

(define-rule c-ns-anchor-property (and #\& ns-anchor-name))
(define-rule ns-anchor-char (and (! c-flow-indicator) ns-char))
(define-rule ns-anchor-name (+ ns-anchor-char))

;;; alias nodes
(define-rule c-ns-alias-node (and #\* ns-anchor-name))

;;; empty node
;;(define-rule e-scalar ()) ; how to express this I wounder.
    
;;; double-quoted-scalars

(define-rule nb-double-char (or c-ns-esc-char (and (! #\\) (! #\") nb-json)))
(define-rule ns-double-char (and (! s-white) nb-double-char))

(define-rule c-double-quoted (and #\" nb-double-text #\")
  (:destructure (start meat end)
		(declare (ignore start end))
		(text meat)))
(define-rule nb-double-text-flow-out nb-double-multi-line (:when (eql c :flow-out)))
(define-rule nb-double-text-flow-in nb-double-multi-line (:when (eql c :flow-in)))
(define-rule nb-double-text-block-key nb-double-one-line (:when (eql c :block-key)))
(define-rule nb-double-text-flow-key nb-double-one-line (:when (eql c :flow-key)))
(define-rule nb-double-text (or nb-double-text-flow-in
				nb-double-text-flow-out
				nb-double-text-flow-key
				nb-double-text-block-key))

(define-rule nb-double-one-line (* nb-double-char))

(define-rule s-double-escaped (and (* s-white) #\\ b-non-content
				   l-empty s-flow-line-prefix))
(define-rule s-double-break (or s-double-escaped s-flow-folded))
(define-rule nb-ns-double-in-line (* (and (* s-white) ns-double-char)))
;; (define-rule s-double-next-line (and s-double-break
;; 				     (? (and ns-double-char
;; 					     nb-ns-double-in-line
;; 					     (or s-double-next-line (* s-white)))))
;;   (:destructure (bbreak lst)
;; 		(if 
(define-rule nb-double-multi-line (and nb-ns-double-in-line
				       (or s-double-next-line (* s-white)))
  (:destructure (first-line rest-lines)
		(text `(,(string-left-trim '(#\space #\tab) (text first-line))
			 ,(if (whitespace-p (text rest-lines)) "" (text rest-lines))))))

(defun whitespace-p (text)
  (iter (for char in-string text)
	(if (not (member char '(#\tab #\space)))
	    (return nil))
	(finally (return t))))


;;; single-quoted-scalars
(define-rule c-quoted-quote "''"
  (:constant #\'))
(define-rule nb-single-char (or c-quoted-quote (and (! #\') nb-json)))
(define-rule ns-single-char (and (! s-white) nb-single-char))

(define-rule c-single-quoted (and #\' nb-single-text #\')
  (:destructure (start meat end)
		(declare (ignore start end))
		(text meat)))
(define-rule nb-single-text-flow-out nb-single-multi-line (:when (eql c :flow-out)))
(define-rule nb-single-text-flow-in nb-single-multi-line (:when (eql c :flow-in)))
(define-rule nb-single-text-block-key nb-single-one-line (:when (eql c :block-key)))
(define-rule nb-single-text-flow-key nb-single-one-line (:when (eql c :flow-key)))
(define-rule nb-single-text (or nb-single-text-flow-in
				nb-single-text-flow-out
				nb-single-text-flow-key
				nb-single-text-block-key))
(define-rule nb-single-one-line (* nb-single-char))

(define-rule nb-ns-single-in-line (* (and (* s-white) ns-single-char)))
(define-rule s-single-next-line (and s-flow-folded
				     (? (and ns-single-char
					     nb-ns-single-in-line
					     (or s-single-next-line (* s-white))))))
(define-rule nb-single-multi-line (and nb-ns-single-in-line
				       (or s-single-next-line
					   (* s-white))))

;; Block scalars

(define-rule c-b-block-header (and (or (and c-indentation-indicator-ne c-chomping-indicator)
				       (and c-chomping-indicator-ne c-indentation-indicator)
				       (and c-chomping-indicator c-indentation-indicator))
				   s-b-comment)
  (:destructure (content comment)
		(declare (ignore comment))
		content))

(define-rule c-indentation-indicator-ne ns-dec-digit
  (:lambda (x)
    `(:block-indentation-indicator . ,(string x))))
(define-rule c-indentation-indicator (or ns-dec-digit "")
  (:lambda (x)
    `(:block-indentation-indicator . ,(string x))))
(define-rule c-chomping-indicator-ne (or #\- #\+)
  (:lambda (x)
    `(:block-chomping-indicator . ,(string x))))
(define-rule c-chomping-indicator (or #\- #\+ "")
  (:lambda (x)
    `(:block-chomping-indicator . ,(string x))))

(defun hash->assoc (hash)
  (iter (for (key val) in-hashtable hash)
	(collect `(,key . ,val))))

(define-context-rules block-scalar-chomping
   clip keep strip)

(define-rule b-chomped-last b-break
  (:lambda (x)
    (declare (ignore x))
    (case block-scalar-chomping
      (:clip #\newline)
      (:keep #\newline)
      (:strip nil))))

(define-rule b-non-content b-break
  (:constant nil))
(define-rule b-as-line-feed b-break)

(define-rule l-chomped-empty (cond ((or strip-context clip-context) l-strip-empty)
				   (keep-context l-keep-empty)))
(define-rule l-strip-empty (and (* (and s-indent-<=n b-non-content))
				(? l-trail-comments))
  (:constant ""))
(define-rule l-keep-empty (and (* l-empty)
			       (? l-trail-comments)))

(define-rule l-trail-comments (and s-indent-<n c-nb-comment-text b-comment
				   (* l-comment))
  (:constant ""))

(define-rule l-literal-content (and (? (and l-nb-literal-text
					    (* b-nb-literal-next)
					    b-chomped-last))
				    l-chomped-empty)
  (:text t))

(define-context-rules block-scalar-style literal folded)

(let ((chomping-map '(("+" . :keep) ("-" . :strip) ("" . :clip)))
      (style-map '(("|" . :literal) (">" . :folded))))
  (define-rule c-l-block-scalar (wrap (and (or "|" ">") c-b-block-header)
				      block-scalar-content)

    (:wrap-around 
     (let ((block-scalar-chomping (cdr (assoc (cdr (assoc :block-chomping-indicator
							  (cadr wrapper)))
					      chomping-map :test #'equal)))
	   (block-scalar-style (cdr (assoc (car wrapper) style-map :test #'equal)))
	   ;; FIXME: implement auto-detection of indentation
	   (n (let ((it (cdr (assoc :block-indentation-indicator
				    (cadr wrapper)))))
		(if (not (equal it ""))
		    (parse-integer it)
		    :autodetect)))
	   (c :block-in))
       (call-parser)))
    (:text t)))

(define-rule block-scalar-content (wrap (cond (autodetect-context detect-indent)
					      (t ""))
					(cond (literal-context l-literal-content)
					      (folded-context l-folded-content)))
  (:wrap-around
   (if (equal wrapper "")
       (call-parser)
       (let ((n wrapper))
	 (call-parser)))))

(define-rule detect-indent (& (and (* l-empty) s-indent-=n nb-char))
  (:destructure (empties indent char)
		(declare (ignore empties char))
		indent))

(define-rule l-nb-literal-text (and (* l-empty)
				    (cond (s-indent-=n (+ nb-char)))))

(define-rule b-nb-literal-next (and b-as-line-feed l-nb-literal-text))
				    

(define-rule s-nb-folded-text (cond (s-indent-=n (and ns-char (* nb-char)))))
(define-rule l-nb-folded-lines (and s-nb-folded-text
				    (* (and b-l-folded s-nb-folded-text))))

(define-rule s-nb-spaced-text (cond (s-indent-=n (and s-white (* nb-char)))))
(define-rule b-l-spaced (and b-as-line-feed (* l-empty)))
(define-rule l-nb-spaced-lines (and s-nb-spaced-text
				    (* (and b-l-spaced s-nb-spaced-text))))
(define-rule l-nb-same-lines (and (* l-empty)
				  (or l-nb-folded-lines l-nb-spaced-lines)))
(define-rule l-nb-diff-lines (and l-nb-same-lines
				  (* (and b-as-line-feed l-nb-same-lines))))
(define-rule l-folded-content (and (? (and l-nb-diff-lines b-chomped-last))
				   l-chomped-empty))

;;; Plain scalars

(define-rule ns-plain-first (or (and (! c-indicator) ns-char)
				(and (or #\? #\: #\-) (& ns-plain-safe))))
(define-rule ns-plain-safe (cond ((or flow-out-context
				      block-key-context
				      block-in-context
				      block-out-context) ns-plain-safe-out)
				 ((or flow-in-context flow-key-context) ns-plain-safe-in)))
(define-rule ns-plain-safe-out ns-char)
(define-rule ns-plain-safe-in (and (! c-flow-indicator) ns-char))
(define-rule ns-plain-char (or (and (! #\:) (! #\#) ns-plain-safe)
			       (and ns-char (& #\#))
			       (and #\: (& ns-plain-safe))))
  
(define-rule ns-plain (cond ((or flow-out-context
				 flow-in-context
				 block-out-context
				 block-in-context) ns-plain-multi-line)
			    ((or block-key-context flow-key-context) ns-plain-one-line)))

(define-rule nb-ns-plain-in-line (* (and (* s-white) ns-plain-char)))
(define-rule ns-plain-one-line (and ns-plain-first nb-ns-plain-in-line)
  (:text t))

(define-rule s-ns-plain-next-line (and s-flow-folded ns-plain-char nb-ns-plain-in-line))
(define-rule ns-plain-multi-line (and ns-plain-one-line
				      (* s-ns-plain-next-line))
  (:text t))

;; block sequences

(define-rule l+block-sequence (wrap detect-block-sequence
				    (+ (and s-indent-=n c-l-block-seq-entry)))
  (:wrap-around (let ((n wrapper)
		      (c :block-in))
		  (call-parser))))

(define-rule c-l-block-seq-entry (and "-" (! ns-char) s-l+block-indented))

(define-rule s-l+block-indented (or ; (and s-indent-=n) ; something should be clearly done here
				 s-l+block-node
				 (and e-node s-l-comments)))

; (define-rule ns-l-compact-sequence ...)

(define-rule l+block-mapping (wrap detect-block-mapping
				   (+ (and s-indent-=n ns-l-block-map-entry)))
  (:wrap-around (let ((n wrapper))
		  (call-parser))))

(define-rule ns-l-block-map-entry (or c-l-block-map-explicit-entry
				      c-l-block-map-implicit-entry))
(define-rule c-l-block-map-explicit-entry (and c-l-block-map-explicit-key
					       (or l-block-map-explicit-value
						   e-node)))
(define-rule c-l-block-map-explicit-key (wrap ""
					      (cond (#\? s-l+block-indented)))
  (:wrap-around (let ((c :block-out))
		  (call-parser))))
(define-rule l-block-map-explicit-value (wrap ""
					      (cond ((and s-indent-=n ":") s-l+block-indented)))
  (:wrap-around (let ((c :block-out))
		  (call-parser))))

(define-rule ns-l-block-map-implicit-entry (and (or ns-s-block-map-implicit-key
						    e-node)
						c-l-block-map-implicit-value))
(define-rule ns-s-block-map-implicit-key (wrap ""
					       (or c-s-implicit-json-key
						   ns-s-implicit-yaml-key))
  (:wrap-around (let ((c :block-key))
		  (call-parser))))
(define-rule c-l-block-map-implicit-value (wrap #\:
						(or s-l+block-node
						    (and e-node s-l-comments)))
  (:wrap-around (let ((c :block-out))
		  (call-parser))))
  
(define-rule ns-l-compact-mapping (and ns-l-block-map-entry
				       (* (and s-indent-=n ns-l-block-map-entry))))

;; I didn't finish block sequences, but I think, I'll figure out sooner or later how to do them

;; block nodes

;; FIXME: autodetection of indentation should only detect indentation, that's greater, than the current one.

(define-rule s-l+block-node (or s-l+block-in-block s-l+flow-in-block))
(define-rule s-l+flow-in-block (wrap ""
				     (and s-separate ns-flow-node s-l-comments))
  (:wrap-around (let ((c :flow-out)
		      (n :autodetect))
		  (call-parser))))

(define-rule s-l+block-in-block (or s-l+block-scalar s-l+block-collection))

(define-rule s-l+block-scalar (and block-node-properties
				   c-l-block-scalar))

(define-rule block-node-properties (wrap ""
					 (? (and s-separate c-ns-properties)))
  (:wrap-around (let ((n (1+ n)))
		  (call-parser))))

(define-rule s-l+block-collection (and block-node-properties
