;;;; This file is one of components of CL-YACLYAML system, licenced under GPL, see COPYING for details

(in-package #:cl-yaclyaml)

;;; Parsing presentation stream

(define-esrap-env yaclyaml)

(register-yaclyaml-context n nil)
(setf n -1)
(register-yaclyaml-context indent-style determined autodetect)
(register-yaclyaml-context context block-out block-in flow-out flow-in block-key flow-key)
(register-yaclyaml-context block-scalar-chomping clip keep strip)
(register-yaclyaml-context block-scalar-style literal folded)

;;; Indicator characters

(defmacro define-alias-rules (clauses)
  "When actual content is discarded anyways."
  `(progn ,@(mapcar (lambda (x)
		      `(define-rule ,(car x) ,(cadr x)
			 (:constant nil)))
		    clauses)))

(defmacro define-context-forcing-rule (context-name forsee-name &optional (expression forsee-name))
  "Define a rule, that enforces a context, when attempting to parse the EXPRESSION."
  `(define-rule ,(sb-int:symbolicate context-name "-" forsee-name)
       (wrap ""
	     ,expression)
     (:wrap-around (let ((context ,(sb-int:keywordicate context-name)))
		     (call-parser)))))

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
(define-rule s-white (or s-space s-tab))

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
(define-rule ns-esc-space #\space (:constant nil))
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


(define-rule s-indent-<n (cond (autodetect-indent-style (* s-space))
			       (determined-indent-style (* (- n 1) s-space)))
  (:lambda (x) (length x)))
(define-rule s-indent-<=n (cond (autodetect-indent-style (* s-space))
				(determined-indent-style (* n s-space)))
  (:lambda (x) (length x)))
(define-rule s-indent-=n (cond (autodetect-indent-style (* (+ n 1) nil s-space))
			       (determined-indent-style (* n n s-space)))
  (:lambda (x) (length x)))

(define-rule start-of-line (or (<- sof) (<- b-char)))

(define-rule s-separate-in-line (or (+ s-white) start-of-line))

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
(define-context-forcing-rule flow-in b-l-folded)

(define-rule s-flow-folded (and (? s-separate-in-line)
				flow-in-b-l-folded
				s-flow-line-prefix)
  (:destructure (sep0 content pref0)
		(declare (ignore sep0 pref0))
		content))

;; comments

(define-rule c-nb-comment-text (and #\# (* nb-char))
  (:constant nil))

(define-rule b-comment (or (-> eof)  b-char))

(define-rule s-b-comment (and (? (and s-separate-in-line (? c-nb-comment-text))) b-comment)
  (:constant nil))

(define-rule l-comment (and s-separate-in-line (? c-nb-comment-text) b-comment)
  (:constant nil))

(define-rule s-l-comments (and (or s-b-comment start-of-line) (* l-comment))
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter tag-handles (make-hash-table :test #'equal))
  (defparameter default-yaml-tagspace "tag:yaml.org,2002:")
  (defparameter default-local-tagspace "!")
  (setf (gethash :secondary-tag-handle tag-handles) default-yaml-tagspace
	(gethash :primary-tag-handle tag-handles) default-local-tagspace))



(define-rule ns-tag-directive (and "TAG" s-separate-in-line c-tag-handle
				   s-separate-in-line ns-tag-prefix)
  (:destructure (tag sep handle sep1 prefix)
		(declare (ignore tag sep sep1))
		(if (gethash handle tag-handles)
		    (error "The TAG directive must be given at most once per handle in the same document.")
		    (setf (gethash handle tag-handles) prefix))))
(define-rule c-tag-handle (or c-named-tag-handle
			      c-secondary-tag-handle
			      c-primary-tag-handle))
(define-rule c-primary-tag-handle #\! (:constant :primary-tag-handle))
(define-rule c-secondary-tag-handle "!!" (:constant :secondary-tag-handle))
(define-rule c-named-tag-handle (and #\! (+ ns-word-char) #\!)
  (:destructure (start meat end)
		(declare (ignore start end))
		`(:named-tag-handle ,(text meat))))

(define-rule ns-tag-prefix (or c-ns-local-tag-prefix
			       ns-global-tag-prefix)
  (:text t))
(define-rule c-ns-local-tag-prefix (and #\! (* ns-uri-char)))
(define-rule ns-global-tag-prefix (and ns-tag-char (* ns-uri-char)))
  
;;; Node properties

(define-rule c-ns-properties (or (and c-ns-tag-property (? (cond (s-separate c-ns-anchor-property))))
				 (and c-ns-anchor-property (? (cond (s-separate c-ns-tag-property)))))
  (:lambda (lst)
    `(:properties ,@(remove-if-not #'identity lst))))

(define-rule c-ns-tag-property (or c-verbatim-tag c-ns-shorthand-tag c-non-specific-tag))
(define-rule c-verbatim-tag (cond ("!<" (first (and (+ ns-uri-char) ">"))))
  (:lambda (x) `(:tag . ,(text x))))

(defun resolve-handle (handle)
  (or (gethash handle tag-handles)
      (error "Unknown handle ~a. Did you forget to declare it?" handle)))
		
(define-rule c-ns-shorthand-tag (and c-tag-handle (+ ns-tag-char))
  (:destructure (handle meat)
		`(:tag . ,(text (resolve-handle handle) meat))))

(define-rule c-non-specific-tag "!"
  (:lambda (x) (declare (ignore x)) `(:tag . :vanilla)))

(define-rule c-ns-anchor-property (cond (#\& ns-anchor-name))
  (:lambda (x) `(:anchor . ,(text x))))
(define-rule ns-anchor-char (and (! c-flow-indicator) ns-char))
(define-rule ns-anchor-name (+ ns-anchor-char))

;;; alias nodes
(define-rule c-ns-alias-node (cond (#\* ns-anchor-name))
  (:lambda (x) `(:alias . ,(text x))))

;;; empty node
(define-rule e-scalar "" (:constant :empty))
(define-rule e-node e-scalar
  (:lambda (x)
    `((:properties (:tag . :non-specific)) (:content . ,x))))
    
;;; double-quoted-scalars

(define-rule nb-double-char (or c-ns-esc-char (and (! #\\) (! #\") nb-json)))
(define-rule ns-double-char (and (! s-white) nb-double-char))

(define-rule c-double-quoted (and #\" nb-double-text #\")
  (:destructure (start meat end)
		(declare (ignore start end))
		(text meat)))
(define-rule nb-double-text (cond ((or block-key-context flow-key-context) nb-double-one-line)
				  ((or block-out-context
				       block-in-context
				       flow-out-context
				       flow-in-context) nb-double-multi-line)))

(define-rule nb-double-one-line (* nb-double-char))

(define-context-forcing-rule flow-in l-empty)

(define-rule s-double-escaped (and (* s-white) #\\ b-non-content
				   (* flow-in-l-empty) s-flow-line-prefix)
  (:destructure (white slash bnc empties pref)
		(declare (ignore slash bnc))
		`(,white ,empties ,(mapcar (lambda (x) (if (numberp x) (make-string x :initial-element #\space) x))
					   pref))))
(define-rule s-double-break (or s-double-escaped s-flow-folded))
(define-rule nb-ns-double-in-line (* (and (* s-white) ns-double-char)))
(define-rule s-double-next-line (and s-double-break
				     (? (and ns-double-char
					     nb-ns-double-in-line
					     (or s-double-next-line (* s-white))))))

(define-rule nb-double-multi-line (and nb-ns-double-in-line
				       (or s-double-next-line (* s-white))))

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
(define-rule nb-single-text (cond ((or block-key-context flow-key-context) nb-single-one-line)
				  ((or block-out-context
				       block-in-context
				       flow-out-context
				       flow-in-context) nb-single-multi-line)))

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

(define-rule l-chomped-empty (cond ((or strip-block-scalar-chomping
					clip-block-scalar-chomping) l-strip-empty)
				   (keep-block-scalar-chomping l-keep-empty)))
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

(let ((chomping-map '(("+" . :keep) ("-" . :strip) ("" . :clip)))
      (style-map '(("|" . :literal) (">" . :folded))))
  (define-rule c-l-block-scalar (wrap (and (or "|" ">") c-b-block-header)
				      block-scalar-content)

    (:wrap-around 
     (let ((block-scalar-chomping (cdr (assoc (cdr (assoc :block-chomping-indicator
							  (cadr wrapper)))
					      chomping-map :test #'equal)))
	   (block-scalar-style (cdr (assoc (car wrapper) style-map :test #'equal))))
       (let ((it (cdr (assoc :block-indentation-indicator
			     (cadr wrapper)))))
	 (if (not (equal it ""))
	     (let ((n (+ n (parse-integer it)))
		   (indent-style :determined))
	       (call-parser))
	     (let ((indent-style :autodetect))
	       (call-parser))))))
    (:text t)))

(define-rule block-scalar-content (wrap (cond (autodetect-indent-style detect-indent)
					      (t ""))
					(cond (literal-block-scalar-style l-literal-content)
					      (folded-block-scalar-style l-folded-content)))
  (:wrap-around
   (if (equal wrapper "")
       (call-parser)
       (let ((n wrapper)
	     (indent-style :determined))
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
				(and (or #\? #\: #\-) (& ns-plain-safe)))
  (:destructure (first second)
		(or first second)))

(define-rule ns-plain-safe (cond ((or flow-out-context
				      block-key-context
				      block-in-context
				      block-out-context) ns-plain-safe-out)
				 ((or flow-in-context flow-key-context) ns-plain-safe-in)))
(define-rule ns-plain-safe-out ns-char)
(define-rule ns-plain-safe-in (and (! c-flow-indicator) ns-char))
(define-rule ns-plain-char (cond ((and (! #\:) (! #\#)) ns-plain-safe)
				 ((<- ns-char) #\#)
				 (t (and #\: (-> ns-plain-safe)))))
  
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
				    (+ (cond (s-indent-=n c-l-block-seq-entry))))
  (:wrap-around (let ((n wrapper)
		      (indent-style :determined)
		      (context :block-in))
		  (call-parser))))

(define-rule detect-block-sequence (wrap ""
					 (& (first (and s-indent-=n (& (and #\- (! ns-char)))))))
  (:wrap-around (let ((indent-style :autodetect))
		  (call-parser))))

(define-rule detect-block-mapping (wrap "" (& (first (and s-indent-=n ns-char))))
  (:wrap-around (let ((indent-style :autodetect))
		  (call-parser))))


(define-rule c-l-block-seq-entry (cond ((and "-" (! ns-char)) s-l+block-indented)))

(define-rule s-l+block-indented (or compact-block-node
				    s-l+block-node
				    (first (and e-node s-l-comments))))

(define-rule compact-block-node (wrap (* 1 nil s-space)
				      (or ns-l-compact-sequence ns-l-compact-mapping))
  (:wrap-around (let ((n (+ n 1 (length wrapper)))
		      (indent-style :determined))
		  (call-parser)))
  (:lambda (x)
    `((:properties (:tag . :non-specific)) (:content . ,x))))
		  
(define-rule ns-l-compact-sequence (and c-l-block-seq-entry
					(* (cond (s-indent-=n c-l-block-seq-entry))))
  (:destructure (first rest)
		`(,first ,. rest)))

(define-rule l+block-mapping (wrap detect-block-mapping
				   (+ (cond (s-indent-=n ns-l-block-map-entry))))
  (:wrap-around (let ((n wrapper)
		      (indent-style :determined))
		  (call-parser)))
  (:lambda (lst)
    `(:mapping ,.lst)))

(define-rule ns-l-block-map-entry (or c-l-block-map-explicit-entry
				      ns-l-block-map-implicit-entry)
  (:destructure (key value)
		`(,key . ,value)))
(define-rule c-l-block-map-explicit-entry (and c-l-block-map-explicit-key
					       (or l-block-map-explicit-value
						   e-node)))


(define-rule c-l-block-map-explicit-key (wrap ""
					      (cond (#\? s-l+block-indented)))
  (:wrap-around (let ((context :block-out))
		  (call-parser))))
(define-rule l-block-map-explicit-value (wrap ""
					      (cond ((and s-indent-=n ":") s-l+block-indented)))
  (:wrap-around (let ((context :block-out))
		  (call-parser))))

(define-rule ns-l-block-map-implicit-entry (and (or ns-s-block-map-implicit-key
						    e-node)
						c-l-block-map-implicit-value))
(define-rule ns-s-block-map-implicit-key (wrap ""
					       (or c-s-implicit-json-key
						   ns-s-implicit-yaml-key))
  (:wrap-around (let ((context :block-key))
		  (call-parser))))
(define-rule c-l-block-map-implicit-value (wrap #\:
						(or s-l+block-node
						    (first (and e-node s-l-comments))))
  (:wrap-around (let ((context :block-out))
		  (call-parser))))
  
(define-rule ns-l-compact-mapping (and ns-l-block-map-entry
				       (* (cond (s-indent-=n ns-l-block-map-entry))))
  (:destructure (first rest)
		`(:mapping ,first ,.rest)))

;; block nodes

(define-rule s-l+block-node (or s-l+block-in-block s-l+flow-in-block))
(define-rule s-l+flow-in-block (wrap ""
				     (cond (s-separate (first (and ns-flow-node s-l-comments)))))
  (:wrap-around (let ((context :flow-out)
		      (n (1+ n)))
		  (call-parser))))

(define-rule s-l+block-in-block (or s-l+block-scalar s-l+block-collection))

(define-rule s-separate-n+1 (wrap "" s-separate)
  (:wrap-around (let ((n (1+ n)))
		  (call-parser))))

(defparameter vanilla-scalar-tag "tag:yaml.org,2002:str")
(defparameter vanilla-mapping-tag "tag:yaml.org,2002:map")
(defparameter vanilla-sequence-tag "tag:yaml.org,2002:seq")

(define-rule s-l+block-scalar (and s-separate-n+1 
				   block-node-properties
				   c-l-block-scalar)
  (:destructure (sep props content)
		(declare (ignore sep))
		(crunch-tag-into-properties props vanilla-scalar-tag vanilla-scalar-tag)
		`(,props (:content . ,content))))

(define-rule block-node-properties (wrap ""
					 (? (cond (s-separate c-ns-properties))))
  (:wrap-around (let ((n (1+ n)))
		  (call-parser))))

(defun seq-spaces (n)
  (if (eql context :block-out)
      (1- n)
      n))

(define-rule s-l+block-collection (and block-node-properties
				       s-l-comments
				       (or l+block-sequence-seq-spaces
					   l+block-mapping))
  (:destructure (props comments content)
		(declare (ignore comments))
		(crunch-tag-into-properties props :non-specific (case (car content)
								  (:mapping vanilla-mapping-tag)
								  (t vanilla-sequence-tag)))
		`(,props (:content . ,content))))


(define-rule l+block-sequence-seq-spaces (wrap "" l+block-sequence)
  (:wrap-around (let ((n (seq-spaces n)))
		  (call-parser))))

;; flow collections

(defun in-flow (context)
  (case context
    ((:block-in :block-out :flow-in :flow-out) :flow-in)
    ((:block-key :flow-key) :flow-key)))

(define-rule c-flow-sequence (wrap (and #\[ (? s-separate))
				   (and (? ns-s-flow-seq-entries) #\]))
  (:wrap-around (let ((context (in-flow context)))
		  (call-parser)))
  (:destructure (content brace)
		(declare (ignore brace))
		content))

(define-rule ns-s-flow-seq-entries (and ns-flow-seq-entry
					(? s-separate)
					(? (and #\, (? s-separate) (? ns-s-flow-seq-entries))))
  (:destructure (entry0 sep0 rest)
		(declare (ignore sep0))
		(if rest
		    (destructuring-bind (comma sep1 entries) rest
			(declare (ignore comma sep1))
			`(,entry0 ,. entries))
		    `(,entry0))))

(define-rule ns-flow-seq-entry (or ns-flow-pair ns-flow-node))

(define-rule c-flow-mapping (wrap (and #\{ (? s-separate))
				  (and (? ns-s-flow-map-entries) #\}))
  (:wrap-around (let ((context (in-flow context)))
		  (call-parser)))
  (:destructure (entries brace)
		(declare (ignore brace))
		`(:mapping ,. entries)))

(define-rule ns-s-flow-map-entries (and ns-flow-map-entry
					(? s-separate)
					(? (and #\, (? s-separate) (? ns-s-flow-map-entries))))
  (:destructure (entry0 sep0 rest)
		(declare (ignore sep0))
		(if rest
		    (destructuring-bind (comma sep1 entries) rest
			(declare (ignore comma sep1))
			`(,entry0 ,. entries))
		    `(,entry0))))


(define-rule ns-flow-map-entry (cond ((and #\? s-separate) ns-flow-map-explicit-entry)
				     (t ns-flow-map-implicit-entry))
  (:destructure (key value)
		`(,key . ,value)))
(define-rule ns-flow-map-explicit-entry (or ns-flow-map-implicit-entry
					    (and e-node e-node)))

(define-rule ns-flow-map-implicit-entry (or ns-flow-map-yaml-key-entry
					    c-ns-flow-map-empty-key-entry
					    c-ns-flow-map-json-key-entry))
(define-rule ns-flow-map-yaml-key-entry
    (and ns-flow-yaml-node
	 (cond ((? s-separate) c-ns-flow-map-separate-value)
	       (t e-node))))
(define-rule c-ns-flow-map-empty-key-entry (and e-node c-ns-flow-map-separate-value))

(define-rule c-ns-flow-map-separate-value
    (cond ((and #\: (! ns-plain-safe)) (cond (s-separate ns-flow-node)
					     (t e-node)))))

(define-rule c-ns-flow-map-json-key-entry (and c-flow-json-node
					       (cond ((? s-separate) c-ns-flow-map-adjacent-value)
						     (t e-node))))
(define-rule c-ns-flow-map-adjacent-value (cond (#\: (cond ((? s-separate) ns-flow-node)
							   (t e-node)))))

(define-rule ns-flow-pair (cond ((and #\? s-separate) ns-flow-map-explicit-entry)
				(t ns-flow-pair-entry))
  (:destructure (key value)
    `(:mapping (,key . ,value))))

(define-rule ns-flow-pair-entry (or ns-flow-pair-yaml-key-entry
				    c-ns-flow-map-empty-key-entry
				    c-ns-flow-pair-json-key-entry))


(define-context-forcing-rule flow-key ns-s-implicit-yaml-key)
(define-context-forcing-rule flow-key c-s-implicit-json-key)

(define-rule ns-flow-pair-yaml-key-entry (and flow-key-ns-s-implicit-yaml-key
					      c-ns-flow-map-separate-value))

(define-rule c-ns-flow-pair-json-key-entry (and flow-key-c-s-implicit-json-key
						c-ns-flow-map-adjacent-value))

;; FIXME: implement restriction on the length of the key
;; FIXME: implement n/a in the indentation portion
(define-rule ns-s-implicit-yaml-key (and ns-flow-yaml-node (? s-separate-in-line))
  (:lambda (lst) (car lst)))
(define-rule c-s-implicit-json-key (and c-flow-json-node (? s-separate-in-line))
  (:lambda (lst) (car lst)))

(define-rule ns-flow-yaml-content ns-plain)
(define-rule c-flow-json-content (or c-flow-sequence c-flow-mapping c-single-quoted c-double-quoted))
(define-rule ns-flow-content (or (tag :yaml ns-flow-yaml-content)
				 (tag :json c-flow-json-content)))


(defmacro with-ensured-properties-not-alias (var &body body)
  `(if (alias-p ,var)
       ,var
       (let ((,var (cond ((property-node-p ,var) ,var)
			 (t `(,(list :properties) (:content . ,,var))))))
	 ,@body)))

(define-rule ns-flow-yaml-node (or c-ns-alias-node
				   ns-flow-yaml-content
				   ns-flow-yaml-properties-node)
  (:lambda (x)
    (with-ensured-properties-not-alias x
      (let ((props (assoc :properties x))
	    (content (cdr (assoc :content x))))
	(crunch-tag-into-properties props :non-specific vanilla-scalar-tag)
	`(,props (:content . ,content))))))
				    
(define-rule ns-flow-yaml-properties-node (and c-ns-properties
					       (or (cond (s-separate ns-flow-yaml-content))
						   e-scalar))
  (:destructure (props content)
		`(,props (:content . ,content))))

(define-rule c-flow-json-node (and (? (first (and c-ns-properties s-separate)))
				   c-flow-json-content)
  (:destructure (props content)
		(if (atom content)
		    (crunch-tag-into-properties props vanilla-scalar-tag vanilla-scalar-tag)
		    (crunch-tag-into-properties props :non-specific (if (equal (car content) :mapping)
									vanilla-mapping-tag
									vanilla-sequence-tag)))
		`(,props (:content . ,content))))

(define-rule ns-flow-node (or c-ns-alias-node
			      ns-flow-content
			      ns-flow-properties-node)
  (:lambda (x)
    ;; (format t "first: ~a~%" x)
    (with-ensured-properties-not-alias x
      ;; (format t "~a~%" x)
      (let ((props (assoc :properties x))
	    (content (cdr (assoc :content x))))
	(destructuring-bind (content-type content) content
	  ;; (format t "~a ~a~%" content-type content)
	  ;; (format t "~a~%" props)
	  (crunch-tag-into-properties props
				      (cond ((eql content-type :yaml) :non-specific)
					    ((eql content-type :json)
					     (cond ((atom content) vanilla-scalar-tag)
						   ((eql (car content) :mapping) :non-specific)
						   (t :non-specific))))
				      (cond ((atom content) vanilla-scalar-tag)
					    ((eql (car content) :mapping) vanilla-mapping-tag)
					    (t vanilla-sequence-tag)))
	  ;; (format t "~a~%" props)
	  `(,props (:content . ,content)))))))

(define-rule ns-flow-properties-node (and c-ns-properties
					  (cond (s-separate ns-flow-content)
						(t (tag :yaml e-scalar))))
  (:destructure (props content)
		`(,props (:content . ,content))))


;;; YaML documents

;; FIXME: correct definition of byte-order-mark
(define-rule c-byte-order-mark (and #\UEFBB #\INVERTED_QUESTION_MARK)
  (:constant :utf8-bom))

(define-rule l-document-prefix (and (? c-byte-order-mark) (* l-comment)))
(define-rule c-directives-end (* 3 3 #\-))
(define-rule c-document-end (* 3 3 #\.))
(define-rule l-document-suffix (and c-document-end s-l-comments))

(define-rule c-forbidden (and start-of-line
			      (or c-directives-end c-document-end)
			      (or b-char s-white (-> eof))))

(define-rule l-bare-document (* (and (! c-forbidden) (and (* nb-char) (? b-break))))
  (:lambda (text)
    (let ((n -1)
	  (indent-style :determined)
	  (context :block-in))
      `(:document ,(yaclyaml-parse 's-l+block-node (text text))))))

(define-rule l-explicit-document (cond (c-directives-end (or l-bare-document
							     (first (and e-node s-l-comments)))))
  (:lambda (x) `(:document ,(if (and (consp x) (eql (car x) :document))
				(cadr x)
				x))))

(define-rule l-directive-document (wrap "" %l-directive-document)
  (:wrap-around (let ((yaml-version nil)
		      (tag-handles (make-hash-table :test #'equal)))
		  (setf (gethash :secondary-tag-handle tag-handles) default-yaml-tagspace
			(gethash :primary-tag-handle tag-handles) default-local-tagspace)
		  (call-parser))))

(let (path)
  (declare (special path))
  (defun try-resolve (handle)
    (multiple-value-bind (prefix position)
	(yaclyaml-parse 'c-tag-handle handle :junk-allowed t)
      ;; (format t "~a ~a ~a~%" handle prefix position)
      (if prefix
	  (let ((pos (position prefix path :test #'equal)))
	    (if (and pos position)
		(error "Loop in prefixes resolution: ~a~%" `(,@(subseq path pos) ,prefix))
		(strcat (gethash (if (atom prefix)
				     prefix
				     (cadr prefix))
				 tag-handles)
			(if position
			    (subseq handle position)))))
	  handle)))

  (defun compile-tag-handles ()
    (iter (for (key . nil) in (hash->assoc tag-handles))
	  (let ((path `(,key)))
	    (declare (special path))
	    (iter (while t)
		  (let ((val (gethash key tag-handles)))
		    (let ((try-val (try-resolve val)))
		      (if (equal try-val val)
			  (terminate)
			  (setf (gethash key tag-handles) try-val
				val try-val)))))))))

(define-rule %l-directive-document (wrap (+ l-directive) l-explicit-document)
  (:wrap-around (compile-tag-handles)
		(call-parser)))


(define-rule l-any-document (or l-directive-document
				l-explicit-document
				l-bare-document))

(define-rule l-yaml-stream (and (cond ((* l-document-prefix) (? l-any-document)))
				(* (cond ((and (+ l-document-suffix) (* l-document-prefix)) (? l-any-document))
					 ((and (* l-document-prefix)) (? l-explicit-document)))))
  (:destructure (first rest)
		;; (format t "~a ~a~%" first rest)
		`(,first ,.(if (and rest (car rest)) rest (list)))))