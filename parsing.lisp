;;;; This file is one of components of CL-YACLYAML system, licenced under GPL, see COPYING for details

(in-package #:cl-yaclyaml)

(enable-read-macro-tokens)

;;; Parsing presentation stream

(register-yaclyaml-context n nil)
(setf n -1)
(register-yaclyaml-context indent-style determined autodetect)
(register-yaclyaml-context context block-out block-in flow-out flow-in block-key flow-key)
(register-yaclyaml-context block-scalar-chomping clip keep strip)
(register-yaclyaml-context block-scalar-style literal folded)

;;; Indicator characters

(define-alias-rules (;; block structure indicators
		     (c-sequence-entry #\-)
		     (c-mapping-key #\?)
		     (c-mapping-value #\:)
		     ;; flow collection indicators
		     (c-colon-entry #\,)
		     (c-sequence-start #\[)
		     (c-sequence-end #\])
		     (c-mapping-start #\{)
		     (c-mapping-end #\})))

(define-alias-rules (;; comments
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
		     (c-reserved (|| #\@ #\`))))

;; (defun c-printable-p (char)
;;   (let ((code (char-code char)))
;;     (or (equal code 9)
;; 	(equal code #xa)
;; 	(equal code #xd)
;; 	(and (>= code #x20) (<= code #x7e))
;; 	(equal code #x85)
;; 	(and (>= code #xa0) (<= code #xd7ff))
;; 	(and (>= code #xe000) (<= code #xfffd))
;; 	(and (>= code #x10000) (<= code #x10ffff)))))
;; (defmacro if-pred (predicate form)
;;   `(a:apif ,(if (symbolp predicate)
;; 		`(symbol-function ',predicate)
;; 		predicate)
;; 	   ,form
;; 	   it
;; 	   ,(if (symbolp predicate)
;; 		`(fail-parse "Predicate ~a does not hold for ~a" ',predicate it)
;; 		`(fail-parse "Predicate does not hold for ~a" it))))

;; (define-rule c-printable (if-pred c-printable-p character))
;; (defun nb-json-p (char)
;;   (let ((code (char-code char)))
;;     (or (equal code 9)
;; 	(and (>= code #x20) (<= code #x10ffff)))))
;; (define-rule nb-json (if-pred nb-json-p character))

;; (define-rule c-indicator (|| c-sequence-entry c-mapping-key c-mapping-value
;; 			     c-colon-entry c-sequence-start c-sequence-end c-mapping-start c-mapping-end
;; 			     c-comment
;; 			     c-anchor c-alias c-tag
;; 			     c-literal c-folded
;; 			     c-single-quote c-double-quote
;; 			     c-directive
;; 			     c-reserved))

;; (define-rule c-flow-indicator (|| c-colon-entry c-sequence-start c-sequence-end c-mapping-start c-mapping-end))

;; (define-rule b-line-feed #/newline)
;; (define-rule b-carriage-return #/return)
;; (define-rule b-char (|| b-line-feed b-carriage-return))

;; (define-rule b-break (progn (|| (list b-carriage-return b-line-feed)
;; 				b-carriage-return
;; 				b-line-feed)
;; 			    #\newline))

;; ;; TODO - include BOM
;; (define-rule nb-char (progn (! b-char)
;; 			    character))

;; (define-rule s-space #/space)
;; (define-rule s-tab #/tab)
;; (define-rule s-white (|| s-space s-tab))

;; (define-rule ns-char (progn (! s-white)
;; 			    nb-char))

;; ;; TODO: write a character-ranges macro
;; ;; TODO: write ~ (ignore-case macro)

;; (define-rule ns-dec-digit (character-ranges (#\0 #\9)))

;; (define-rule ns-hex-digit (|| ns-dec-digit (~ #\a) (~ #\b) (~ #\c) (~ #\d) (~ #\e) (~ #\f)))

;; (define-rule ns-ascii-letter (if-pred alpha-char-p character))

;; (define-rule ns-word-char (|| ns-dec-digit ns-ascii-letter #/-))

;; ;; I'll go mad trying to get all this new syntax to work
;; (define-rule ns-uri-char (text (|| (list #/% ns-hex-digit ns-hex-digit)
;; 				   ns-word-char
;; 				   #/# #/; #// #/? #/: #/@ #/& #/= #/+ #/$ #/,
;; 				   #/_ #/. #/! #/~ #/* #/' #/( #/) #/[ #/])))
			     
;; (define-rule ns-tag-char (text (progn (! #\!)
;; 				      (! c-flow-indicator)
;; 				      ns-uri-char)))

;; ;; Escaped characters

;; (define-rule c-escape #/\)

;; (define-rule ns-esc-null (progn #/0 (code-char 0)))
;; (define-rule ns-esc-bell (progn #/a (code-char 7)))
;; (define-rule ns-esc-backspace (progn #/b (code-char 8)))
;; (define-rule ns-esc-horizontal-tab (progn #/t (code-char 9)))
;; (define-rule ns-esc-line-feed (progn #/n (code-char 10)))
;; (define-rule ns-esc-vertical-tab (progn #/v (code-char 11)))
;; (define-rule ns-esc-form-feed (progn #/f (code-char 12)))
;; (define-rule ns-esc-carriage-return (progn #/r (code-char 13)))
;; (define-rule ns-esc-escape (progn #/e (code-char 27)))
;; (define-rule ns-esc-space (progn #/space nil))
;; (define-rule ns-esc-double-quote #/") ; TODO: fix this sad notation flaw ")
;; (define-rule ns-esc-slash #//)
;; (define-rule ns-esc-backslash #/\)
;; (define-rule ns-esc-next-line (progn #/N (code-char 133)))
;; (define-rule ns-esc-non-breaking-space (progn #/_ (code-char 160)))
;; (define-rule ns-esc-line-separator (progn #/L (code-char 8232)))
;; (define-rule ns-esc-paragraph-separator (progn #/P (code-char 8233)))
;; (define-rule ns-esc-8-bit (progn #**"x" ; TODO : write down the correct reader-macro for this
;; 				 (code-char (parse-integer (text (** ns-hex-digit :exactly 2))
;; 							   :radix 16))))
;; (define-rule ns-esc-16-bit
;;     (progn #**"u"
;; 	   (code-char (parse-integer (text (** ns-hex-digit :exactly 4))
;; 				     :radix 16))))
;; (define-rule ns-esc-32-bit (progn #**"U"
;; 				  (code-char (parse-integer (text (** ns-hex-digit :exactly 8)) :radix 16))))
    

;; (define-rule c-ns-esc-char (progn #/\
;; 			     (|| ns-esc-null
;; 				 ns-esc-bell
;; 				 ns-esc-backspace
;; 				 ns-esc-horizontal-tab
;; 				 ns-esc-line-feed
;; 				 ns-esc-vertical-tab
;; 				 ns-esc-form-feed
;; 				 ns-esc-carriage-return
;; 				 ns-esc-escape
;; 				 ns-esc-space
;; 				 ns-esc-double-quote
;; 				 ns-esc-slash
;; 				 ns-esc-backslash
;; 				 ns-esc-next-line
;; 				 ns-esc-non-breaking-space
;; 				 ns-esc-line-separator
;; 				 ns-esc-paragraph-separator
;; 				 ns-esc-8-bit
;; 				 ns-esc-16-bit
;; 				 ns-esc-32-bit)))

;; ;; Indentation

;; ;; TODO: something to substitute COND, but not conflicting with CL's cond

;; (define-rule s-indent-<n (length (|| (progn autodetect-indent-style (** s-space))
;; 				     (progn determined-indent-style (** s-space :upto (- n 1))))))
;; (define-rule s-indent-<=n (length (|| (progn autodetect-indent-style (** s-space))
;; 				      (progn determined-indent-style (** s-space :upto n)))))
;; (define-rule s-indent-=n (length (|| (progn autodetect-indent-style (** s-space :from (+ n 1)))
;; 				     (progn determined-indent-style (** s-space :exactly n)))))

;; ; TODO: write preceded by and start-of-file macros
;; (define-rule start-of-line (|| (<- sof) (<- b-char)))

;; (define-rule s-separate-in-line (|| (++ s-white) start-of-line))

;; (define-rule s-block-line-prefix s-indent-=n)
;; ; TODO: write optional macro
;; (define-rule s-flow-line-prefix (list s-indent-=n (? s-separate-in-line)))
;; (define-rule s-line-prefix (|| (progn (|| block-out-context block-in-context) s-block-line-prefix)
;; 			       (progn (|| flow-out-context flow-in-context) s-flow-line-prefix)))

;; (define-rule l-empty (|| (progn (or s-line-prefix s-indent-<n) b-break)))

;; (define-rule b-l-trimmed (progn b-break
;; 				(make-string (length (+ l-empty))
;; 					     :initial-element #\newline)))

;; (define-rule b-as-space (progn b-break (! l-empty) " "))

;; (define-rule b-l-folded (|| b-l-trimmed b-as-space))
;; (define-context-forcing-rule flow-in b-l-folded)

;; (define-rule s-flow-folded (progn (? s-separate-in-line)
;; 				  (prog1 flow-in-b-l-folded
;; 				    s-flow-line-prefix)))

;; ;; comments

;; (define-rule c-nb-comment-text (progn #/# (** nb-char) nil))

;; (define-rule b-comment (|| (-> eof)  b-char))

;; (define-rule s-b-comment (progn (? (progn s-separate-in-line
;; 					  (? c-nb-comment-text)))
;; 				b-comment
;; 				nil))

;; (define-rule l-comment (progn s-separate-in-line (? c-nb-comment-text) b-comment nil))

;; (define-rule s-l-comments (progn (|| s-b-comment start-of-line) (** l-comment) nil))

;; ;; separation lines

;; (define-rule s-separate-lines (progn (|| (progn s-l-comments s-flow-line-prefix)
;; 					 s-separate-in-line)
;; 				     " "))

;; ;; TODO: write definition of COND-PARSE
;; (define-rule s-separate (cond-parse ((|| block-out-context
;; 					 block-in-context
;; 					 flow-out-context
;; 					 flow-in-context) s-separate-lines)
;; 				    ((|| block-key-context flow-key-context) s-separate-in-line)))

;; ;; Directives (finally, something non-trivial)

;; (define-rule l-directive (progn #/% (prog1 (|| ns-yaml-directive
;; 					       ns-tag-directive
;; 					       ns-reserved-directive)
;; 				      s-l-comments)))

;; (define-rule ns-reserved-directive (list ns-directive-name (** (progn s-separate-in-line ns-directive-parameter))))
;; (define-rule ns-directive-name (++ ns-char))
;; (define-rule ns-directive-parameter (++ ns-char))

;; ;; YAML directive
;; ;;; Lets hack logic of %YAML directive, since this is simple enough for me to understand now.

;; (defparameter yaml-version nil)
;; (define-rule ns-yaml-directive (let ((version (progn "YAML" s-separate-in-line ns-yaml-version)))
;; 				 (if yaml-version
;; 				     (fail-parse "The YAML directive must be only given once per document.")
;; 				     (if (not (equal (car version) 1))
;; 					 (fail-parse "Major version ~a differs from processor's version." (car version))
;; 					 (progn (if (> (cadr version) 2)
;; 						    (warn-parse "Minor version is greater than that of the processor, attempt to parse anyway."))
;; 						(setf yaml-version version)
;; 						nil)))))
			    

;; (define-rule ns-yaml-version (destructuring-bind (major pt minor) (list (+ ns-dec-digit) #\. (+ ns-dec-digit))
;; 			       (declare (ignore pt))
;; 			       `(,(parse-integer (text major)) ,(parse-integer (text minor)))))

;; ;; TAG directive
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defparameter tag-handles (make-hash-table :test #'equal))
;;   (defparameter default-yaml-tagspace "tag:yaml.org,2002:")
;;   (defparameter default-local-tagspace "!")
;;   (setf (gethash :secondary-tag-handle tag-handles) default-yaml-tagspace
;; 	(gethash :primary-tag-handle tag-handles) default-local-tagspace))



;; (define-rule ns-tag-directive (destructuring-bind (tag sep handle sep1 prefix)
;; 				  (list "TAG" s-separate-in-line c-tag-handle s-separate-in-line ns-tag-prefix)
;; 				(declare (ignore tag sep sep1))
;; 				(if (gethash handle tag-handles)
;; 				    (fail-parse "The TAG directive must be given at most once per handle in the same document.")
;; 				    (setf (gethash handle tag-handles) prefix)
;; 				    ;; TODO: should I write NIL here, or I must emit a tag-prefix instead?
;; 				    nil)))

;; (define-rule c-tag-handle (|| c-named-tag-handle
;; 			      c-secondary-tag-handle
;; 			      c-primary-tag-handle))
;; (define-rule c-primary-tag-handle (progn #/! :primary-tag-handle))
;; (define-rule c-secondary-tag-handle (progn #**"!!" :secondary-tag-handle))
;; (define-rule c-named-tag-handle `(:named-tag-handle ,(text (progn #/! (prog1 (+ ns-word-char) #/!)))))
		

;; (define-rule ns-tag-prefix (text (|| c-ns-local-tag-prefix
;; 				     ns-global-tag-prefix)))
;; (define-rule c-ns-local-tag-prefix (list #\! (** ns-uri-char)))
;; (define-rule ns-global-tag-prefix (list ns-tag-char (** ns-uri-char)))
  
;; ;;; Node properties

;; (define-rule c-ns-properties
;;     `(:properties ,@(remove-if-not #'identity
;; 				   (|| (list c-ns-tag-property (? (progn s-separate
;; 									 c-ns-anchor-property)))
;; 				       (list c-ns-anchor-property (? (progn s-separate
;; 									    c-ns-tag-property)))))))

;; (define-rule c-ns-tag-property (|| c-verbatim-tag c-ns-shorthand-tag c-non-specific-tag))
;; (define-rule c-verbatim-tag `(:tag . ,(text (progn #**"!<" (prog1 (++ ns-uri-char) #**">")))))

;; (defun resolve-handle (handle)
;;   (or (gethash handle tag-handles)
;;       (fail-parse "Unknown handle ~a. Did you forget to declare it?" handle)))
		
;; (define-rule c-ns-shorthand-tag (destructuring-bind (handle meat) (list c-tag-handle (++ ns-tag-char))
;; 				  `(:tag . ,(text (resolve-handle handle) meat))))

;; (define-rule c-non-specific-tag `(:tag .  ,(progn #**"!" :vanilla)))

;; (define-rule c-ns-anchor-property `(:anchor . ,(text (progn #/& ns-anchor-name))))
;; (define-rule ns-anchor-char (progn (! c-flow-indicator) ns-char))
;; (define-rule ns-anchor-name (++ ns-anchor-char))

;; ;;; alias nodes
;; (define-rule c-ns-alias-node `(:alias . ,(text (progn #/* ns-anchor-name))))

;; ;;; empty node
;; (define-rule e-scalar :empty)
;; (define-rule e-node `((:properties (:tag . :non-specific)) (:content . ,e-scalar)))
    
;; ;;; double-quoted-scalars

;; (define-rule nb-double-char (|| c-ns-esc-char (progn (! #\\) (! #\") nb-json)))
;; (define-rule ns-double-char (progn (! s-white) nb-double-char))


;; (define-rule c-double-quoted (text (progm #/" nb-double-text #/")))
;; (define-rule nb-double-text (cond-parse ((|| block-key-context flow-key-context) nb-double-one-line)
;; 					((|| block-out-context
;; 					     block-in-context
;; 					     flow-out-context
;; 					     flow-in-context) nb-double-multi-line)))

;; (define-rule nb-double-one-line (** nb-double-char))

;; (define-context-forcing-rule flow-in l-empty)

;; (define-rule s-double-escaped
;;     (destructuring-bind (white slash bnc empties pref)
;; 	(list (** s-white) #/\ b-non-content
;; 	      (** flow-in-l-empty) s-flow-line-prefix)
;;       (declare (ignore slash bnc))
;;       `(,white ,empties ,(mapcar (lambda (x) (if (numberp x) (make-string x :initial-element #\space) x))
;; 				 pref))))
;; (define-rule s-double-break (|| s-double-escaped s-flow-folded))
;; (define-rule nb-ns-double-in-line (** (list (** s-white) ns-double-char)))
;; (define-rule s-double-next-line (list s-double-break
;; 				      (? (list ns-double-char
;; 					       nb-ns-double-in-line
;; 					       (|| s-double-next-line (** s-white))))))

;; (define-rule nb-double-multi-line (list nb-ns-double-in-line
;; 					(|| s-double-next-line (** s-white))))

;; (defun whitespace-p (text)
;;   (iter (for char in-string text)
;; 	(if (not (member char '(#\tab #\space)))
;; 	    (return nil))
;; 	(finally (return t))))


;; ;;; single-quoted-scalars
;; (define-rule c-quoted-quote (progn #**"''" #\'))
;; (define-rule nb-single-char (|| c-quoted-quote (progn (! #\') nb-json)))
;; (define-rule ns-single-char (progn (! s-white) nb-single-char))

;; (define-rule c-single-quoted (text (progm #/' nb-single-text #/')))
;; (define-rule nb-single-text (cond-parse ((|| block-key-context flow-key-context) nb-single-one-line)
;; 					((|| block-out-context
;; 					     block-in-context
;; 					     flow-out-context
;; 					     flow-in-context) nb-single-multi-line)))

;; (define-rule nb-single-one-line (** nb-single-char))

;; (define-rule nb-ns-single-in-line (** (list (** s-white) ns-single-char)))
;; (define-rule s-single-next-line (list s-flow-folded
;; 				      (? (list ns-single-char
;; 					       nb-ns-single-in-line
;; 					       (|| s-single-next-line (** s-white))))))
;; (define-rule nb-single-multi-line (list nb-ns-single-in-line
;; 					(|| s-single-next-line
;; 					    (** s-white))))

;; ;; Block scalars

;; (define-rule c-b-block-header (prog1 (|| (list c-indentation-indicator-ne c-chomping-indicator)
;; 					 (list c-chomping-indicator-ne c-indentation-indicator)
;; 					 (list c-chomping-indicator c-indentation-indicator))
;; 				s-b-comment))

;; (define-rule c-indentation-indicator-ne `(:block-indentation-indicator . ,(string ns-dec-digit)))
;; (define-rule c-indentation-indicator `(:block-indentation-indicator . ,(string (|| ns-dec-digit #**""))))
;; (define-rule c-chomping-indicator-ne `(:block-chomping-indicator . ,(string (|| #\- #\+))))
;; (define-rule c-chomping-indicator `(:block-chomping-indicator . ,(string (|| #\- #\+ ""))))

;; (defun hash->assoc (hash)
;;   (iter (for (key val) in-hashtable hash)
;; 	(collect `(,key . ,val))))

;; (define-rule b-chomped-last (progn b-break
;; 				   (case block-scalar-chomping
;; 				     (:clip #\newline)
;; 				     (:keep #\newline)
;; 				     (:strip nil))))

;; (define-rule b-non-content (progn b-break nil))
;; (define-rule b-as-line-feed b-break)

;; (define-rule l-chomped-empty (cond-parse ((|| strip-block-scalar-chomping
;; 					      clip-block-scalar-chomping) l-strip-empty)
;; 					 (keep-block-scalar-chomping l-keep-empty)))
;; (define-rule l-strip-empty (progn (** (progn s-indent-<=n b-non-content))
;; 				  (? l-trail-comments)
;; 				  ""))
;; (define-rule l-keep-empty (list (** l-empty)
;; 				(? l-trail-comments)))
;; (define-rule l-trail-comments (progn s-indent-<n c-nb-comment-text b-comment
;; 				     (** l-comment)
;; 				     ""))

;; (define-rule l-literal-content (text (? (list l-nb-literal-text
;; 					      (** b-nb-literal-next)
;; 					      b-chomped-last))
;; 				     l-chomped-empty))

;; (let ((chomping-map '(("+" . :keep) ("-" . :strip) ("" . :clip)))
;;       (style-map '(("|" . :literal) (">" . :folded))))
;;   (define-rule c-l-block-scalar
;;       (macrolet ((call-parser ()
;; 		   (text block-scalar-content)))
;; 	(let ((header (progn (|| "|" ">") c-b-block-header)))
;; 	  (let ((block-scalar-chomping (cdr (assoc (cdr (assoc :block-chomping-indicator
;; 							       (cadr header)))
;; 						   chomping-map :test #'equal)))
;; 		(block-scalar-style (cdr (assoc (car header) style-map :test #'equal))))
;; 	    (let ((it (cdr (assoc :block-indentation-indicator
;; 				  (cadr header)))))
;; 	      (if (not (equal it ""))
;; 		  (let ((n (+ n (parse-integer it)))
;; 			(indent-style :determined))
;; 		    (call-parser))
;; 		  (let ((indent-style :autodetect))
;; 		    (call-parser)))))))))

;; (define-rule block-scalar-content
;;     (let ((wrapper (cond-parse (autodetect-indent-style detect-indent)
;; 			       (t ""))))
;;       (macrolet ((call-parser ()
;; 		   (cond-parse (literal-block-scalar-style l-literal-content)
;; 			       (folded-block-scalar-style l-folded-content))))
;; 	(if (equal wrapper "")
;; 	    (call-parser)
;; 	    (let ((n wrapper)
;; 		  (indent-style :determined))
;; 	      (call-parser))))))

;; (define-rule detect-indent (& (progm (** l-empty) s-indent-=n nb-char)))

;; (define-rule l-nb-literal-text (list (** l-empty)
;; 				     (progn s-indent-=n (++ nb-char))))

;; (define-rule b-nb-literal-next (list b-as-line-feed l-nb-literal-text))
				    

;; (define-rule s-nb-folded-text (progn s-indent-=n (list ns-char (* nb-char))))
;; (define-rule l-nb-folded-lines (list s-nb-folded-text
;; 				     (** (list b-l-folded s-nb-folded-text))))

;; (define-rule s-nb-spaced-text (progn s-indent-=n (list s-white (** nb-char))))
;; (define-rule b-l-spaced (list b-as-line-feed (** l-empty)))
;; (define-rule l-nb-spaced-lines (list s-nb-spaced-text
;; 				     (** (and b-l-spaced s-nb-spaced-text))))
;; (define-rule l-nb-same-lines (list (** l-empty)
;; 				   (|| l-nb-folded-lines l-nb-spaced-lines)))
;; (define-rule l-nb-diff-lines (list l-nb-same-lines
;; 				   (** (list b-as-line-feed l-nb-same-lines))))
;; (define-rule l-folded-content (list (? (list l-nb-diff-lines b-chomped-last))
;; 				    l-chomped-empty))

;; ;;; Plain scalars

;; (define-rule ns-plain-first (|| (progn (! c-indicator) ns-char)
;; 				(prog1 (|| #\? #\: #\-) (& ns-plain-safe))))

;; (define-rule ns-plain-safe (cond-parse ((|| flow-out-context
;; 					    block-key-context
;; 					    block-in-context
;; 					    block-out-context) ns-plain-safe-out)
;; 				       ((|| flow-in-context flow-key-context) ns-plain-safe-in)))
;; (define-rule ns-plain-safe-out ns-char)
;; (define-rule ns-plain-safe-in (progn (! c-flow-indicator) ns-char))
;; (define-rule ns-plain-char (cond-parse ((list (! #/:) (! #/#)) ns-plain-safe)
;; 				       ((<- ns-char) #/#)
;; 				       (t (prog1 #\: (-> ns-plain-safe)))
  
;; (define-rule ns-plain (cond-parse ((|| flow-out-context
;; 				       flow-in-context
;; 				       block-out-context
;; 				       block-in-context) ns-plain-multi-line)
;; 				  ((|| block-key-context flow-key-context) ns-plain-one-line)))

;; (define-rule nb-ns-plain-in-line (** (list (** s-white) ns-plain-char)))
;; (define-rule ns-plain-one-line (text ns-plain-first nb-ns-plain-in-line))

;; (define-rule s-ns-plain-next-line (list s-flow-folded ns-plain-char nb-ns-plain-in-line))
;; (define-rule ns-plain-multi-line (text ns-plain-one-line (** s-ns-plain-next-line)))

;; ;; block sequences

;; (define-rule l+block-sequence
;;   (let ((n detect-block-sequence)
;;         (indent-style :determined)
;;         (context :block-in))
;;     (+ (cond (s-indent-=n c-l-block-seq-entry)))))

;; (define-rule detect-block-sequence (let ((indent-style :autodetect))
;;                                      (& (prog1 s-indent-=n (& (list #\- (! ns-char)))))))
;;   (:wrap-around 
;; 		  (call-parser))))

;; (define-rule detect-block-mapping (let ((indent-style :autodetect))
;;                                     (& (prog1 s-indent-=n ns-char))))

;; (define-rule c-l-block-seq-entry (progn "-" (! ns-char) s-l+block-indented))

;; (define-rule s-l+block-indented (|| compact-block-node
;; 				    s-l+block-node
;; 				    (prog1 e-node s-l-comments)))

;; (define-rule compact-block-node (let ((n (+ n 1 (length (** s-space :from 1))))
;;                                       (indent-style :determined))                                      
;;                                   `((:properties (:tag . :non-specific))
;;                                     (:content . ,(|| ns-l-compact-sequence ns-l-compact-mapping))))
		  
;; (define-rule ns-l-compact-sequence `(,c-l-block-seq-entry
;;                                      ,.(** (progn s-indent-=n c-l-block-seq-entry))))

;; (define-rule l+block-mapping (let ((n detect-block-mapping)
;;                                    (indent-style :determined))
;;                                `(:mapping ,.(++ (progn s-indent-=n ns-l-block-map-entry)))))

;; (define-rule ns-l-block-map-entry
;;     (destructuring-bind (key value) (|| c-l-block-map-explicit-entry
;;                                         ns-l-block-map-implicit-entry)
;;       `(,key . ,value)))
;; (define-rule c-l-block-map-explicit-entry (list c-l-block-map-explicit-key
;;                                                 (|| l-block-map-explicit-value
;;                                                     e-node)))


;; (define-rule c-l-block-map-explicit-key (let ((context :block-out))
;;                                           (progn #\? s-l+block-indented)))
;; (define-rule l-block-map-explicit-value (let ((context :block-out))
;;                                           (progn s-indent-=n ":" s-l+block-indented)))

;; (define-rule ns-l-block-map-implicit-entry (list (|| ns-s-block-map-implicit-key
;;                                                      e-node)
;;                                                  c-l-block-map-implicit-value))
;; (define-rule ns-s-block-map-implicit-key (let ((context :block-key))
;;                                            (|| c-s-implicit-json-key
;;                                                ns-s-implicit-yaml-key)))
;; (define-rule c-l-block-map-implicit-value (progn #\:
;;                                                  (let ((context :block-out))
;;                                                    (|| s-l+block-node
;;                                                        (prog1 e-node s-l-comments)))))
  
;; (define-rule ns-l-compact-mapping
;;     `(:mapping ,ns-l-block-map-entry ,.(** (progn s-indent-=n ns-l-block-map-entry))))

;; ;; block nodes

;; (define-rule s-l+block-node (|| s-l+block-in-block s-l+flow-in-block))
;; (define-rule s-l+flow-in-block (let ((context :flow-out)
;;                                      (n (1+ n)))
;;                                  (progm s-separate ns-flow-node s-l-comments)))

;; (define-rule s-l+block-in-block (|| s-l+block-scalar s-l+block-collection))

;; (define-rule s-separate-n+1 (let ((n (1+ n)))
;;                               s-separate))

;; (defparameter vanilla-scalar-tag "tag:yaml.org,2002:str")
;; (defparameter vanilla-mapping-tag "tag:yaml.org,2002:map")
;; (defparameter vanilla-sequence-tag "tag:yaml.org,2002:seq")

;; (define-rule s-l+block-scalar (progn s-separate-n+1
;;                                      (let ((props block-node-properties)
;;                                            (content c-l-block-scalar))
;;                                        (crunch-tag-into-properties props vanilla-scalar-tag vanilla-scalar-tag)
;;                                        `(,props (:content . ,content)))))

;; (define-rule block-node-properties (let ((n (1+ n)))
;;                                      (? (progn s-separate c-ns-properties))))

;; (defun seq-spaces (n)
;;   (if (eql context :block-out)
;;       (1- n)
;;       n))

;; (define-rule s-l+block-collection (let ((props block-node-properties))
;;                                     (progn s-l-comments
;;                                            (let ((content (|| l+block-sequence-seq-spaces
;;                                                               l+block-mapping)))
;;                                              (crunch-tag-into-properties props :non-specific
;;                                                                          (case (car content)
;;                                                                            (:mapping vanilla-mapping-tag)
;;                                                                            (t vanilla-sequence-tag)))
;;                                              `(,props (:content . ,content))))))


;; (define-rule l+block-sequence-seq-spaces (let ((n (seq-spaces n)))
;;                                            l+block-sequence))

;; ;; flow collections

;; (defun in-flow (context)
;;   (case context
;;     ((:block-in :block-out :flow-in :flow-out) :flow-in)
;;     ((:block-key :flow-key) :flow-key)))

;; (define-rule c-flow-sequence (progn #\[ (? s-separate)
;;                                     (let ((context (in-flow context)))
;;                                       (prog1 (? ns-s-flow-seq-entries) #\]))))

;; (define-rule ns-s-flow-seq-entries
;;     (let ((entry ns-flow-seq-entry))
;;       (? s-separate)
;;       (let ((entries (? (progn #\, (? s-separate) (? ns-s-flow-seq-entries)))))
;;         `(,entry ,. entries))))

;; (define-rule ns-flow-seq-entry (|| ns-flow-pair ns-flow-node))

;; (define-rule c-flow-mapping (progn #\{ (? s-separate)
;;                                    (let ((context (in-flow context)))                   
;;                                      `(:mapping ,. (prog1 (? ns-s-flow-map-entries) #\})))))

;; (define-rule ns-s-flow-map-entries
;;     (let ((entry ns-flow-map-entry))
;;       (? s-separate)
;;       (let ((entries (? (progn #\, (? s-separate) (? ns-s-flow-map-entries)))))
;;         `(,entry ,. entries))))

;; (define-rule ns-flow-map-entry
;;     (destructuring-bind (key value) (|| (progn #\? s-separate ns-flow-map-explicit-entry)
;;                                         ns-flow-map-implicit-entry)
;;       `(,key . ,value)))
;; (define-rule ns-flow-map-explicit-entry (|| ns-flow-map-implicit-entry
;; 					    (list e-node e-node)))

;; (define-rule ns-flow-map-implicit-entry (|| ns-flow-map-yaml-key-entry
;; 					    c-ns-flow-map-empty-key-entry
;; 					    c-ns-flow-map-json-key-entry))
;; (define-rule ns-flow-map-yaml-key-entry
;;     (list ns-flow-yaml-node
;;           (|| (progn (? s-separate) c-ns-flow-map-separate-value)
;;               e-node)))
;; (define-rule c-ns-flow-map-empty-key-entry (list e-node c-ns-flow-map-separate-value))

;; (define-rule c-ns-flow-map-separate-value
;;     (progn #\: (! ns-plain-safe)
;;            (|| (progn s-separate ns-flow-node)
;;                e-node)))

;; (define-rule c-ns-flow-map-json-key-entry (list c-flow-json-node
;;                                                 (|| (progn (? s-separate) c-ns-flow-map-adjacent-value)
;;                                                     e-node)))
;; (define-rule c-ns-flow-map-adjacent-value (progn #\: (|| (progn (? s-separate) ns-flow-node)
;;                                                          e-node)))

;; (define-rule ns-flow-pair 
;;   (destructuring-bind (key value) (|| (progn #\? s-separate ns-flow-map-explicit-entry)
;;                                       ns-flow-pair-entry)
;;     `(:mapping (,key . ,value))))

;; (define-rule ns-flow-pair-entry (|| ns-flow-pair-yaml-key-entry
;; 				    c-ns-flow-map-empty-key-entry
;; 				    c-ns-flow-pair-json-key-entry))


;; (define-context-forcing-rule flow-key ns-s-implicit-yaml-key)
;; (define-context-forcing-rule flow-key c-s-implicit-json-key)

;; (define-rule ns-flow-pair-yaml-key-entry (list flow-key-ns-s-implicit-yaml-key
;;                                                c-ns-flow-map-separate-value))

;; (define-rule c-ns-flow-pair-json-key-entry (list flow-key-c-s-implicit-json-key
;;                                                  c-ns-flow-map-adjacent-value))

;; ;; FIXME: implement restriction on the length of the key
;; ;; FIXME: implement n/a in the indentation portion
;; (define-rule ns-s-implicit-yaml-key (prog1 ns-flow-yaml-node (? s-separate-in-line)))
;; (define-rule c-s-implicit-json-key (prog1 c-flow-json-node (? s-separate-in-line)))

;; (define-rule ns-flow-yaml-content ns-plain)
;; (define-rule c-flow-json-content (|| c-flow-sequence c-flow-mapping c-single-quoted c-double-quoted))
;; (define-rule ns-flow-content (|| (list :yaml ns-flow-yaml-content)
;; 				 (list :json c-flow-json-content)))


;; (defmacro with-ensured-properties-not-alias (var &body body)
;;   `(if (alias-p ,var)
;;        ,var
;;        (let ((,var (cond ((property-node-p ,var) ,var)
;; 			 (t `(,(list :properties) (:content . ,,var))))))
;; 	 ,@body)))

;; (define-rule ns-flow-yaml-node (let ((node (|| c-ns-alias-node
;;                                                ns-flow-yaml-content
;;                                                ns-flow-yaml-properties-node)))
;;                                  (with-ensured-properties-not-alias node
;;                                    (let ((props (assoc :properties node))
;;                                          (content (cdr (assoc :content node))))
;;                                      (crunch-tag-into-properties props :non-specific vanilla-scalar-tag)
;;                                      `(,props (:content . ,content))))))
				    
;; (define-rule ns-flow-yaml-properties-node
;;     `(,c-ns-properties (:content . ,(|| (progn s-separate ns-flow-yaml-content)
;;                                         e-scalar))))

;; (define-rule c-flow-json-node
;;     (let ((props (? (prog1 c-ns-properties s-separate)))
;;           (content c-flow-json-content))
;;       (if (atom content)
;;           (crunch-tag-into-properties props vanilla-scalar-tag vanilla-scalar-tag)
;;           (crunch-tag-into-properties props :non-specific (if (equal (car content) :mapping)
;;                                                               vanilla-mapping-tag
;;                                                               vanilla-sequence-tag)))
;;       `(,props (:content . ,content))))

;; (define-rule ns-flow-node (let ((node (|| c-ns-alias-node
;;                                           ns-flow-content
;;                                           ns-flow-properties-node)))
;;                             (with-ensured-properties-not-alias node
;;                               (let ((props (assoc :properties node))
;;                                     (content (cdr (assoc :content node))))
;;                                 (destructuring-bind (content-type content) content
;;                                   (crunch-tag-into-properties props
;;                                                               (cond ((eql content-type :yaml) :non-specific)
;;                                                                     ((eql content-type :json)
;;                                                                      (cond ((atom content) vanilla-scalar-tag)
;;                                                                            ((eql (car content) :mapping) :non-specific)
;;                                                                            (t :non-specific))))
;;                                                               (cond ((atom content) vanilla-scalar-tag)
;;                                                                     ((eql (car content) :mapping) vanilla-mapping-tag)
;;                                                                     (t vanilla-sequence-tag)))
;;                                   `(,props (:content . ,content)))))))

;; (define-rule ns-flow-properties-node `(,c-ns-properties
;;                                        (:content . ,(|| (progn s-separate ns-flow-content)
;;                                                         (tag :yaml e-scalar)))))

;; ;;; YaML documents

;; ;; FIXME: correct definition of byte-order-mark
;; (define-rule c-byte-order-mark (progn #\UEFBB #\INVERTED_QUESTION_MARK :utf8-bom))

;; (define-rule l-document-prefix (list (? c-byte-order-mark) (* l-comment)))
;; (define-rule c-directives-end (** #\- :exactly 3))
;; (define-rule c-document-end (** #\. :exactly 3))
;; (define-rule l-document-suffix (list c-document-end s-l-comments))

;; (define-rule c-forbidden (list start-of-line
;;                                (|| c-directives-end c-document-end)
;;                                (|| b-char s-white (-> eof))))

;; (define-rule l-bare-document (let ((text (** (list (! c-forbidden) (list (* nb-char) (? b-break))))))
;;                                (let ((n -1)
;;                                      (indent-style :determined)
;;                                      (context :block-in))
;;                                  `(:document ,(yaclyaml-parse 's-l+block-node (text text))))))

;; (define-rule l-explicit-document
;;     (let ((text (progn c-directives-end (|| l-bare-document
;;                                             (prog1 e-node s-l-comments)))))
;;       `(:document ,(if (and (consp text) (eql (car text) :document))
;;                        (cadr text)
;;                        text))))

;; (define-rule l-directive-document
;;     (let ((yaml-version nil)
;;           (tag-handles (make-hash-table :test #'equal)))
;;       (setf (gethash :secondary-tag-handle tag-handles) default-yaml-tagspace
;;             (gethash :primary-tag-handle tag-handles) default-local-tagspace)
;;       %l-directive-document))

;; (let (path)
;;   (declare (special path))
;;   (defun try-resolve (handle)
;;     (multiple-value-bind (prefix position)
;; 	(yaclyaml-parse 'c-tag-handle handle :junk-allowed t)
;;       ;; (format t "~a ~a ~a~%" handle prefix position)
;;       (if prefix
;; 	  (let ((pos (position prefix path :test #'equal)))
;; 	    (if (and pos position)
;; 		(error "Loop in prefixes resolution: ~a~%" `(,@(subseq path pos) ,prefix))
;; 		(strcat (gethash (if (atom prefix)
;; 				     prefix
;; 				     (cadr prefix))
;; 				 tag-handles)
;; 			(if position
;; 			    (subseq handle position)))))
;; 	  handle)))

;;   (defun compile-tag-handles ()
;;     (iter (for (key . nil) in (hash->assoc tag-handles))
;; 	  (let ((path `(,key)))
;; 	    (declare (special path))
;; 	    (iter (while t)
;; 		  (let ((val (gethash key tag-handles)))
;; 		    (let ((try-val (try-resolve val)))
;; 		      (if (equal try-val val)
;; 			  (terminate)
;; 			  (setf (gethash key tag-handles) try-val
;; 				val try-val)))))))))

;; (define-rule %l-directive-document
;;     (progn (+ l-directive)
;;            (compile-tag-handles)
;;            l-explicit-document))

;; (define-rule l-any-document (|| l-directive-document
;; 				l-explicit-document
;; 				l-bare-document))

;; (define-rule l-yaml-stream
;;     (let ((first (progn (* l-document-prefix) (? l-any-document)))
;;           (rest (** (|| (progn (+ l-document-suffix) (* l-document-prefix) (? l-any-document))
;;                         (progn (* l-document-prefix)) (? l-explicit-document)))))
;;       `(,first ,.(if (and rest (car rest)) rest (list)))))
