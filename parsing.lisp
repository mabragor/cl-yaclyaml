;;;; This file is one of components of CL-YACLYAML system, licenced under GPL, see COPYING for details

(in-package #:cl-yaclyaml)

(enable-read-macro-tokens)

;;; Parsing presentation stream

(register-yaclyaml-context n nil)
(setf n 0)
(register-yaclyaml-context indent-style determined autodetect)
(register-yaclyaml-context context block-out block-in flow-out flow-in block-key flow-key)
(register-yaclyaml-context block-scalar-chomping clip keep strip)
(register-yaclyaml-context block-scalar-style literal folded)

;;; Indicator characters

;; block structure indicators
(define-alias-rule c-sequence-entry #\-)
(define-alias-rule c-mapping-key #\?)
(define-alias-rule c-mapping-value #\:)

;; flow collection indicators
(define-alias-rule c-colon-entry #\,)
(define-alias-rule c-sequence-start #\[)
(define-alias-rule c-sequence-end #\])
(define-alias-rule c-mapping-start #\{)
(define-alias-rule c-mapping-end #\})

;; comments
(define-alias-rule c-comment #\#)
;; tags and aliases
(define-alias-rule c-anchor #\&)
(define-alias-rule c-alias #\*)
(define-alias-rule c-tag #\!)
;; block-scalar style
(define-alias-rule c-literal #\|)
(define-alias-rule c-folded #\>)
;; quoted scalars
(define-alias-rule c-single-quote #\')
(define-alias-rule c-double-quote #\")
(define-alias-rule c-directive #\%)
(define-alias-rule c-reserved (|| #\@ #\`))

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

(define-yaclyaml-rule c-printable ()
  (pred #'c-printable-p character))
(defun nb-json-p (char)
  (let ((code (char-code char)))
    (or (equal code 9)
	(and (>= code #x20) (<= code #x10ffff)))))
(define-yaclyaml-rule nb-json ()
  (pred #'nb-json-p character))

(define-yaclyaml-rule c-indicator ()
  (|| c-sequence-entry c-mapping-key c-mapping-value
      c-colon-entry c-sequence-start c-sequence-end c-mapping-start c-mapping-end
      c-comment
      c-anchor c-alias c-tag
      c-literal c-folded
      c-single-quote c-double-quote
      c-directive
      c-reserved))

(define-yaclyaml-rule c-flow-indicator ()
  (|| c-colon-entry
      c-sequence-start c-sequence-end
      c-mapping-start c-mapping-end))

(define-yaclyaml-rule b-line-feed ()
  #\newline)
(define-yaclyaml-rule b-carriage-return ()
  #\return)
(define-yaclyaml-rule b-char ()
  (|| b-line-feed b-carriage-return))

(define-yaclyaml-rule b-break ()
  (|| (list b-carriage-return b-line-feed)
      b-carriage-return
      b-line-feed)
  (literal-char #\newline))

;; ;; TODO - include BOM
(define-yy-rule nb-char ()
  (! b-char)
  character)

(define-yy-rule s-space () #\space)
(define-yy-rule s-tab () #\tab)
(define-yy-rule s-white () (|| s-space s-tab))

(define-yy-rule ns-char ()
  (! s-white)
  nb-char)

;; ;; TODO: write a character-ranges macro
;; ;; TODO: write ~ (ignore-case macro)

(define-yy-rule ns-dec-digit ()
  (character-ranges (#\0 #\9)))

(define-yy-rule ns-hex-digit ()
  (|| ns-dec-digit
      ;; KLUDGE, until I write ignore-case macro
      #\a #\b #\c #\d #\e #\f
      #\A #\B #\C #\D #\E #\F))

(define-yy-rule ns-ascii-letter ()
  (pred #'alpha-char-p character))

(define-yy-rule ns-word-char ()
  (|| ns-dec-digit
      ns-ascii-letter
      #\-))

(define-yy-rule ns-uri-char ()
  (text (|| (list #\% ns-hex-digit ns-hex-digit)
	    ns-word-char
	    #\# #\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\,
	    #\_ #\. #\! #\~ #\* #\' #\( #\) #\[ #\])))
			     
(define-yy-rule ns-tag-char ()
  (! #\!)
  (! c-flow-indicator)
  ns-uri-char)

;; ;; Escaped characters

(define-yy-rule c-escape () #\\)

(define-yy-rule ns-esc-null () #\0 (code-char 0))
(define-yy-rule ns-esc-bell () #\a (code-char 7))
(define-yy-rule ns-esc-backspace () #\b (code-char 8))
(define-yy-rule ns-esc-horizontal-tab () #\t (code-char 9))
(define-yy-rule ns-esc-line-feed () #\n (code-char 10))
(define-yy-rule ns-esc-vertical-tab () #\v (code-char 11))
(define-yy-rule ns-esc-form-feed () #\f (code-char 12))
(define-yy-rule ns-esc-carriage-return () #\r (code-char 13))
(define-yy-rule ns-esc-escape () #\e (code-char 27))
(define-yy-rule ns-esc-space () #\space nil)
(define-yy-rule ns-esc-double-quote () #\") ; TODO: fix this sad notation flaw ")
(define-yy-rule ns-esc-slash () #\/)
(define-yy-rule ns-esc-backslash () #\\)
(define-yy-rule ns-esc-next-line () #\N (code-char 133))
(define-yy-rule ns-esc-non-breaking-space () #\_ (code-char 160))
(define-yy-rule ns-esc-line-separator () #\L (code-char 8232))
(define-yy-rule ns-esc-paragraph-separator () #\P (code-char 8233))
(define-yy-rule ns-esc-8-bit ()
  #\x
  (code-char (parse-integer (text (times ns-hex-digit :exactly 2))
			    :radix 16)))
(define-yy-rule ns-esc-16-bit ()
  #\u
  (code-char (parse-integer (text (times ns-hex-digit :exactly 4))
			    :radix 16)))
(define-yy-rule ns-esc-32-bit ()
  #\U
  (code-char (parse-integer (text (times ns-hex-digit :exactly 8))
			    :radix 16)))
    

(define-yy-rule c-ns-esc-char ()
  #\\
  (|| ns-esc-null
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

;; ;; Indentation

(define-yy-rule s-indent-<n ()
  (length (cond-parse (autodetect-indent-style (times s-space))
		      (determined-indent-style (times s-space :upto (- n 1))))))
(define-yy-rule s-indent-<=n ()
  (length (cond-parse (autodetect-indent-style (times s-space))
		      (determined-indent-style (times s-space :upto n)))))
(define-yy-rule s-indent-=n ()
  (length (cond-parse (autodetect-indent-style (times s-space :from (+ n 1)))
		      (determined-indent-style (times s-space :exactly n)))))

(define-yy-rule start-of-line ()
  (|| (<- sof) (<- b-char)))

(define-yy-rule s-separate-in-line ()
  (|| (postimes s-white)
      start-of-line))

(define-yy-rule s-block-line-prefix () s-indent-=n)
(define-yy-rule s-flow-line-prefix ()
  (list s-indent-=n
	(? s-separate-in-line)))
(define-yy-rule s-line-prefix ()
  (cond-parse ((|| block-out-context block-in-context) s-block-line-prefix)
	      ((|| flow-out-context flow-in-context) s-flow-line-prefix)))

(define-yy-rule l-empty ()
  (|| s-line-prefix s-indent-<n)
  b-break)

(define-yy-rule b-l-trimmed ()
  b-break
  (make-string (length (postimes l-empty))
	       :initial-element (literal-char #\newline)))

(define-yy-rule b-as-space ()
  b-break
  (! l-empty)
  (literal-string " "))

(define-yy-rule b-l-folded ()
  (|| b-l-trimmed b-as-space))
(define-context-forcing-rule flow-in b-l-folded)

(define-yy-rule s-flow-folded ()
  (? s-separate-in-line)
  (prog1 flow-in-b-l-folded
    s-flow-line-prefix))

;; comments

(define-yy-rule c-nb-comment-text ()
  #\#
  (times nb-char)
  nil)

(define-yy-rule b-comment () (|| (-> eof)  b-char))

(define-yy-rule s-b-comment ()
  (? (progn s-separate-in-line
	    (? c-nb-comment-text)))
  b-comment
  nil)

(define-yy-rule l-comment ()
  s-separate-in-line
  (? c-nb-comment-text)
  b-comment
  nil)

(define-yy-rule s-l-comments ()
  (|| s-b-comment start-of-line)
  (times l-comment)
  nil)

;; ;; separation lines

(define-yy-rule s-separate-lines ()
  (|| (progn s-l-comments
	     s-flow-line-prefix)
      s-separate-in-line)
  (literal-string " "))

(define-yy-rule s-separate ()
  (cond-parse ((|| block-out-context
		   block-in-context
		   flow-out-context
		   flow-in-context) s-separate-lines)
	      ((|| block-key-context flow-key-context) s-separate-in-line)))

;; ;; Directives (finally, something non-trivial)

(define-yy-rule l-directive ()
  #\%
  (prog1 (|| ns-yaml-directive
	     ns-tag-directive
	     ns-reserved-directive)
    s-l-comments))

(define-yy-rule ns-reserved-directive ()
  (list ns-directive-name (times (progn s-separate-in-line
					ns-directive-parameter))))
(define-yy-rule ns-directive-name () (postimes ns-char))
(define-yy-rule ns-directive-parameter () (postimes ns-char))

;; YAML directive
;;; Lets hack logic of %YAML directive, since this is simple enough for me to understand now.

(defparameter yaml-version nil)
(define-yy-rule ns-yaml-directive ()
  (let ((version (progn "YAML" s-separate-in-line ns-yaml-version)))
    (if yaml-version
	(fail-parse "The YAML directive must be only given once per document.")
	(if (not (equal (car version) 1))
	    (fail-parse-format "Major version ~a differs from processor's version." (car version))
	    (progn (if (> (cadr version) 2)
		       (warn-parse "Minor version is greater than that of the processor, attempt to parse anyway."))
		   (setf yaml-version version)
		   nil)))))
			    

(define-yy-rule ns-yaml-version ()
  (let* ((major (postimes ns-dec-digit))
	 (pt #\.)
	 (minor (postimes ns-dec-digit)))
    (declare (ignore pt))
    `(,(parse-integer (text major)) ,(parse-integer (text minor)))))

;; TAG directive
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter tag-handles (make-hash-table :test #'equal))
  (defparameter default-yaml-tagspace "tag:yaml.org,2002:")
  (defparameter default-local-tagspace "!")
  (setf (gethash :secondary-tag-handle tag-handles) default-yaml-tagspace
	(gethash :primary-tag-handle tag-handles) default-local-tagspace))



(define-yy-rule ns-tag-directive ()
  (let* ((tag "TAG")
	 (sep s-separate-in-line)
	 (handle c-tag-handle)
	 (sep1 s-separate-in-line)
	 (prefix ns-tag-prefix))
    (declare (ignore tag sep sep1))
    ;; (format t (literal-string "got tag directive: ~a ~a~%") handle prefix)
    (if (gethash handle tag-handles)
	(fail-parse "The TAG directive must be given at most once per handle in the same document.")
	(setf (gethash handle tag-handles) prefix))
    ;; TODO: should I write NIL here, or I must emit a tag-prefix instead?
    nil))

(define-yy-rule c-tag-handle ()
  (|| c-named-tag-handle
      c-secondary-tag-handle
      c-primary-tag-handle))
(define-yy-rule c-primary-tag-handle ()
  #\! :primary-tag-handle)
(define-yy-rule c-secondary-tag-handle ()
  "!!" :secondary-tag-handle)
(define-yy-rule c-named-tag-handle ()
  `(:named-tag-handle ,(text (progn #\!
				    (prog1 (postimes ns-word-char)
				      #\!)))))
		

(define-yy-rule ns-tag-prefix ()
  (text (|| c-ns-local-tag-prefix
	    ns-global-tag-prefix)))
(define-yy-rule c-ns-local-tag-prefix ()
  (list #\! (times ns-uri-char)))
(define-yy-rule ns-global-tag-prefix ()
  (list ns-tag-char (times ns-uri-char)))
  
;;; Node properties

(define-yy-rule c-ns-properties ()
  `(:properties ,@(remove-if-not #'identity
				 (|| (list c-ns-tag-property (? (progn s-separate
								       c-ns-anchor-property)))
				     (list c-ns-anchor-property (? (progn s-separate
									  c-ns-tag-property)))))))

(define-yy-rule c-ns-tag-property ()
  (|| c-verbatim-tag
      c-ns-shorthand-tag
      c-non-specific-tag))
(define-yy-rule c-verbatim-tag ()
  `(:tag . ,(text (progm "!<" (postimes ns-uri-char) ">"))))

(defun resolve-handle (handle position text)
  (or (gethash handle tag-handles)
      (fail-parse-format "Unknown handle ~a. Did you forget to declare it?" handle)))
		
(define-yy-rule c-ns-shorthand-tag ()
  (let ((handle c-tag-handle)
	(meat (postimes ns-tag-char)))
    `(:tag . ,(text (resolve-handle handle position text) meat))))

(define-yy-rule c-non-specific-tag ()
  `(:tag .  ,(progn "!" :vanilla)))

(define-yy-rule c-ns-anchor-property ()
  `(:anchor . ,(text (progn #\& ns-anchor-name))))
(define-yy-rule ns-anchor-char ()
  (! c-flow-indicator)
  ns-char)
(define-yy-rule ns-anchor-name ()
  (postimes ns-anchor-char))

;;; alias nodes
(define-yy-rule c-ns-alias-node ()
  `(:alias . ,(text (progn #\* ns-anchor-name))))

;;; empty node
(define-yy-rule e-scalar () :empty)
(define-yy-rule e-node ()
  `((:properties (:tag . :non-specific)) (:content . ,e-scalar)))
    
;;; double-quoted-scalars

(define-yy-rule nb-double-char ()
  (|| c-ns-esc-char
      (progn (! #\\)
	     (! #\")
	     nb-json)))
(define-yy-rule ns-double-char ()
  (! s-white)
  nb-double-char)


(define-yy-rule c-double-quoted ()
  (text (progm #\" nb-double-text #\")))
(define-yy-rule nb-double-text ()
  (cond-parse ((|| block-key-context flow-key-context) nb-double-one-line)
	      ((|| block-out-context
		   block-in-context
		   flow-out-context
		   flow-in-context) nb-double-multi-line)))

(define-yy-rule nb-double-one-line ()
  (times nb-double-char))

(define-context-forcing-rule flow-in l-empty)

(define-yy-rule s-double-escaped ()
  (destructuring-bind (white slash bnc empties pref)
      (list (times s-white) #\\ b-non-content
	    (times flow-in-l-empty) s-flow-line-prefix)
    (declare (ignore slash bnc))
    `(,white ,empties ,(mapcar (lambda (x)
				 (if (numberp x)
				     (make-string x :initial-element (literal-char #\space))
				     x))
			       pref))))
(define-yy-rule s-double-break ()
  (|| s-double-escaped s-flow-folded))
(define-yy-rule nb-ns-double-in-line ()
  (times (list (times s-white) ns-double-char)))
(define-yy-rule s-double-next-line ()
  (list s-double-break
	(? (list ns-double-char
		 nb-ns-double-in-line
		 (|| s-double-next-line (times s-white))))))

(define-yy-rule nb-double-multi-line ()
  (list nb-ns-double-in-line
	(|| s-double-next-line (times s-white))))

(defun whitespace-p (text)
  (iter (for char in-string text)
	(if (not (member char '(#\tab #\space)))
	    (return nil))
	(finally (return t))))


;;; single-quoted-scalars
(define-yy-rule c-quoted-quote ()
  "''"
  (literal-char #\'))
(define-yy-rule nb-single-char ()
  (|| c-quoted-quote
      (progn (! #\')
	     nb-json)))
(define-yy-rule ns-single-char ()
  (! s-white)
  nb-single-char)

(define-yy-rule c-single-quoted ()
  (text (progm #\' nb-single-text #\')))
(define-yy-rule nb-single-text ()
  (cond-parse ((|| block-key-context flow-key-context) nb-single-one-line)
	      ((|| block-out-context
		   block-in-context
		   flow-out-context
		   flow-in-context) nb-single-multi-line)))

(define-yy-rule nb-single-one-line ()
  (times nb-single-char))

(define-yy-rule nb-ns-single-in-line ()
  (times (list (times s-white) ns-single-char)))
(define-yy-rule s-single-next-line ()
  (list s-flow-folded
	(? (list ns-single-char
		 nb-ns-single-in-line
		 (|| s-single-next-line (times s-white))))))
(define-yy-rule nb-single-multi-line ()
  (list nb-ns-single-in-line
	(|| s-single-next-line
	    (times s-white))))

;; Block scalars

(define-yy-rule c-b-block-header ()
  (prog1 (|| (list c-indentation-indicator-ne c-chomping-indicator)
	     (list c-chomping-indicator-ne c-indentation-indicator)
	     (list c-chomping-indicator c-indentation-indicator))
    s-b-comment))

(define-yy-rule c-indentation-indicator-ne ()
  `(:block-indentation-indicator . ,(string ns-dec-digit)))
(define-yy-rule c-indentation-indicator ()
  `(:block-indentation-indicator . ,(string (|| ns-dec-digit
						""))))
(define-yy-rule c-chomping-indicator-ne ()
  `(:block-chomping-indicator . ,(string (|| #\- #\+))))
(define-yy-rule c-chomping-indicator ()
  `(:block-chomping-indicator . ,(string (|| #\- #\+ ""))))

(defun hash->assoc (hash)
  (iter (for (key val) in-hashtable hash)
	(collect `(,key . ,val))))

(define-yy-rule b-chomped-last ()
  b-break
  (case block-scalar-chomping
    (:clip (literal-char #\newline))
    (:keep (literal-char #\newline))
    (:strip nil)))

(define-yy-rule b-non-content ()
  b-break
  nil)
(define-yy-rule b-as-line-feed ()
  b-break)

(define-yy-rule l-chomped-empty ()
  (cond-parse ((|| strip-block-scalar-chomping
		   clip-block-scalar-chomping) l-strip-empty)
	      (keep-block-scalar-chomping l-keep-empty)))
(define-yy-rule l-strip-empty ()
  (times (progn s-indent-<=n b-non-content))
  (? l-trail-comments)
  "")
(define-yy-rule l-keep-empty ()
  (list (times l-empty)
	(? l-trail-comments)))
(define-yy-rule l-trail-comments ()
  s-indent-<n
  c-nb-comment-text
  b-comment
  (times l-comment)
  "")

(define-yy-rule l-literal-content ()
  (text (? (list l-nb-literal-text
		 (times b-nb-literal-next)
		 b-chomped-last))
	l-chomped-empty))

(let ((chomping-map '(("+" . :keep) ("-" . :strip) ("" . :clip)))
      (style-map '(("|" . :literal) (">" . :folded))))
  (define-yy-rule c-l-block-scalar ()
    (macrolet ((call-parser ()
		 `(text block-scalar-content)))
      (let ((header (list (|| "|" ">") c-b-block-header)))
	(let ((block-scalar-chomping (cdr (assoc (cdr (assoc :block-chomping-indicator
							     (cadr header)))
						 chomping-map
						 :test #'equal)))
	      (block-scalar-style (cdr (assoc (car header)
					      style-map
					      :test #'equal))))
	  (let ((it (cdr (assoc :block-indentation-indicator
				(cadr header)))))
	    (if (not (equal it ""))
		(let ((n (+ n (parse-integer it)))
		      (indent-style :determined))
		  (call-parser))
		(let ((indent-style :autodetect))
		  (call-parser)))))))))

(define-yy-rule block-scalar-content ()
  (let ((wrapper (cond-parse (autodetect-indent-style detect-indent)
			     (t ""))))
    (macrolet ((call-parser ()
		 `(cond-parse (literal-block-scalar-style l-literal-content)
			      (folded-block-scalar-style l-folded-content))))
      (if (equal wrapper "")
	  (call-parser)
	  (let ((n wrapper)
		(indent-style :determined))
	    (call-parser))))))

(define-yy-rule detect-indent ()
  (& (progm (times l-empty) s-indent-=n nb-char)))

(define-yy-rule l-nb-literal-text ()
  (list (times l-empty)
	(progn s-indent-=n (postimes nb-char))))

(define-yy-rule b-nb-literal-next ()
  (list b-as-line-feed l-nb-literal-text))
				    
(define-yy-rule s-nb-folded-text ()
  s-indent-=n
  (list ns-char (times nb-char)))
(define-yy-rule l-nb-folded-lines ()
  (list s-nb-folded-text
	(times (list b-l-folded s-nb-folded-text))))

(define-yy-rule s-nb-spaced-text ()
  s-indent-=n
  (list s-white (times nb-char)))
(define-yy-rule b-l-spaced ()
  (list b-as-line-feed (times l-empty)))
(define-yy-rule l-nb-spaced-lines ()
  (list s-nb-spaced-text
	(times (list b-l-spaced s-nb-spaced-text))))
(define-yy-rule l-nb-same-lines ()
  (list (times l-empty)
	(|| l-nb-folded-lines l-nb-spaced-lines)))
(define-yy-rule l-nb-diff-lines ()
  (list l-nb-same-lines
	(times (list b-as-line-feed l-nb-same-lines))))
(define-yy-rule l-folded-content ()
  (list (? (list l-nb-diff-lines b-chomped-last))
	l-chomped-empty))

;;; Plain scalars

(define-yy-rule ns-plain-first ()
  (|| (progn (! c-indicator)
	     ns-char)
      (prog1 (|| #\? #\: #\-)
	(& ns-plain-safe))))

(define-yy-rule ns-plain-safe ()
  (cond-parse ((|| flow-out-context
		   block-key-context
		   block-in-context
		   block-out-context) ns-plain-safe-out)
	      ((|| flow-in-context flow-key-context) ns-plain-safe-in)))
(define-yy-rule ns-plain-safe-out () ns-char)
(define-yy-rule ns-plain-safe-in ()
  (! c-flow-indicator)
  ns-char)
(define-yy-rule ns-plain-char ()
  (cond-parse ((list (! #\:) (! #\#)) ns-plain-safe)
	      ((<- ns-char) #\#)
	      (t (prog1 #\: (-> ns-plain-safe)))))
  
(define-yy-rule ns-plain ()
  (cond-parse ((|| flow-out-context
		   flow-in-context
		   block-out-context
		   block-in-context) ns-plain-multi-line)
	      ((|| block-key-context flow-key-context) ns-plain-one-line)))

(define-yy-rule nb-ns-plain-in-line ()
  (times (list (times s-white) ns-plain-char)))
(define-yy-rule ns-plain-one-line ()
  (text ns-plain-first nb-ns-plain-in-line))

(define-yy-rule s-ns-plain-next-line ()
  (list s-flow-folded ns-plain-char nb-ns-plain-in-line))
(define-yy-rule ns-plain-multi-line ()
  (text ns-plain-one-line (times s-ns-plain-next-line)))

;; block sequences

(define-yy-rule l+block-sequence ()
  (let ((n detect-block-sequence)
        (indent-style :determined)
        (context :block-in))
    (postimes (progn s-indent-=n
		     c-l-block-seq-entry))))

(define-yy-rule detect-block-sequence ()
  (let ((indent-style :autodetect))
    (& (prog1 s-indent-=n
	 (& (list #\- (! ns-char)))))))

(define-yy-rule detect-block-mapping ()
  (let ((indent-style :autodetect))
    (& (prog1 s-indent-=n
	 ns-char))))

(define-yy-rule c-l-block-seq-entry ()
  "-" (! ns-char)
  s-l+block-indented)

(define-yy-rule s-l+block-indented ()
  (|| compact-block-node
      s-l+block-node
      (prog1 e-node
	s-l-comments)))

(define-yy-rule compact-block-node ()
  (let ((n (+ n 1 (length (postimes s-space))))
	(indent-style :determined))                          
    `((:properties (:tag . :non-specific))
      (:content . ,(|| ns-l-compact-sequence ns-l-compact-mapping)))))
		  
(define-yy-rule ns-l-compact-sequence ()
  `(,c-l-block-seq-entry
    ,.(times (progn s-indent-=n c-l-block-seq-entry))))

(define-yy-rule l+block-mapping ()
  (let ((n detect-block-mapping)
	(indent-style :determined))
    `(:mapping ,.(postimes (progn s-indent-=n ns-l-block-map-entry)))))

(define-yy-rule ns-l-block-map-entry ()
  (destructuring-bind (key value) (|| c-l-block-map-explicit-entry
				      ns-l-block-map-implicit-entry)
    `(,key . ,value)))
(define-yy-rule c-l-block-map-explicit-entry ()
  (list c-l-block-map-explicit-key
	(|| l-block-map-explicit-value
	    e-node)))

(define-yy-rule c-l-block-map-explicit-key ()
  (let ((context :block-out))
    #\?
    s-l+block-indented))
(define-yy-rule l-block-map-explicit-value ()
  (let ((context :block-out))
    s-indent-=n
    ":"
    s-l+block-indented))

(define-yy-rule ns-l-block-map-implicit-entry ()
  (list (|| ns-s-block-map-implicit-key
	    e-node)
	c-l-block-map-implicit-value))
(define-yy-rule ns-s-block-map-implicit-key ()
  (let ((context :block-key))
    (|| c-s-implicit-json-key
	ns-s-implicit-yaml-key)))
(define-yy-rule c-l-block-map-implicit-value ()
  #\:
  (let ((context :block-out))
    (|| s-l+block-node
	(prog1 e-node
	  s-l-comments))))

(define-yy-rule ns-l-compact-mapping ()
  `(:mapping ,ns-l-block-map-entry ,.(times (progn s-indent-=n
						   ns-l-block-map-entry))))

;; block nodes

(define-yy-rule s-l+block-node ()
  (|| s-l+block-in-block s-l+flow-in-block))
(define-yy-rule s-l+flow-in-block ()
  (let ((context :flow-out)
	(n (1+ n)))
    (progm s-separate ns-flow-node s-l-comments)))

(define-yy-rule s-l+block-in-block ()
  (|| s-l+block-scalar s-l+block-collection))

(define-yy-rule s-separate-n+1 ()
  (let ((n (1+ n)))
    s-separate))

(defparameter vanilla-scalar-tag "tag:yaml.org,2002:str")
(defparameter vanilla-mapping-tag "tag:yaml.org,2002:map")
(defparameter vanilla-sequence-tag "tag:yaml.org,2002:seq")

(define-yy-rule s-l+block-scalar ()
  s-separate-n+1
  (let ((props block-node-properties)
	(content c-l-block-scalar))
    (crunch-tag-into-properties props vanilla-scalar-tag vanilla-scalar-tag)
    `(,props (:content . ,content))))

(define-yy-rule block-node-properties ()
  (let ((n (1+ n)))
    (? (progn s-separate c-ns-properties))))

(defun seq-spaces (n)
  (if (eql context :block-out)
      (1- n)
      n))

(define-yy-rule s-l+block-collection ()
  (let ((props block-node-properties))
    s-l-comments
    (let ((content (|| l+block-sequence-seq-spaces
		       l+block-mapping)))
      (crunch-tag-into-properties props :non-specific
				  (case (car content)
				    (:mapping vanilla-mapping-tag)
				    (t vanilla-sequence-tag)))
      `(,props (:content . ,content)))))


(define-yy-rule l+block-sequence-seq-spaces ()
  (let ((n (seq-spaces n)))
    l+block-sequence))

;; flow collections

(defun in-flow (context)
  (case context
    ((:block-in :block-out :flow-in :flow-out) :flow-in)
    ((:block-key :flow-key) :flow-key)))

(define-yy-rule c-flow-sequence ()
  #\[ (? s-separate)
  (let ((context (in-flow context)))
    (prog1 (? ns-s-flow-seq-entries)
      #\])))

(define-yy-rule ns-s-flow-seq-entries ()
  (let ((entry ns-flow-seq-entry))
    (? s-separate)
    (let ((entries (? (progn #\, (? s-separate) (? ns-s-flow-seq-entries)))))
      `(,entry ,. entries))))

(define-yy-rule ns-flow-seq-entry ()
  (|| ns-flow-pair ns-flow-node))

(define-yy-rule c-flow-mapping ()
  #\{ (? s-separate)
  (let ((context (in-flow context)))                   
    `(:mapping ,. (prog1 (? ns-s-flow-map-entries)
		    #\}))))

(define-yy-rule ns-s-flow-map-entries ()
  (let ((entry ns-flow-map-entry))
    (? s-separate)
    (let ((entries (? (progn #\, (? s-separate) (? ns-s-flow-map-entries)))))
      `(,entry ,. entries))))

(define-yy-rule ns-flow-map-entry ()
  (destructuring-bind (key value) (|| (progn #\? s-separate ns-flow-map-explicit-entry)
				      ns-flow-map-implicit-entry)
    `(,key . ,value)))
(define-yy-rule ns-flow-map-explicit-entry ()
  (|| ns-flow-map-implicit-entry
      (list e-node e-node)))

(define-yy-rule ns-flow-map-implicit-entry ()
  (|| ns-flow-map-yaml-key-entry
      c-ns-flow-map-empty-key-entry
      c-ns-flow-map-json-key-entry))
(define-yy-rule ns-flow-map-yaml-key-entry ()
  (list ns-flow-yaml-node
	(|| (progn (? s-separate) c-ns-flow-map-separate-value)
	    e-node)))
(define-yy-rule c-ns-flow-map-empty-key-entry ()
  (list e-node c-ns-flow-map-separate-value))

(define-yy-rule c-ns-flow-map-separate-value ()
  #\: (! ns-plain-safe)
  (|| (progn s-separate ns-flow-node)
      e-node))

(define-yy-rule c-ns-flow-map-json-key-entry ()
  (list c-flow-json-node
	(|| (progn (? s-separate) c-ns-flow-map-adjacent-value)
	    e-node)))
(define-yy-rule c-ns-flow-map-adjacent-value ()
  #\:
  (|| (progn (? s-separate) ns-flow-node)
      e-node))

(define-yy-rule ns-flow-pair ()
  (destructuring-bind (key value) (|| (progn #\? s-separate ns-flow-map-explicit-entry)
                                      ns-flow-pair-entry)
    `(:mapping (,key . ,value))))

(define-yy-rule ns-flow-pair-entry ()
  (|| ns-flow-pair-yaml-key-entry
      c-ns-flow-map-empty-key-entry
      c-ns-flow-pair-json-key-entry))


(define-context-forcing-rule flow-key ns-s-implicit-yaml-key)
(define-context-forcing-rule flow-key c-s-implicit-json-key)

(define-yy-rule ns-flow-pair-yaml-key-entry ()
  (list flow-key-ns-s-implicit-yaml-key
	c-ns-flow-map-separate-value))

(define-yy-rule c-ns-flow-pair-json-key-entry ()
  (list flow-key-c-s-implicit-json-key
	c-ns-flow-map-adjacent-value))

;; FIXME: implement restriction on the length of the key
;; FIXME: implement n/a in the indentation portion
(define-yy-rule ns-s-implicit-yaml-key ()
  (prog1 ns-flow-yaml-node
    (? s-separate-in-line)))
(define-yy-rule c-s-implicit-json-key ()
  (prog1 c-flow-json-node
    (? s-separate-in-line)))

(define-yy-rule ns-flow-yaml-content () ns-plain)
(define-yy-rule c-flow-json-content ()
  (|| c-flow-sequence
      c-flow-mapping
      c-single-quoted
      c-double-quoted))
(define-yy-rule ns-flow-content ()
  (|| (list :yaml ns-flow-yaml-content)
      (list :json c-flow-json-content)))


(defmacro with-ensured-properties-not-alias (var &body body)
  `(if (alias-p ,var)
       ,var
       (let ((,var (cond ((property-node-p ,var) ,var)
			 (t `(,(list :properties) (:content . ,,var))))))
	 ,@body)))

(define-yy-rule ns-flow-yaml-node ()
  (let ((node (|| c-ns-alias-node
		  ns-flow-yaml-content
		  ns-flow-yaml-properties-node)))
    (with-ensured-properties-not-alias node
      (let ((props (assoc :properties node))
	    (content (cdr (assoc :content node))))
	(crunch-tag-into-properties props :non-specific vanilla-scalar-tag)
	`(,props (:content . ,content))))))
				    
(define-yy-rule ns-flow-yaml-properties-node ()
  `(,c-ns-properties (:content . ,(|| (progn s-separate ns-flow-yaml-content)
				      e-scalar))))

(define-yy-rule c-flow-json-node ()
  (let ((props (? (prog1 c-ns-properties
		    s-separate)))
	(content c-flow-json-content))
    (if (atom content)
	(crunch-tag-into-properties props vanilla-scalar-tag vanilla-scalar-tag)
	(crunch-tag-into-properties props :non-specific (if (equal (car content) :mapping)
							    vanilla-mapping-tag
							    vanilla-sequence-tag)))
    `(,props (:content . ,content))))


(define-yy-rule ns-flow-node ()
  (let ((node (|| c-ns-alias-node
		  ns-flow-content
		  ns-flow-properties-node)))
    (with-ensured-properties-not-alias node
      (let ((props (assoc :properties node))
	    (content (cdr (assoc :content node))))
	(destructuring-bind (content-type content) content
	  (crunch-tag-into-properties props
				      (cond ((eql content-type :yaml) :non-specific)
					    ((eql content-type :json)
					     (cond ((atom content) vanilla-scalar-tag)
						   ((eql (car content) :mapping) :non-specific)
						   (t :non-specific))))
				      (cond ((atom content) vanilla-scalar-tag)
					    ((eql (car content) :mapping) vanilla-mapping-tag)
					    (t vanilla-sequence-tag)))
	  `(,props (:content . ,content)))))))

(define-yy-rule ns-flow-properties-node ()
  `(,c-ns-properties
    (:content . ,(|| (progn s-separate ns-flow-content)
		     (list :yaml e-scalar)))))

;;; YaML documents

;; FIXME: correct definition of byte-order-mark
(define-yy-rule c-byte-order-mark ()
  (progn #\UEFBB #\INVERTED_QUESTION_MARK :utf8-bom))

(define-yy-rule l-document-prefix ()
  (list (? c-byte-order-mark) (times l-comment)))
(define-yy-rule c-directives-end ()
  (times #\- :exactly 3))
(define-yy-rule c-document-end ()
  (times #\. :exactly 3))
(define-yy-rule l-document-suffix ()
  (list c-document-end s-l-comments))

(define-yy-rule c-forbidden ()
  (list start-of-line
	(|| c-directives-end c-document-end)
	(|| b-char s-white (-> eof))))

(define-yy-rule l-bare-document ()
  (let ((text (times (list (! c-forbidden)
			   (list (times nb-char)
				 (? b-break))))))
    ;; (format t (literal-string "text: ~a~%") text)
    (let ((n -1)
	  (indent-style :determined)
	  (context :block-in))
      `(:document ,(yy-parse 's-l+block-node (text text))))))

(define-yy-rule l-explicit-document ()
  (let ((text (progn c-directives-end (|| l-bare-document
					  (prog1 e-node s-l-comments)))))
    `(:document ,(if (and (consp text) (eql (car text) :document))
		     (cadr text)
		     text))))

(define-yy-rule l-directive-document ()
  (let ((yaml-version nil)
	(tag-handles (make-hash-table :test #'equal)))
    (postimes l-directive)
    (compile-tag-handles)
    (if (not (gethash :secondary-tag-handle tag-handles))
	(setf (gethash :secondary-tag-handle tag-handles) default-yaml-tagspace))
    (if (not (gethash :primary-tag-handle tag-handles))
	(setf (gethash :primary-tag-handle tag-handles) default-local-tagspace))
    l-explicit-document))

(let (path)
  (declare (special path))
  (defun try-resolve (handle)
    (multiple-value-bind (prefix position)
	(yaclyaml-parse 'c-tag-handle handle :junk-allowed t)
      ;; (format t "~%in resolving handle: ~a ~a ~a~%" handle prefix position)
      (if prefix
	  (let ((pos (position prefix path :test #'equal)))
	    (if (and pos position (not (equal 0 pos)))
		(error "Loop in prefixes resolution: ~a~%" `(,@(subseq path pos) ,prefix))
		(strcat (gethash prefix tag-handles)
			(if position
			    (subseq handle position)))))
	  handle)))

  (defun compile-tag-handles ()
    ;; (format t "~%handles: ~a ~%" (hash->assoc tag-handles))
    (iter (for (key . nil) in (hash->assoc tag-handles))
	  ;; (format t "handle: ~a ~%" key)
	  (let ((path `(,key)))
	    (declare (special path))
	    (iter (while t)
		  (let ((val (gethash key tag-handles)))
		    ;; (format t "value: ~a ~%" val)
		    (let ((try-val (try-resolve val)))
		      ;; (format t "try-value: ~a ~%" try-val)
		      (if (equal try-val val)
			  (terminate)
			  (setf (gethash key tag-handles) try-val
				val try-val
				path (cons try-val path))))))))
    ;; (format t "~%compiled handles: ~a ~%" (hash->assoc tag-handles))
    ))


(define-yy-rule l-any-document ()
  (|| l-directive-document
      l-explicit-document
      l-bare-document))

(define-yy-rule l-yaml-stream ()
  (let ((first (progn (times l-document-prefix) (? l-any-document)))
	(rest (times (|| (progn (postimes l-document-suffix)
				(times l-document-prefix)
				(? l-any-document))
			 (progn (times l-document-prefix)
				(? l-explicit-document))))))
    `(,first ,.(if (and rest (car rest)) rest (list)))))
