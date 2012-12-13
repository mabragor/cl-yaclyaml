;;;; cl-yaclyaml.lisp

(in-package #:cl-yaclyaml)

;;; "cl-yaclyaml" goes here. Hacks and glory await!

(defun str-* (n str)
  (iter (for i from 0 below n)
        (collect str into res)
        (finally (return (apply #'strcat res)))))

(defmacro fart (&body args)
  `(format t ,(strcat (str-* (- (length args) 1) "~a ") "~a~%") ,@args))

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

(defmacro def-yaml-scalar-reader (name args &body body)
  `(defun ,name ,args
     (macrolet ((use-whitespace ()
                  `(progn (nconcing (nreverse whitespace) into res)
                          (setf whitespace nil)))
                (discard-whitespace ()
                  `(setf whitespace nil))
                (fold-newlines ()
                  `(progn
                     (cond ((equal new-line-count 0) nil)
                           ((equal new-line-count 1)
                            (collect #\space into res))
                           (t (collect (make-string (- new-line-count 1)
                                                    :initial-element #\newline)
                                into res)))
                     (setf new-line-count 0)))
                (b-break-p (char)
                  `(or (char= ,char #\newline)
                       (and (char= ,char #\return)
                            (if (char= (peek-char nil stream nil t t) #\newline)
                                (next ,char))))))
       (macrolet ((process-nws-char (&key discard-leading-whitespace)
                    `(if begin-of-first-line-p
                         (progn ,(if discard-leading-whitespace
                                     `(discard-whitespace)
                                     `(use-whitespace))
                                (collect char into res)
                                (setf begin-of-first-line-p nil))
                         (if begin-of-line-p
                             (progn (discard-whitespace)
                                    (fold-newlines)
                                    (collect char into res)
                                    (setf begin-of-line-p nil))
                             (progn (use-whitespace)
                                    (collect char into res)))))
                  (process-b-break-char ()
                    `(progn (discard-whitespace)
                            (incf new-line-count)
                            (setf begin-of-first-line-p nil
                                  begin-of-line-p t))))
         (flet ((whitespace-p (char) (or (char= char #\space)
                                         (char= char #\tab))))
           (let (whitespace
                 (begin-of-first-line-p t)
                 (begin-of-line-p t)
                 (new-line-count 0))
             (iter (generate char in-non-closing-stream stream using #'read-char)
                   (next char)
                   ;;(format t "~a~%" char)
                   ,@body)))))))

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
  (def-yaml-scalar-reader yaml-double-quote-reader (stream double-quote-char)
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
                 (error "Invalid escape character: ~s ~s"
                        char
                        (char-code char))
                 (let ((char (apif #'characterp (cdr esc)
                                   it
                                   (funcall it stream))))
                   (process-nws-char))))))
      ((b-break-p char) (process-b-break-char))
      (t (process-nws-char)))
    (finally
     (return (values (apply #'strcat (if begin-of-line-p
                                         res
                                         (nconc res whitespace)))
                     nil)))))
        
(def-yaml-scalar-reader yaml-single-quote-reader (stream single-quote-char)
  (cond
    ((char= char single-quote-char)
     (if (char= (next char) single-quote-char)
         (collect char into res)
         (finish)))
    ((whitespace-p char) (push char whitespace))
    ((b-break-p char) (process-b-break-char))
    (t (process-nws-char)))
  (finally
   ;;(fart "Farting:" char (char-code char))
   (return (values (apply #'strcat (if begin-of-line-p
                                       res
                                       (nconc res whitespace)))
                   (if (characterp char) char nil)))))



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

(def-yaml-scalar-reader yaml-read-comment (stream prev-char)
  (fart "Comment:" char)
  (if (b-break-p char) (finish))
  (finally (return (values nil nil))))

(def-yaml-scalar-reader yaml-plain-scalar-reader (stream already-read-chars)
  (fart char)
  (cond
    ((whitespace-p char) (push char whitespace))
    ((b-break-p char) (process-b-break-char))
    ((char= char #\:) (if (whitespace-p (next char))
                          ;; KLUDGE - whitespace will be disregarded anyway
                          (progn (setf char #\:)
                                 (finish))
                          (progn (collect #\: into res)
                                 (collect char into res))))
    ((char= char #\#) (if whitespace
                          ;; Maybe we should retain comments in serialization
                          ;; tree?
                          (yaml-read-comment stream #\#)
                          (collect char into res)))
    ((find char '(#\, #\[ #\] #\{ #\}) :test #'char=)
     (if (find yaml-context '(flow-in flow-key))
         (finish)
         (collect char into res)))
    (t (process-nws-char :discard-leading-whitespace t)))
  (finally (return (values (apply #'strcat res)
                           (if (characterp char) char nil)))))
    



        

;(defun yaml-reader (stream)
;  "Central function of all module. Analog of read-token in standard Lisp reader."
;  ...)
