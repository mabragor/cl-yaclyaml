;;;; cl-yaclyaml.lisp

(in-package #:cl-yaclyaml)

;;; "cl-yaclyaml" goes here. Hacks and glory await!

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
  (defun yaml-double-quote-reader (stream double-quote-char)
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
                    (setf new-line-count 0))))
      (macrolet ((process-nws-char ()
                   `(if begin-of-first-line-p
                        (progn (use-whitespace)
                               (collect char into res)
                               (setf begin-of-first-line-p nil))
                        (if begin-of-line-p
                            (progn (discard-whitespace)
                                   (fold-newlines)
                                   (collect char into res)
                                   (setf begin-of-line-p nil))
                            (progn (use-whitespace)
                                   (collect char into res))))))
        (let (whitespace
              (begin-of-first-line-p t)
              (begin-of-line-p t)
              (new-line-count 0))
          (iter (generate char in-stream stream using #'read-char)
                (next char)
                ;;(format t "~a~%" char)
                (until (char= char double-quote-char))
                (cond
                  ((or (char= char #\tab) (char= char #\space))
                   (push char whitespace))
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
                  ((char= char #\newline) (progn (discard-whitespace)
                                                 (incf new-line-count)
                                                 (setf begin-of-first-line-p nil
                                                       begin-of-line-p t)))
                  (t (process-nws-char)))
                (finally
                 (return (apply #'strcat (if begin-of-line-p
                                             res
                                             (nconc res whitespace)))))))))))
                      

        
  
