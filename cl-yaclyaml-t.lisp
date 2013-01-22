(in-package cl-yaclyaml)

(cl-interpol:enable-interpol-syntax)

(plan 5)
(diag "Testing just in case test suite does not work.")
(ok (eq (+ 1 1) 2) "Test 1")
(ok (eq (+ 2 2) 4) "Test 2")

;; We need to hack out a macro to help our testing
(defmacro test-reader (fname (&rest string-components) prev-char
                                string-read next-char)
  (sb-int:with-unique-names (g!-value g!-next-char)
    `(multiple-value-bind (,g!-value ,g!-next-char)
         (,fname (make-string-input-stream (strcat ,@string-components))
                 ,prev-char)
       (if (not (ok (equal ,g!-value ,string-read)))
           (format t "# String read incorrectly:~%# expected: ~s~%# found: ~s~%"
                   ,string-read ,g!-value))
       (if (not (ok (equal ,g!-next-char ,next-char)))
           (format t (strcat "# Next char determined incorrectly:~%"
                               "# expected: ~s~%"
                               "# found: ~s~%")
                   ,next-char ,g!-next-char)))))

(defmacro test-error (e-name &body body)
  `(handler-case (progn ,@body (ok (equal 1 2)))
     (,e-name () (ok (equal 1 1)))))

(diag "Testing single-quote-reader")
(test-reader single-quote-reader
             (#?/here''s to "quotes"'/) #\'
             '(scalar :tag ! #?"here's to \"quotes\"") nil)
(test-reader single-quote-reader
             (#?" 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty '") #\'
             '(scalar :tag ! #?" 1st non-empty\n2nd non-empty 3rd non-empty ") nil)
(test-reader single-quote-reader
             (#?" 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty 'a") #\'
             '(scalar :tag ! #?" 1st non-empty\n2nd non-empty 3rd non-empty ") #\a)
;; This test should pass if an error is correctly generated

(test-error simple-reader-error
  (test-reader single-quote-reader
               (#?" 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty ") #\'
               '(scalar :tag !
                 #?" 1st non-empty\n2nd non-empty 3rd non-empty ") #\a))

(defmacro anchor-tag-context (anchor tag &body body)
  `(let ((anchor ,anchor)
         (tag ,tag))
     (declare (special anchor tag))
     ,@body))

(anchor-tag-context "anchor1" "tag1"
  (test-reader single-quote-reader
             (#?/here''s to "quotes"'/) #\'
             '(scalar :anchor "anchor1" :tag "tag1"
               #?"here's to \"quotes\"") nil))

(diag "Testing yaml-double-quoted-scalar-reader")
(test-reader double-quote-reader ("asdf\"") #\" '(scalar :tag ! "asdf") nil)
(test-reader double-quote-reader
             (#?" 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty \" asdf") #\"
             '(scalar :tag ! #?" 1st non-empty\n2nd non-empty 3rd non-empty ") nil)
(test-reader double-quote-reader
             (#?"    \\\n\"") #\" '(scalar :tag ! #?"    ") nil)
(test-reader double-quote-reader
             (#?"folded \nto a space,\t\n \n"
                #?"to a line feed, or \t\\\n \\ \tnon-content\"")
             #\"
             '(scalar :tag !
               #?"folded to a space,\nto a line feed, or \t \tnon-content")
             nil)
(test-error simple-reader-error
  (double-quote-reader
   (make-string-input-stream (strcat #?"folded \nto a space,\t\n \n"
                                     #?"to a line feed, or \t\\\n"
                                     #?"\\ \tnon-content"))
   #\"))


(diag "Testing read-comment")
     
(test-reader read-comment ("asdf" #\newline "a") #\# nil #\newline)
(test-reader read-comment ("asdf" #\return "a") #\# nil #\newline)
(test-reader read-comment ("asdf" #\return #\newline "a") #\# nil #\newline)

(diag "Testing plain-scalar-reader")
(test-reader plain-scalar-reader
             (#?"st non-empty\n\n 2nd non-empty \n\t3rd non-empty : ")
             '(#\1)
             '(scalar :tag ? #?"1st non-empty\n2nd non-empty 3rd non-empty")
             #\:)
(test-reader plain-scalar-reader
             (#?"vector\n- \":")
             '(#\: #\:)
             '(scalar :tag ? #?"::vector")
             #\-)
(test-reader plain-scalar-reader
             (#?"p, up, and away!")
             '(#\U)
             '(scalar :tag ? #?"Up, up, and away!")
             nil)
(test-reader plain-scalar-reader
             (#?"23 # tra-la-la")
             '(#\- #\1)
             '(scalar :tag ? #?"-123")
             nil)
(test-reader plain-scalar-reader
             (#?"ttp://example.com/foo#bar\n# Inside flow collection:")
             '(#\h)
             '(scalar :tag ? #?"http://example.com/foo#bar")
             nil)
(anchor-tag-context "anchor1" "tag1"
  (test-reader plain-scalar-reader
               (#?"ttp://example.com/foo#bar\n# Inside flow collection:")
               '(#\h)
               '(scalar :anchor "anchor1" :tag "tag1"
                 #?"http://example.com/foo#bar")
               nil))
(let ((yaml-context 'flow-in))
  (declare (special yaml-context))
  (test-reader plain-scalar-reader
               (#?"vector\n- \":")
               '(#\: #\:)
               '(scalar :tag ? #?"::vector")
               #\-)
  (test-reader plain-scalar-reader
               (#?"p, up, and away!")
               '(#\U)
               '(scalar :tag ? #?"Up")
               #\,)
  (test-reader plain-scalar-reader
               (#?"23 # tra-la-la")
               '(#\- #\1)
               '(scalar :tag ? #?"-123")
               nil)
  (test-reader plain-scalar-reader
               (#?"ttp://example.com/foo#bar\n# Inside flow collection:")
               '(#\h)
               '(scalar :tag ? #?"http://example.com/foo#bar")
               nil))

(diag "Testing anchor and alias-node reader")

(test-reader read-anchor (#?"asdf") #\& '(anchor "asdf") nil)
(test-reader read-anchor (#?"asdf") #\* '(alias "asdf") nil)
(test-reader read-anchor (#?"asdf[") #\& '(anchor "asdf") #\[)
(test-reader read-anchor (#?"asdf ") #\& '(anchor "asdf") #\space)
(test-reader read-anchor (#?"asdf&") #\& '(anchor "asdf&") nil)

(diag "Testing tag reader")
(test-reader read-verbatim-tag ("tag:yaml.org,2002:str> asd")
             #\< '(tag "tag:yaml.org,2002:str") nil)
(test-reader read-verbatim-tag ("!bar> asdf")
             #\< '(tag "!bar") nil)
(test-error simple-reader-error
  (test-reader read-verbatim-tag ("!> asdf")
             #\< '(tag "tag:yaml.org,2002:str") nil))
(test-error simple-reader-error
  (test-reader read-verbatim-tag ("$:?> asdf")
             #\< '(tag "tag:yaml.org,2002:str") nil))

;; (test-reader read-tag ("<tag:yaml.org,2002:str> asdf")
;;              #\! '(tag "tag:yaml.org,2002:str") nil)
;; (test-reader read-tag ("<!bar> asdf")
;;              #\! '(tag "!bar") nil)

(diag "Testing block scalar header reader")
(test-reader block-scalar-header-reader (#?" # Empty Header\n")
	     #\| '((indent . 0) (chomp . clip)) nil)
(test-reader block-scalar-header-reader (#?"1 # Indentation indicator\n")
	     #\> '((indent . 1) (chomp . clip)) nil)
(test-reader block-scalar-header-reader (#?"- # Chomping indicator\n")
	     #\| '((indent . 0) (chomp . strip)) nil)
(test-reader block-scalar-header-reader (#?"1- # Both indicators\n")
	     #\> '((indent . 1) (chomp . strip)) nil)
(diag "Testing left spaces reader")
(test-reader left-spaces-read ("   ") #\> 3 nil)
(test-reader left-spaces-read ("   ") #\space 4 nil)
(test-reader left-spaces-read (#?"   \n") #\> 3 #\newline)
(test-reader left-spaces-read (#?"   \r\n") #\> 3 #\newline)
(diag "Testing line reader")
(test-reader line-reader ("asdf") #\# "#asdf" nil)
(test-reader line-reader (#?"asdf\n") #\# "#asdf" #\newline)
(test-reader line-reader (#?"asdf\r\n") #\# "#asdf" #\newline)

(diag "Testing block scalar reader")
(test-reader block-scalar-reader
	     (#?"1- # this is header\n"
		#?"   \n"
		#?"    this is first non-empty line.\r\n"
		#?"     this is the second.\n"
		#?"  ")
	     #\| #?"this is first non-empty line.\n this is the second.\n" nil)
