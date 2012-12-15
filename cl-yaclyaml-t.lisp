(in-package cl-yaclyaml)

(cl-interpol:enable-interpol-syntax)

(plan 5)

(ok (eq (+ 1 1) 2) "Test 1")
(ok (eq (+ 2 2) 4) "Test 2")

(defmacro test-doubly-quoted-scalar (str-in str-out &optional comment)
  `(ok (equal (yaml-double-quote-reader
               (make-string-input-stream ,(strcat str-in "\""))
               #\")
              ,str-out)
       ,comment))

(test-doubly-quoted-scalar "asdf" "asdf")
(test-doubly-quoted-scalar
 #?" 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty "
 #?" 1st non-empty\n2nd non-empty 3rd non-empty ")

(test-doubly-quoted-scalar
 #?"    \\\n" #?"    ")

(test-doubly-quoted-scalar
 #?"folded \nto a space,\t\n \nto a line feed, or \t\\\n \\ \tnon-content"
 #?"folded to a space,\nto a line feed, or \t \tnon-content")

;; Testing reading of singly quoted scalars

(defmacro def-ts-macro (name func-name add-args)
  `(defmacro ,name (str-in char-after str-out &optional comment)
     `(multiple-value-bind (exp-str-out after-char)
          (,',func-name
           (make-string-input-stream ,(strcat str-in char-after))
           ,@',add-args)
        (ok (and (equal exp-str-out ,str-out)
                 (equal after-char ,char-after))
            ,comment))))

(def-ts-macro test-singly-quoted-scalar yaml-single-quote-reader (#\'))

(test-singly-quoted-scalar
 #?/here''s to "quotes"'/ #\[ #?"here's to \"quotes\"")
(test-singly-quoted-scalar
 #?" 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty '" nil
 #?" 1st non-empty\n2nd non-empty 3rd non-empty ")


(diag "Testing yaml-read-comment")
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
     
(test-reader yaml-read-comment ("asdf" #\newline "a") #\# nil #\newline)
(test-reader yaml-read-comment ("asdf" #\return "a") #\# nil #\newline)
(test-reader yaml-read-comment ("asdf" #\return #\newline "a") #\# nil #\newline)

(diag "Testing yaml-plain-scalar-reader")
(test-reader yaml-plain-scalar-reader
             (#?"st non-empty\n\n 2nd non-empty \n\t3rd non-empty : ")
             '(#\1)
             #?"1st non-empty\n2nd non-empty 3rd non-empty"
             #\:)
(test-reader yaml-plain-scalar-reader
             (#?"vector\n- \":")
             '(#\: #\:)
             #?"::vector"
             #\-)
(test-reader yaml-plain-scalar-reader
             (#?"p, up, and away!")
             '(#\U)
             #?"Up, up, and away!"
             nil)
(test-reader yaml-plain-scalar-reader
             (#?"23 # tra-la-la")
             '(#\- #\1)
             #?"-123"
             nil)
(test-reader yaml-plain-scalar-reader
             (#?"ttp://example.com/foo#bar\n# Inside flow collection:")
             '(#\h)
             #?"http://example.com/foo#bar"
             nil)
(let ((yaml-context 'flow-in))
  (declare (special yaml-context))
  (test-reader yaml-plain-scalar-reader
               (#?"vector\n- \":")
               '(#\: #\:)
               #?"::vector"
               #\-)
  (test-reader yaml-plain-scalar-reader
               (#?"p, up, and away!")
               '(#\U)
               #?"Up"
               #\,)
  (test-reader yaml-plain-scalar-reader
               (#?"23 # tra-la-la")
               '(#\- #\1)
               #?"-123"
               nil)
  (test-reader yaml-plain-scalar-reader
               (#?"ttp://example.com/foo#bar\n# Inside flow collection:")
               '(#\h)
               #?"http://example.com/foo#bar"
               nil))



;;; Testing anchor and alias-node reader

;(def-ts-macro test-alias-node-reader yaml-alias-node-reader ())
;(def-ts-macro test-anchor-reader yaml-anchor-reader ())

;; (test-anchor-reader
;;  #?"asdf" #\space #?"asdf")
;; (test-anchor-reader
;;  #?"asdf" #\[ #?"asdf")
;; (test-alias-node-reader
;;  #?"asdf" #\space #?"asdf")
;; (test-alias-node-reader
;;  #?"asdf" #\[ #?"asdf")


                    
