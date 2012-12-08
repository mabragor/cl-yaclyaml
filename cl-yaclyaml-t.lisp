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
	



