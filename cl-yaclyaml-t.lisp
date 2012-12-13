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

;;; Testing plain scalar reading
(def-ts-macro test-plain-scalar-reader yaml-plain-scalar-reader (nil))
(test-plain-scalar-reader
 #?" 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty : "
 #\:
 #?"1st non-empty\n2nd non-empty 3rd non-empty")

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


                    
