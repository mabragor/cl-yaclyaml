(in-package :cl-user)

(defpackage :cl-yaclyaml-tests
  (:use :alexandria :cl :cl-yaclyaml :eos)
  (:export #:run-tests))

(in-package :cl-yaclyaml-tests)

(cl-interpol:enable-interpol-syntax)

(def-suite yaclyaml)
(in-suite yaclyaml)

(defun run-tests ()
  (let ((results (run 'yaclyaml)))
    (eos:explain! results)
    (unless (eos:results-status results)
      (error "Tests failed."))))

(test block-scalar-header
  (is (equal '((:block-indentation-indicator . "3") (:block-chomping-indicator . "-"))
	     (yaclyaml-parse 'c-b-block-header #?"3- #asdf\n")))
  (is (equal '((:block-chomping-indicator . "-") (:block-indentation-indicator . "3"))
	     (yaclyaml-parse 'c-b-block-header #?"-3 #asdf\n")))
  (is (equal '((:block-chomping-indicator . "-") (:block-indentation-indicator . ""))
	     (yaclyaml-parse 'c-b-block-header #?"- #asdf\n")))
  (is (equal '((:block-indentation-indicator . "3") (:block-chomping-indicator . ""))
	     (yaclyaml-parse 'c-b-block-header #?"3 #asdf\n")))
  (is (equal '((:block-chomping-indicator . "") (:block-indentation-indicator . ""))
	     (yaclyaml-parse 'c-b-block-header #?" #asdf\n"))))


(test literal-block-scalars
  (is (equal #?" explicit\n" (yaclyaml-parse 'c-l-block-scalar #?"|1\n  explicit\n")))
  (is (equal #?"text" (yaclyaml-parse 'c-l-block-scalar #?"|0-\ntext\n")))
  (is (equal #?"text\n" (yaclyaml-parse 'c-l-block-scalar #?"|0\ntext\n")))
  (is (equal #?"text\n" (yaclyaml-parse 'c-l-block-scalar #?"|0+\ntext\n")))
  (is (equal #?"\n\nliteral\n \n\ntext\n"
	     (yaclyaml-parse 'c-l-block-scalar
			     #?"|2\n \n  \n  literal\n   \n  \n  text\n\n\n # Comment\n"))))

(test folded-block-scalars
  (is (equal #?"folded text\n" (yaclyaml-parse 'c-l-block-scalar #?">1\n folded text\n\n")))
  (is (equal #?"\nfolded line\nnext line\nlast line\n"
	     (yaclyaml-parse 'c-l-block-scalar
			     #?">\n\n folded\n line\n\n next\n line\n\n last\n line\n
# Comment\n")))
  (is (equal #?"foobar\n  * bullet\n  * list\n\n  * lines\n"
	     (yaclyaml-parse 'c-l-block-scalar
			     #?">\n foobar\n   * bullet\n   * list\n\n   * lines\n"))))
      
(test plain-scalars
  (is (equal #?"1st non-empty\n2nd non-empty 3rd non-empty"
	     (yaclyaml-parse 'ns-plain
			     #?"1st non-empty\n\n 2nd non-empty \n\t3rd non-empty"))))

(test double-quoted-scalars
  (is (equal "implicit block key" (let ((cl-yaclyaml::context :block-key))
				    (yaclyaml-parse 'c-double-quoted "\"implicit block key\""))))
  (is (equal "implicit flow key" (let ((cl-yaclyaml::context :flow-key))
				   (yaclyaml-parse 'c-double-quoted "\"implicit flow key\""))))
  (is (equal #?"folded to a space"
	     (yaclyaml-parse 'c-double-quoted
			     #?"\"folded \nto a space\"")))
  (is (equal #?",\nto a line feed"
  	     (yaclyaml-parse 'c-double-quoted
  			     #?"\",\t\n \nto a line feed\"")))
  (is (equal #?"or \t \tnon-content"
  	     (yaclyaml-parse 'c-double-quoted
  			     #?"\"or \t\\\n \\ \tnon-content\"")))
  ;; and all the above components together
  (is (equal #?"folded to a space,\nto a line feed, or \t \tnon-content"
  	     (yaclyaml-parse 'c-double-quoted
  			     #?"\"folded \nto a space,\t\n \nto a line feed, or \t\\\n \\ \tnon-content\"")))
  (is (equal #?" 1st non-empty\n2nd non-empty 3rd non-empty "
  	     (yaclyaml-parse 'c-double-quoted
  			     #?"\" 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty \""))))

(test single-quoted-scalars
  (is (equal "here's to \"quotes\""
	     (yaclyaml-parse 'c-single-quoted "'here''s to \"quotes\"'")))
  (is (equal #?" 1st non-empty\n2nd non-empty 3rd non-empty "
	     (yaclyaml-parse 'c-single-quoted
			     #?"' 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty '"))))
  
  
(test flow-sequence-nodes
  (is (equal '("one" "two") (yaclyaml-parse 'c-flow-sequence #?"[ one, two, ]")))
  (is (equal '("three" "four") (yaclyaml-parse 'c-flow-sequence #?"[three ,four]")))
  ;; (is (equal '("double quoted" "single quoted" "plain text" ("nested")
  ;; 	       (:mapping ("single" . "pair")))
  ;; 	     (yaclyaml-parse 'c-flow-sequence
  ;; 			     #?"[\n\"double\n quoted\", 'single
  ;;      quoted',\nplain\n text, [ nested ],\nsingle: pair,\n]")))
  )

(test plain-scalars
  (is (equal "::vector"
	     (let ((cl-yaclyaml::context :block-in))
	       (yaclyaml-parse 'ns-plain
			       #?"::vector"))))
  (is (equal "Up, up, and away!"
	     (let ((cl-yaclyaml::context :block-in))
	       (yaclyaml-parse 'ns-plain
			       #?"Up, up, and away!"))))
  (is (equal "-123"
	     (let ((cl-yaclyaml::context :block-in))
	       (yaclyaml-parse 'ns-plain
			       #?"-123"))))
  (is (equal "http://example.com/foo#bar"
	     (let ((cl-yaclyaml::context :block-in))
	       (yaclyaml-parse 'ns-plain
			       #?"http://example.com/foo#bar"))))
  )

(test flow-mapping-nodes
  (is (equal '(:mapping ("one" . "two") ("three" . "four"))
	     (yaclyaml-parse 'c-flow-mapping
			     #?"{ one : two , three: four , }")))
  (is (equal '(:mapping ("five" . "six") ("seven" . "eight"))
	     (yaclyaml-parse 'c-flow-mapping
			     #?"{five: six,seven : eight}")))
  (is (equal '(:mapping ("explicit" . "entry") ("implicit" . "entry") ("" . ""))
	     (yaclyaml-parse 'c-flow-mapping
			     #?"{\n? explicit: entry,\nimplicit: entry,\n?\n}")))
  (is (equal '(:mapping ("unquoted" . "separate")
	       ("http://foo.com" . "")
	       ("omitted value" . "")
	       ("" . "omitted key"))
	     (yaclyaml-parse 'c-flow-mapping
			     #?"{\nunquoted : \"separate\",\nhttp://foo.com,
omitted value:,\n: omitted key,\n}")))
  )
