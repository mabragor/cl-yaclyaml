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
  (is (equal #?" explicit\n" (yaclyaml-parse 'c-l-block-scalar #?"|2\n  explicit\n")))
  (is (equal #?"text" (yaclyaml-parse 'c-l-block-scalar #?"|-\ntext\n")))
  (is (equal #?"text\n" (yaclyaml-parse 'c-l-block-scalar #?"|\ntext\n")))
  (is (equal #?"text\n" (yaclyaml-parse 'c-l-block-scalar #?"|+\ntext\n")))
  (is (equal #?"\n\nliteral\n \n\ntext\n"
	     (yaclyaml-parse 'c-l-block-scalar
			     #?"|3\n \n  \n  literal\n   \n  \n  text\n\n\n # Comment\n"))))

(test folded-block-scalars
  (is (equal #?"folded text\n" (yaclyaml-parse 'c-l-block-scalar #?">\n folded text\n\n")))
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
  (is (equal '(:mapping ("adjacent" . "value")
	       ("readable" . "value")
	       ("empty" . ""))
	     (yaclyaml-parse 'c-flow-mapping
			     #?"{\n\"adjacent\":value,\n\"readable\": value,\n\"empty\":\n}")))
  (is (equal '((:mapping ("foo" . "bar")))
	     (yaclyaml-parse 'c-flow-sequence
			     #?"[\nfoo: bar\n]")))
  (is (equal '((:mapping ("foo bar" . "baz")))
	     (yaclyaml-parse 'c-flow-sequence
			     #?"[\n? foo\n bar : baz\n]")))
  (is (equal '((:mapping ("YAML" . "separate")))
	     (yaclyaml-parse 'c-flow-sequence
			     #?"[ YAML : separate ]")))
  (is (equal '((:mapping ("" . "empty key entry")))
	     (yaclyaml-parse 'c-flow-sequence
			     #?"[ : empty key entry ]")))
  (is (equal '((:mapping ((:mapping ("JSON" . "like")) . "adjacent")))
	     (yaclyaml-parse 'c-flow-sequence
			     #?"[ {JSON: like}:adjacent ]")))
  )

(test block-sequences
  (is (equal `("foo" "bar" "baz") (yaclyaml-parse 'l+block-sequence #?"- foo\n- bar\n- baz\n")))
  (is (equal `((:mapping ("one" . "two"))) (yaclyaml-parse 'l+block-sequence #?"- one: two # compact mapping\n")))
  (is (equal `(:mapping ("block sequence" . ("one" (:mapping ("two" . "three")))))
  	     (yaclyaml-parse 'l+block-mapping #?"block sequence:\n  - one\n  - two : three\n")))
  (is (equal `("" ,#?"block node\n" ("one" "two") (:mapping ("one" . "two")))
  	     (yaclyaml-parse 'l+block-sequence
  			     #?"- # Empty\n- |\n block node\n- - one # Compact\n  - two # sequence\n- one: two\n")))
  )

(test block-mappings
  (is (equal `(:mapping ("block mapping" . (:mapping ("key" . "value"))))
	     (yaclyaml-parse 'l+block-mapping #?"block mapping:\n key: value\n")))
  (is (equal `(:mapping ("explicit key" . ""))
	     (yaclyaml-parse 'l+block-mapping #?"? explicit key # Empty value\n")))
  (is (equal `(:mapping (,#?"block key\n" . "flow value"))
	     (yaclyaml-parse 'l+block-mapping #?"? |\n  block key\n: flow value\n")))
  (is (equal `(:mapping (,#?"block key\n" . ("one" "two")))
	     (yaclyaml-parse 'l+block-mapping #?"? |\n  block key\n: - one # Explicit compact\n  - two # block value\n")))
  (is (equal `(:mapping ("plain key" . "in-line value") ("" . "") ("quoted key" . ("entry")))
	     (yaclyaml-parse 'l+block-mapping #?"plain key: in-line value\n: # Both empty\n\"quoted key\":\n- entry\n")))
  )

(test compact-block-mappings
  (is (equal `((:mapping ("sun" . "yellow")) (:mapping ((:mapping ("earth" . "blue")) . (:mapping ("moon" . "white")))))
	     (yaclyaml-parse 'l+block-sequence #?"- sun: yellow\n- ? earth: blue\n  : moon: white\n"))))

(test node-properties
  (is (equal `(:mapping (((:properties (:tag . "tag:yaml.org,2002:str") (:anchor . "a1")) (:content . "foo"))
			 . ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "bar")))
			(((:properties (:anchor . "a2")) (:content . "baz"))
			 . (:alias . "a1")))
	     (yaclyaml-parse 'l+block-mapping #?"!!str &a1 \"foo\":\n  !!str bar\n&a2 baz : *a1\n"))))

(test bare-document  
  (is (equal '((:document "Bare document") 14)
	     (multiple-value-list (yaclyaml-parse 'l-bare-document
						  #?"Bare document\n...\n# No document\n...\n|\n%!PS-Adobe-2.0 # Not the first line\n" :junk-allowed t))))
  ;; (is (equal '("Bare document" 14)
  ;; 	     (multiple-value-list (yaclyaml-parse 'l-bare-document
  ;; 						  #?"# No document\n...\n|\n%!PS-Adobe-2.0 # Not the first line\n" :junk-allowed t))))
  (is (equal `(:document ,#?"%!PS-Adobe-2.0 # Not the first line\n")
	     (yaclyaml-parse 'l-bare-document
			     #?"|\n%!PS-Adobe-2.0 # Not the first line\n")))
  )

(test explicit-documents
  (is (equal '(:document (:mapping ("matches %" . "20")))
	     (yaclyaml-parse 'l-explicit-document
			     #?"---\n{ matches\n% : 20 }\n")))
  ;; (is (equal ""
  ;; 	     (yaclyaml-parse 'l-explicit-document
  ;; 			     #?"---\n# Empty\n")))
  )

(test directive-documents
  (is (equal `(:document ,#?"%!PS-Adobe-2.0\n")
	     (yaclyaml-parse 'l-directive-document
			     #?"%YAML 1.2\n--- |\n%!PS-Adobe-2.0\n")))
  ;; (is (equal #?""
  ;; 	     (yaclyaml-parse 'l-directive-document
  ;; 			     #?"%YAML 1.2\n---# Empty\n")))
  )


(test yaml-stream
  (is (equal '((:document "Document") (:document "another") (:document (:mapping ("matches %" . "20"))))
	     (yaclyaml-parse 'l-yaml-stream
			     #?"Document\n---\nanother\n...\n%YAML 1.2\n---\nmatches %: 20")))
  )
  
				  
;; (test flow-nodes
;;   (is (equal '((:mapping ("YAML" . "separate"))) (yaclyaml-parse 'ns-flow-node #?"!!str \"a\"")))
;;   )
  

(test tag-shorthands
  (is (equal '((:document (((:properties (:tag . "!local")) (:content . "foo"))
			   ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "bar"))
			   ((:properties (:tag . "tag:example.com,2000:app/tag%21")) (:content . "baz")))))
	     (yaclyaml-parse 'l-yaml-stream #?"%TAG !e! tag:example.com,2000:app/\n---
- !local foo\n- !!str bar\n- !e!tag%21 baz")))
  )
  
