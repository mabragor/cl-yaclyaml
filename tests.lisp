(in-package :cl-user)

(defpackage :cl-yaclyaml-tests
  (:use :alexandria :cl :cl-yaclyaml :fiveam :iterate :cl-read-macro-tokens)
  (:export #:run-tests))

(in-package :cl-yaclyaml-tests)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)

(def-suite yaclyaml)
(in-suite yaclyaml)

(defun run-tests ()
  (let ((results (run 'yaclyaml)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test basic
  (is (equal #\tab (yaclyaml-parse 's-white #?"\t")))
  (is (equal #\space (yaclyaml-parse 's-white #?" ")))
  (is (equal nil (yaclyaml-parse 's-white #?"a" :junk-allowed t)))
  (is (equal '(#\tab #\space #\tab) (yaclyaml-parse 's-separate-in-line #?"\t \t")))
  (is (equal nil (yaclyaml-parse 's-separate-in-line #?"")))
  (is (equal nil (yaclyaml-parse 's-separate-in-line #?"\n" :start 1))))

(test indents
  (let ((cl-yaclyaml::n 5)
	(cl-yaclyaml::indent-style :determined))
    (is (equal 3 (yaclyaml-parse 's-indent-<n "   ")))
    (is (equal 4 (yaclyaml-parse 's-indent-<n "    ")))
    (signals (esrap-liquid::esrap-error) (yaclyaml-parse 's-indent-<n "     "))
    (is (equal 5 (yaclyaml-parse 's-indent-<=n "     ")))
    (signals (esrap-liquid::esrap-error) (yaclyaml-parse 's-indent-<n "      "))
    (signals (esrap-liquid::esrap-error) (yaclyaml-parse 's-indent-=n "   "))
    (is (equal 5 (yaclyaml-parse 's-indent-=n "     ")))
    (signals (esrap-liquid::esrap-error) (yaclyaml-parse 's-indent-=n "       "))))

(test flow-line-prefix
  (let ((cl-yaclyaml::n 0))
    (is (equal '(0 (#\space #\space #\space)) (yaclyaml-parse 's-flow-line-prefix #?"   ")))
    (is (equal '(0 (#\tab #\space #\space)) (yaclyaml-parse 's-flow-line-prefix #?"\t  ")))
  (let ((cl-yaclyaml::n 2))
    (is (equal '(2 (#\space)) (yaclyaml-parse 's-flow-line-prefix #?"   ")))
    (signals (esrap-liquid::esrap-error) (yaclyaml-parse 's-flow-line-prefix #?"\t  ")))))

(test b-l-folded
  (is (equal #?"\n\n\n" (let ((cl-yaclyaml::context :flow-out))
			  (yaclyaml-parse 'b-l-folded #?"\n  \n \n\n")))))

(test flow-folded
  (let ((cl-yaclyaml::n 0))
    (is (equal " " (yaclyaml-parse 's-flow-folded #?"\n  ")))
    (is (equal #?"\n" (yaclyaml-parse 's-flow-folded #?" \n \n  \t ")))
    (is (equal #?"\n" (yaclyaml-parse 's-flow-folded #?"\n\n  ")))
    (is (equal " " (yaclyaml-parse 's-flow-folded #?"\n")))))
  

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
  (is (equal #?"text" (let ((cl-yaclyaml::n -1))
			(yaclyaml-parse 'c-l-block-scalar #?"|-\n text\n"))))
  (is (equal #?"text\n" (let ((cl-yaclyaml::n -1))
			  (yaclyaml-parse 'c-l-block-scalar #?"|\ntext\n"))))
  (is (equal #?"text\n" (let ((cl-yaclyaml::n -1))
			  (yaclyaml-parse 'c-l-block-scalar #?"|+\ntext\n"))))
  (is (equal #?"\n\nliteral\n \n\ntext\n"
	     (yaclyaml-parse 'c-l-block-scalar
			     #?"|2\n \n  \n  literal\n   \n  \n  text\n\n\n # Comment\n"))))

(test folded-block-scalars
  (is (equal #?"folded text\n" (yaclyaml-parse 'c-l-block-scalar #?">\n folded text\n\n")))
  (is (equal #?"\nfolded line\nnext line\nlast line\n"
	     (yaclyaml-parse 'c-l-block-scalar
			     #?">\n\n folded\n line\n\n next\n line\n\n last\n line\n
# Comment\n")))
  (is (equal #?"foobar\n  * bullet\n  * list\n\n  * lines\n"
	     (yaclyaml-parse 'c-l-block-scalar
			     #?">\n foobar\n   * bullet\n   * list\n\n   * lines\n"))))
      
(test plain-scalars-simple
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
  (is (equal '(((:properties (:tag . :non-specific)) (:content . "one"))
	       ((:properties (:tag . :non-specific)) (:content . "two")))
	     (yaclyaml-parse 'c-flow-sequence #?"[ one, two, ]")))
  (is (equal '(((:properties (:tag . :non-specific)) (:content . "three"))
	       ((:properties (:tag . :non-specific)) (:content . "four")))
	     (yaclyaml-parse 'c-flow-sequence #?"[three ,four]")))
  (is (equal '(((:PROPERTIES (:TAG . "tag:yaml.org,2002:str")) (:CONTENT . "double quoted"))
	       ((:PROPERTIES (:TAG . "tag:yaml.org,2002:str")) (:CONTENT . "single quoted"))
	       ((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "plain text"))
	       ((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT ((:PROPERTIES (:TAG . :NON-SPECIFIC))
								(:CONTENT . "nested"))))
	       (:MAPPING (((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "single"))
			  (:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "pair"))))
  	     (yaclyaml-parse 'c-flow-sequence
  			     #?"[\n\"double\n quoted\", 'single
       quoted',\nplain\n text, [ nested ],\nsingle: pair,\n]")))
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

(test flow-mapping-nodes-simple
  (is (equal '(:mapping (((:properties (:tag . :non-specific)) (:content . "one"))
			 . ((:properties (:tag . :non-specific)) (:content . "two")))
	       (((:properties (:tag . :non-specific)) (:content . "three"))
			 . ((:properties (:tag . :non-specific)) (:content . "four"))))
	     (yaclyaml-parse 'c-flow-mapping
			     #?"{ one : two , three: four , }")))
  (is (equal '(:mapping (((:properties (:tag . :non-specific)) (:content . "five"))
			 . ((:properties (:tag . :non-specific)) (:content . "six")))
	       (((:properties (:tag . :non-specific)) (:content . "seven"))
			 . ((:properties (:tag . :non-specific)) (:content . "eight"))))
	     (yaclyaml-parse 'c-flow-mapping
			     #?"{five: six,seven : eight}")))
  (is (equal '(:mapping (((:properties (:tag . :non-specific)) (:content . "explicit"))
			 . ((:properties (:tag . :non-specific)) (:content . "entry")))
	       (((:properties (:tag . :non-specific)) (:content . "implicit"))
			 . ((:properties (:tag . :non-specific)) (:content . "entry")))
	       (((:properties (:tag . :non-specific)) (:content . :empty))
		. ((:properties (:tag . :non-specific)) (:content . :empty))))
	     (yaclyaml-parse 'c-flow-mapping
			     #?"{\n? explicit: entry,\nimplicit: entry,\n?\n}"))))

(test tags-simple
  (is (equal '(:tag . "asdf") (yaclyaml-parse 'c-ns-tag-property "!<asdf>")))
  (signals (esrap-liquid::esrap-error) (yaclyaml-parse 'c-ns-tag-property ":")))



(test flow-mapping-nodes-individual-complex
  (is (equal '(:mapping (((:properties (:tag . :non-specific)) (:content . "unquoted"))
			 . ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "separate"))))
	     (yaclyaml-parse 'c-flow-mapping
			     #?"{\nunquoted : \"separate\"}"))))
  
(test flow-mapping-nodes-complex)

(test flow-mapping-nodes-complex
  (is (equal '(:mapping (((:properties (:tag . :non-specific)) (:content . "unquoted"))
			 . ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "separate")))
	       (((:properties (:tag . :non-specific)) (:content . "http://foo.com"))
		. ((:properties (:tag . :non-specific)) (:content . :empty)))
	       (((:properties (:tag . :non-specific)) (:content . "omitted value"))
		. ((:properties (:tag . :non-specific)) (:content . :empty)))
	       (((:properties (:tag . :non-specific)) (:content . :empty))
		. ((:properties (:tag . :non-specific)) (:content . "omitted key")))
	       (((:properties (:tag . "tag:yaml.org,2002:str")) (:content . ""))
		. ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . ""))))
	     (yaclyaml-parse 'c-flow-mapping
			     #?"{\nunquoted : \"separate\",\nhttp://foo.com,
omitted value:,\n: omitted key,'':'',\n}"))))

(test flow-mapping-nodes-next
  (is (equal '(:mapping (((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "adjacent"))
			 . ((:properties (:tag . :non-specific)) (:content . "value")))
	       (((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "readable"))
		. ((:properties (:tag . :non-specific)) (:content . "value")))
	       (((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "empty"))
			 . ((:properties (:tag . :non-specific)) (:content . :empty))))
	     (yaclyaml-parse 'c-flow-mapping
			     #?"{\n\"adjacent\":value,\n\"readable\": value,\n\"empty\":\n}")))
  (is (equal '((:mapping (((:properties (:tag . :non-specific)) (:content . "foo"))
			  . ((:properties (:tag . :non-specific)) (:content . "bar")))))
	     (yaclyaml-parse 'c-flow-sequence
			     #?"[\nfoo: bar\n]")))
  (is (equal '((:mapping (((:properties (:tag . :non-specific)) (:content . "foo bar"))
			  . ((:properties (:tag . :non-specific)) (:content . "baz")))))
	     (yaclyaml-parse 'c-flow-sequence
			     #?"[\n? foo\n bar : baz\n]")))
  (is (equal '((:mapping (((:properties (:tag . :non-specific)) (:content . "YAML"))
			  . ((:properties (:tag . :non-specific)) (:content . "separate")))))
	     (yaclyaml-parse 'c-flow-sequence
			     #?"[ YAML : separate ]")))
  (is (equal '((:mapping (((:properties (:tag . :non-specific)) (:content . :empty))
			  . ((:properties (:tag . :non-specific)) (:content . "empty key entry")))))
	     (yaclyaml-parse 'c-flow-sequence
			     #?"[ : empty key entry ]")))
  (is (equal '((:mapping (((:properties (:tag . :non-specific))
			   (:content . (:mapping (((:properties (:tag . :non-specific)) (:content . "JSON"))
						  . ((:properties (:tag . :non-specific)) (:content . "like"))))))
			  . ((:properties (:tag . :non-specific)) (:content . "adjacent")))))
	     (yaclyaml-parse 'c-flow-sequence
			     #?"[ {JSON: like}:adjacent ]")))
  )

(test block-sequences
  (is (equal `(((:properties (:tag . :non-specific)) (:content . "foo"))
	       ((:properties (:tag . :non-specific)) (:content . "bar"))
	       ((:properties (:tag . :non-specific)) (:content . "baz")))
	     (let ((cl-yy::n -1))
	       (yaclyaml-parse 'l+block-sequence #?"- foo\n- bar\n- baz\n"))))
  (is (equal `(((:properties (:tag . :non-specific))
		(:content . (:mapping (((:properties (:tag . :non-specific)) (:content . "one"))
				       . ((:properties (:tag . :non-specific)) (:content . "two")))))))
	     (let ((cl-yy::n -1))
	       (yaclyaml-parse 'l+block-sequence #?"- one: two # compact mapping\n"))))
  (is (equal `(((:properties (:tag . :non-specific)) (:content . :empty))
	       ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . ,#?"block node\n"))
	       ((:properties (:tag . :non-specific))
		(:content . (((:properties (:tag . :non-specific)) (:content . "one"))
			     ((:properties (:tag . :non-specific)) (:content . "two")))))
	       ((:properties (:tag . :non-specific))
		(:content . (:mapping (((:properties (:tag . :non-specific)) (:content . "one"))
				       . ((:properties (:tag . :non-specific)) (:content . "two")))))))
  	     (let ((cl-yy::n -1))
	       (yaclyaml-parse 'l+block-sequence
			       #?"- # Empty\n- |\n block node\n- - one # Compact\n  - two # sequence\n- one: two\n")))))

(test detect-block-mapping
  (is (equal 0 (let ((cl-yy::n -1)) (yaclyaml-parse 'detect-block-mapping "a" :junk-allowed t))))
  (is (equal 1 (let ((cl-yy::n -1)) (yaclyaml-parse 'detect-block-mapping " a" :junk-allowed t))))
  (is (equal 1 (let ((cl-yy::n 0)) (yaclyaml-parse 'detect-block-mapping " a" :junk-allowed t))))
  (is (equal 0 (let ((cl-yy::n -1)) (yaclyaml-parse 'detect-block-mapping "?" :junk-allowed t))))
  (is (equal nil (let ((cl-yy::n 2)) (yaclyaml-parse 'detect-block-mapping " a" :junk-allowed t)))))

(test block-map-entry
  (is (equal '(((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "explicit key"))
	       (:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . :EMPTY))
	     (let ((cl-yy::n 0)) (yaclyaml-parse 'ns-l-block-map-entry #?"? explicit key # Empty value\n"))))
  (is (equal '(((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "explicit key"))
	       (:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . :EMPTY))
	     (let ((cl-yy::n 0)) (yaclyaml-parse '(progn cl-yy::s-indent-=n cl-yy::ns-l-block-map-entry)
						 #?"? explicit key # Empty value\n"))))
  (is (equal '(((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "explicit key"))
	       (:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . :EMPTY))
	     (let ((cl-yy::n -1)) (yaclyaml-parse '(let ((cl-yy::n cl-yy::detect-block-mapping)
							 (cl-yy::indent-style :determined))
						    (progn cl-yy::s-indent-=n cl-yy::ns-l-block-map-entry))
						  #?"? explicit key # Empty value\n"))))
  (is (equal '((((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "explicit key"))
		(:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . :EMPTY)))
	     (let ((cl-yy::n -1)) (yaclyaml-parse '(let ((cl-yy::n cl-yy::detect-block-mapping)
							 (cl-yy::indent-style :determined))
						    (cl-yy::postimes
						     (progn cl-yy::s-indent-=n cl-yy::ns-l-block-map-entry)))
						  #?"? explicit key # Empty value\n"))))
  (is (equal '(:mapping (((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "explicit key"))
			 (:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . :EMPTY)))
	     (let ((cl-yy::n -1)) (yaclyaml-parse '(let ((cl-yy::n cl-yy::detect-block-mapping)
							 (cl-yy::indent-style :determined))
						    `(:mapping ,.(cl-yy::postimes
								  (progn cl-yy::s-indent-=n
									 cl-yy::ns-l-block-map-entry))))
						  #?"? explicit key # Empty value\n")))))

(test block-mappings-basic
  (is (equal '(((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "one"))
	       ((:PROPERTIES (:TAG . :NON-SPECIFIC))
		(:CONTENT :MAPPING (((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "two"))
				    (:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "three")))))
	     (let ((cl-yy::n 0)) (yaclyaml-parse 'l+block-sequence #?"  - one\n  - two : three\n"))))
  (is (equal '(((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "block sequence"))
	       14)
	     (multiple-value-list 
	      (let ((cl-yy::n 0)
		    (indent-style :determined))
		(yaclyaml-parse 'ns-s-block-map-implicit-key #?"block sequence:\n  - one\n  - two : three\n"
				:junk-allowed t)))))
  (is (equal '((:PROPERTIES (:TAG . :NON-SPECIFIC))
	       (:CONTENT ((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "one"))
		((:PROPERTIES (:TAG . :NON-SPECIFIC))
		 (:CONTENT :MAPPING (((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "two"))
				     (:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "three"))))))
  	     (let ((cl-yy::n 0)
  		   (indent-style :determined))
  	       (yaclyaml-parse 'c-l-block-map-implicit-value #?"block sequence:\n  - one\n  - two : three\n"
  			       :start 14))))
  (is (equal '(((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "block sequence"))
	       ((:PROPERTIES (:TAG . :NON-SPECIFIC))
		(:CONTENT ((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "one"))
		 ((:PROPERTIES (:TAG . :NON-SPECIFIC))
		  (:CONTENT :MAPPING (((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "two"))
				      (:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "three")))))))
  	     (let ((cl-yy::n 0)
  		     (indent-style :determined))
  	       (yaclyaml-parse 'ns-l-block-map-implicit-entry #?"block sequence:\n  - one\n  - two : three\n"))))
  (is (equal '(((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "block sequence"))
	       (:PROPERTIES (:TAG . :NON-SPECIFIC))
	       (:CONTENT ((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "one"))
		((:PROPERTIES (:TAG . :NON-SPECIFIC))
		 (:CONTENT :MAPPING (((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "two"))
				     (:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "three"))))))
	     (let ((cl-yy::n 0)
		   (indent-style :determined))
	       (yaclyaml-parse 'ns-l-block-map-entry #?"block sequence:\n  - one\n  - two : three\n"))))
  (is (equal '(:MAPPING
	       (((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "block sequence"))
		(:PROPERTIES (:TAG . :NON-SPECIFIC))
		(:CONTENT ((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "one"))
		 ((:PROPERTIES (:TAG . :NON-SPECIFIC))
		  (:CONTENT :MAPPING (((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "two"))
				      (:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "three")))))))
	     (let ((cl-yy::n -1))
	       (yaclyaml-parse 'l+block-mapping #?"block sequence:\n  - one\n  - two : three\n")))))


(test block-mappings
  (is (equal `(:mapping (((:properties (:tag . :non-specific)) (:content . "explicit key"))
			 . ((:properties (:tag . :non-specific)) (:content . :empty))))
	     (let ((cl-yy::n -1)) (yaclyaml-parse 'l+block-mapping #?"? explicit key # Empty value\n"))))
  (is (equal `(:mapping (((:properties (:tag . "tag:yaml.org,2002:str")) (:content . ,#?"block key\n"))
			 . ((:properties (:tag . :non-specific)) (:content . "flow value"))))
	     (let ((cl-yy::n -1)) (yaclyaml-parse 'l+block-mapping #?"? |\n  block key\n: flow value\n"))))
  (is (equal `(:mapping (((:properties (:tag . "tag:yaml.org,2002:str")) (:content . ,#?"block key\n"))
			 . ((:properties (:tag . :non-specific))
			    (:content . (((:properties (:tag . :non-specific)) (:content . "one"))
					 ((:properties (:tag . :non-specific)) (:content . "two")))))))
	     (let ((cl-yy::n -1))
	       (yaclyaml-parse 'l+block-mapping #?"? |\n  block key\n: - one # Explicit compact\n  - two # block value\n")))))
  
(test block-mappings-implicit
  (is (equal `(:mapping (((:properties (:tag . :non-specific)) (:content . "block mapping"))
			 . ((:properties (:tag . :non-specific))
			    (:content . (:mapping (((:properties (:tag . :non-specific)) (:content . "key"))
						   . ((:properties (:tag . :non-specific)) (:content . "value"))))))))
	     (let ((cl-yy::n -1))
	       (yaclyaml-parse 'l+block-mapping #?"block mapping:\n key: value\n"))))
  (is (equal `(:mapping (((:properties (:tag . :non-specific)) (:content . "plain key"))
			 . ((:properties (:tag . :non-specific)) (:content . "in-line value")))
			(((:properties (:tag . :non-specific)) (:content . :empty))
			 . ((:properties (:tag . :non-specific)) (:content . :empty)))
			(((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "quoted key"))
			 . ((:properties (:tag . :non-specific))
			    (:content . (((:properties (:tag . :non-specific)) (:content . "entry")))))))
	     (let ((cl-yy::n -1))
	       (yaclyaml-parse 'l+block-mapping #?"plain key: in-line value\n: # Both empty
\"quoted key\":\n- entry\n"))))
  )

(test compact-block-mappings
  (is (equal `(((:properties (:tag . :non-specific))
		(:content . (:mapping (((:properties (:tag . :non-specific)) (:content . "sun"))
				       . ((:properties (:tag . :non-specific)) (:content . "yellow"))))))
	       ((:properties (:tag . :non-specific))
		(:content . (:mapping (((:properties (:tag . :non-specific))
					(:content . (:mapping (((:properties (:tag . :non-specific)) (:content . "earth"))
							       . ((:properties (:tag . :non-specific))
								  (:content . "blue"))))))
				       . ((:properties (:tag . :non-specific))
					  (:content . (:mapping (((:properties (:tag . :non-specific)) (:content . "moon"))
								 . ((:properties (:tag . :non-specific))
								    (:content . "white")))))))))))
	     (let ((cl-yy::n -1))
	       (yaclyaml-parse 'l+block-sequence #?"- sun: yellow\n- ? earth: blue\n  : moon: white\n")))))

(test node-properties
  (is (equal `(:mapping (((:properties (:tag . "tag:yaml.org,2002:str") (:anchor . "a1")) (:content . "foo"))
			 . ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "bar")))
			(((:properties (:tag . :non-specific) (:anchor . "a2")) (:content . "baz"))
			 . (:alias . "a1")))
	     (let ((cl-yy::n -1))
	       (yaclyaml-parse 'l+block-mapping #?"!!str &a1 \"foo\":\n  !!str bar\n&a2 baz : *a1\n")))))

(test bare-document  
  (is (equal '((:document ((:properties (:tag . :non-specific)) (:content . "Bare document"))) 14)
	     (multiple-value-list (yaclyaml-parse 'l-bare-document
						  #?"Bare document\n...\n# No document\n...\n|\n%!PS-Adobe-2.0 # Not the first line\n" :junk-allowed t))))
  ;; bare documents may not be empty!
  (signals (esrap-liquid::esrap-error) (yaclyaml-parse 'l-bare-document
						       #?"# No document\n"))
  (is (equal `(:document ((:properties (:tag . "tag:yaml.org,2002:str"))
			  (:content . ,#?"%!PS-Adobe-2.0 # Not the first line\n")))
	     (yaclyaml-parse 'l-bare-document
			     #?"|\n%!PS-Adobe-2.0 # Not the first line\n")))
  )

(test explicit-documents
  (is (equal '(:document ((:properties (:tag . :non-specific))
			  (:content . (:mapping (((:properties (:tag . :non-specific)) (:content . "matches %"))
						 . ((:properties (:tag . :non-specific)) (:content . "20")))))))
	     (yaclyaml-parse 'l-explicit-document
			     #?"---\n{ matches\n% : 20 }\n")))
  (is (equal '(:DOCUMENT ((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . :EMPTY)))
  	     (yaclyaml-parse 'l-explicit-document
  			     #?"---\n# Empty\n")))
  )

(test directive-documents
  (is (equal `(:document ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . ,#?"%!PS-Adobe-2.0\n")))
	     (yaclyaml-parse 'l-directive-document
			     #?"%YAML 1.2\n--- |\n%!PS-Adobe-2.0\n")))
  (is (equal '(:DOCUMENT ((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . :EMPTY)))
  	     (yaclyaml-parse 'l-directive-document
  			     #?"%YAML 1.2\n---\n# Empty\n")))
  )

(test nested-tag-handles
  (is (equal '(:DOCUMENT ((:PROPERTIES (:TAG . "edcf")) (:CONTENT . "asdf")))
	     (yy-parse 'l-directive-document
		       #?"%TAG !a! !b!c\n%TAG !b! !d\n%TAG ! e\n---\n!a!f asdf"))))

(test yaml-stream
  (is (equal '((:document ((:properties (:tag . :non-specific)) (:content . "Document")))
	       (:document ((:properties (:tag . :non-specific)) (:content . "another")))
	       (:document ((:properties (:tag . :non-specific))
			   (:content . (:mapping (((:properties (:tag . :non-specific)) (:content . "matches %"))
						  . ((:properties (:tag . :non-specific)) (:content . "20"))))))))
	     (yaclyaml-parse 'l-yaml-stream
			     #?"Document\n---\nanother\n...\n%YAML 1.2\n---\nmatches %: 20")))
  (is (equal `((:DOCUMENT ((:PROPERTIES (:TAG . :NON-SPECIFIC)) (:CONTENT . "Bare document")))
	       (:DOCUMENT
		((:PROPERTIES (:TAG . "tag:yaml.org,2002:str"))
		 ;; sadly, block scalars do not strip comment of their contents.
		 (:CONTENT . ,#?"%!PS-Adobe-2.0 # Not the first line\n"))))
	     (yaclyaml-parse 'l-yaml-stream
			     #?"Bare document\n...\n# No document\n...\n|\n%!PS-Adobe-2.0 # Not the first line\n")))
  )

(test tag-handle-compilation
  (is (equal '((:SECONDARY-TAG-HANDLE . "tag:yaml.org,2002:")
	       (:PRIMARY-TAG-HANDLE . "e")
	       ((:NAMED-TAG-HANDLE "a") . "edc") ((:NAMED-TAG-HANDLE "b") . "ed"))
	     (let ((cl-yy::tag-handles (make-hash-table :test #'equal)))
	       (setf (gethash :secondary-tag-handle cl-yy::tag-handles) "tag:yaml.org,2002:"
		     (gethash :primary-tag-handle cl-yy::tag-handles) "!"
		     (gethash '(:named-tag-handle "a") cl-yy::tag-handles) "!b!c"
		     (gethash '(:named-tag-handle "b") cl-yy::tag-handles) "!d"
		     (gethash :primary-tag-handle cl-yy::tag-handles) "e")
	       (cl-yy::compile-tag-handles)
	       (hash->assoc cl-yy::tag-handles)))))

  
(test tag-shorthands
  (is (equal '((:document ((:properties (:tag . :non-specific))
			   (:content . (((:properties (:tag . "!local")) (:content . "foo"))
					((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "bar"))
					((:properties (:tag . "tag:example.com,2000:app/tag%21")) (:content . "baz")))))))
	     (yaclyaml-parse 'l-yaml-stream #?"%TAG !e! tag:example.com,2000:app/\n---
- !local foo\n- !!str bar\n- !e!tag%21 baz")))
  )

(test construction-of-representation-graph
  ;; FIXME: for now such a lame check will suffice, I dunno how to check shared structured-ness easily
  (is (equal `(:mapping (((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "foo"))
			 . ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "bar")))
			(((:properties) (:content . "baz"))
			 . ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "foo"))))
	     (ncompose-representation-graph
	      (copy-tree `(:mapping (((:properties (:tag . "tag:yaml.org,2002:str") (:anchor . "a1")) (:content . "foo"))
				     . ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "bar")))
				    (((:properties (:anchor . "a2")) (:content . "baz"))
				     . (:alias . "a1"))))))))


;;; Generating native language structures from representation graph

;; (defmacro with-flat-nodes ((from to) &body body)
;;   (iter (for i from from to to)
;; 	(collect `(,(sb-int:symbolicate "NODE" (format nil "~a" i))
;; 		    '((:properties) (:content . ,(format nil "~a" i)))) into res)
;; 	(finally (return `(let ,res ,@body)))))

;; (defmacro with-cons-nodes ((from to) &body body)
;;   (iter (for i from from to to)
;; 	(collect `(,(sb-int:symbolicate "CONS-NODE" (format nil "~a" i))
;; 		    (list (list :properties) (list :content))) into res)
;; 	(finally (return `(let ,res ,@body)))))

;; (defmacro link-nodes (which where)
;;   `(push ,which (cdr (assoc :content ,where))))

;; (defun mk-node (num)
;;   (sb-int:symbolicate "NODE" (format nil "~a" num)))

;; (defun mk-cons-node (num)
;;   (sb-int:symbolicate "CONS-NODE" (format nil "~a" num)))


;; (test find-parents
;;   (with-flat-nodes (1 5)
;;     (with-cons-nodes (1 5)
;;       (is (equal
;; 	   (let ((res (make-hash-table :test #'eq)))
;; 	     (setf (gethash cons-node2 res) (list cons-node1)
;; 		   (gethash node1 res) (list cons-node1)
;; 		   (gethash cons-node3 res) (list cons-node2)
;; 		   (gethash node2 res) (list cons-node2)
;; 		   (gethash cons-node4 res) (list cons-node3)
;; 		   (gethash node3 res) (list cons-node3)
;; 		   (gethash cons-node5 res) (list cons-node4)
;; 		   (gethash node4 res) (list cons-node4)
;; 		   (gethash node5 res) (list cons-node5))
;; 	     res)
;; 	   (macrolet ((with-simple-chain ((from to) &body body)
;; 			(iter (for i from from below to)
;; 			      (collect `(link-nodes ,(mk-node (1+ i))
;; 						    ,(mk-cons-node (1+ i))) into res)
;; 			      (collect `(link-nodes ,(mk-cons-node (1+ i))
;; 						    ,(mk-cons-node i)) into res)
;; 			      (finally (return
;; 					 `(progn (link-nodes ,(mk-node from)
;; 							     ,(mk-cons-node from))
;; 						 ,@res
;; 						 (let ((chain ,(mk-cons-node from)))
;; 						   ,@body)))))))
;; 	     (with-simple-chain (1 5)
;; 	       (cl-yaclyaml::find-parents chain))))))))
	     
	
(test scalar-construction-failsafe
  (is (equal '((:content . "asdf") (:tag . :non-specific))
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "asdf")) :schema :failsafe)))
  (is (equal "asdf"
	     (construct '((:properties . ((:tag . "tag:yaml.org,2002:str"))) (:content . "asdf")) :schema :failsafe)))
  (is (equal '((:content . :empty) (:tag . "tag:yaml.org,2002:null"))
	     (construct '((:properties . ((:tag . "tag:yaml.org,2002:null"))) (:content . :empty)) :schema :failsafe))))
  
  
(test scalar-construction-json
  (signals (error "JSON impication of scalar didn't signal an error.")
      (construct '((:properties . ((:tag . :non-specific))) (:content . "asdf")) :schema :json))
  (is (equal "asdf"
	     (construct '((:properties . ((:tag . "tag:yaml.org,2002:str"))) (:content . "asdf")) :schema :json)))
  (is (equal 123
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "123")) :schema :json)))
  (is (equal -3.14
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "-3.14")) :schema :json)))
  (is (equal nil
	     (construct '((:properties . ((:tag . :non-specific))) (:content . :empty)) :schema :json)))
  (is (equal nil
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "null")) :schema :json))))
  

(test scalar-construction-core
  (is (equal "asdf"
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "asdf")) :schema :core)))
  (is (equal "asdf"
	     (construct '((:properties . ((:tag . "tag:yaml.org,2002:str"))) (:content . "asdf")) :schema :core)))
  (is (equal 123
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "123")) :schema :core)))
  (is (equal -3.14
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "-3.14")) :schema :core)))
  (is (equal nil
	     (construct '((:properties . ((:tag . :non-specific))) (:content . :empty)) :schema :core)))
  (is (equal nil
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "null")) :schema :core)))
  (is (equal ""
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "")) :schema :core)))
  (is (equal t
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "true")) :schema :core)))
  (is (equal t
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "True")) :schema :core)))
  (is (equal nil
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "false")) :schema :core)))
  (is (equal nil
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "FALSE")) :schema :core)))
  (is (equal 0
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "0")) :schema :core)))
  (is (equal 7
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "0o7")) :schema :core)))
  (is (equal 58
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "0x3A")) :schema :core)))
  (is (equal -19
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "-19")) :schema :core)))
  (is (equalp 0
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "0.")) :schema :core)))
  (is (equalp 0
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "-0.0")) :schema :core)))
  (is (equal 0.5
	     (construct '((:properties . ((:tag . :non-specific))) (:content . ".5")) :schema :core)))
  (is (equalp 12000
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "+12e03")) :schema :core)))
  (is (equalp -200000
	     (construct '((:properties . ((:tag . :non-specific))) (:content . "-2E+05")) :schema :core)))
  (is (equalp :nan
	      (construct '((:properties . ((:tag . :non-specific))) (:content . ".NAN")) :schema :core)))
  (is (equalp :infinity
	      (construct '((:properties . ((:tag . :non-specific))) (:content . ".Inf")) :schema :core)))
  )

(test simple-sequences
  (is (equal '((:document ("foo" "bar" "baz")))
	     (yaml-load #?"- foo\n- bar\n- baz\n")))
  (is (equal '((:document ((:content . (((:content . "foo") (:tag . :non-specific))
					((:content . "bar") (:tag . :non-specific))
					((:content . "baz") (:tag . :non-specific))))
			   (:tag . :non-specific))))
	     (yaml-load #?"- foo\n- bar\n- baz\n" :schema :failsafe)))
  )

(test simple-sequences-load
  (is (equal '("foo" "bar" "baz")
	     (yaml-simple-load #?"- foo\n- bar\n- baz\n")))
  (is (equal '((:content . (((:content . "foo") (:tag . :non-specific))
			    ((:content . "bar") (:tag . :non-specific))
			    ((:content . "baz") (:tag . :non-specific))))
	       (:tag . :non-specific))
	     (yaml-simple-load #?"- foo\n- bar\n- baz\n" :schema :failsafe)))
  (signals (error "simple load of multiple documents doesn't signal an error.")
	   (cl-yaclyaml::yaml-simple-load #?"- foo\n- bar\n- baz\n...\n- goo...\n- goo"))
  (is (equal '("foo" "bar" ("baz" "baz2" "baz3"))
	     (yaml-simple-load #?"- foo\n- bar\n- - baz\n  - baz2\n  - baz3")))
  )


(test simple-mappings
  (is (equal '(("earth" . "green") ("moon" . "blue") ("sun" . "gold"))
	     (sort (hash->assoc (cadar (yaml-load #?"sun : gold\nearth : green\nmoon : blue")))
		   #'string< :key #'car))))

(defparameter simplest-cyclic (yaml-simple-load #?"&foo\n- 1\n- *foo\n- 2\n- 3"))

(test simple-cyclics
  (is (equal 1 (car simplest-cyclic)))
  (is (equal 2 (caddr simplest-cyclic)))
  (is (equal 3 (cadddr simplest-cyclic)))
  (is (equal nil (nth 4 simplest-cyclic)))
  (is (equal 1 (car (cadr simplest-cyclic)))))

(defparameter alias-mapping
  ;; additional sequence level in all but first map is to ensure actual values of &FOO and &BAR are
  ;; processed after those maps, thus the need to use callback-code is there.
  (let ((loadee (yaml-simple-load #?"- ? &foo a\n  : &bar b
- - ? *foo
    : b
- - ? a
    : *bar
- - ? *foo
    : *bar")))
    (mapcar #'hash->assoc `(,(car loadee) ,@(mapcar #'car (cdr loadee))))))
    
(test alias-mappings
  (is (eq (caar (first alias-mapping)) (caar (second alias-mapping))))
  (is (not (eq (cdar (first alias-mapping)) (cdar (second alias-mapping)))))
  (is (not (eq (caar (first alias-mapping)) (caar (third alias-mapping)))))
  (is (eq (cdar (first alias-mapping)) (cdar (third alias-mapping))))
  (is (eq (caar (first alias-mapping)) (caar (fourth alias-mapping))))
  (is (eq (cdar (first alias-mapping)) (cdar (fourth alias-mapping)))))
  

;;;; tests for process of dumping

(test represent-node
  (is (equal '((:properties (:tag . "tag:yaml.org,2002:int")) (:CONTENT . "123"))
	     (represent-node 123)))
  (is (equal '((:properties (:tag . "tag:yaml.org,2002:float")) (:CONTENT . "12.3"))
	     (represent-node 12.3)))
  (is (equal '((:properties (:tag . "tag:yaml.org,2002:str")) (:CONTENT . "123"))
	     (represent-node "123")))
  (is (equal '((:properties (:tag . "tag:lisp,2013:symbol")) (:CONTENT . "ASDF"))
	     (represent-node 'asdf)))
  (is (equal '((:properties (:tag . "tag:lisp,2013:keyword")) (:CONTENT . "ASDF"))
	     (represent-node :asdf)))
  (is (equal '((:properties (:tag . "tag:yaml.org,2002:seq"))
	       (:content . (((:properties (:tag . "tag:yaml.org,2002:int")) (:CONTENT . "1"))
			    ((:properties (:tag . "tag:yaml.org,2002:int")) (:CONTENT . "2"))
			    ((:properties (:tag . "tag:yaml.org,2002:int")) (:CONTENT . "3")))))
	     (represent-node '(1 2 3))))
  (is (equal '((:properties (:tag . "tag:yaml.org,2002:map"))
	       (:content . (:mapping
			    (((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "e"))
			     . ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "r")))
			    (((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "q"))
			     . ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "w")))
			    (((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "t"))
			     . ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "y"))))))
	     (let ((res (represent-node (let ((a (make-hash-table :test #'equal)))
					  (setf (gethash "q" a) "w"
						(gethash "e" a) "r"
						(gethash "t" a) "y")
					  a))))
	       (setf (cddadr res) (sort (cddadr res) #'string< :key (lambda (x)
								      (cdr (assoc :content (car x))))))
	       res)))
  )

      
(defun gen-shared-sequence1 ()
  (declare (optimize (safety 3) (speed 0)))
  (let ((lst (list `(,(list :properties '(:tag . "tag:yaml.org,2002:str")) (:content . "foo"))
		   `(,(list :properties '(:tag . "tag:yaml.org,2002:str")) (:content . "bar"))
		   `(,(list :properties) (:content . "baz")))))
    `(,(list :properties '(:tag . "tag:yaml.org,2002:seq"))
      (:content . (,. lst ,(car lst))))))

(defun gen-shared-sequence2 ()
  (let* ((list0 (list `(,(list :properties '(:tag . "tag:yaml.org,2002:str")) (:content . "foo"))))
	 (list1 (cons nil list0))
	 (seq `(,(list :properties '(:tag . "tag:yaml.org,2002:seq"))
		 (:content . ,list1))))
    (setf (car list1) seq)
    seq))

(defun gen-shared-mapping1 ()
  (let ((node1 `(,(list :properties '(:tag . "tag:yaml.org,2002:str")) (:content . "foo")))
	(node2 `(,(list :properties '(:tag . "tag:yaml.org,2002:str")) (:content . "bar")))
	(node3 `(,(list :properties) (:content . "baz"))))
    `(,(list :properties '(:tag . "tag:yaml.org,2002:map"))
       (:content . (:mapping (,node1 . ,node2) (,node3 . ,node1))))))

(test serialization
  (is (equal `((:properties (:tag . "tag:yaml.org,2002:seq"))
	       (:content .(((:properties (:anchor . "a1") (:tag . "tag:yaml.org,2002:str")) (:content . "foo"))
			   ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "bar"))
			   ((:properties) (:content . "baz"))
			   (:alias . "a1"))))
	     (nserialize (gen-shared-sequence1))))
  (is (equal '((:properties (:anchor . "a1") (:tag . "tag:yaml.org,2002:seq"))
	       (:content . ((:alias . "a1")
			    ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "foo")))))
	     (nserialize (gen-shared-sequence2))))
  (is (equal `((:properties (:tag . "tag:yaml.org,2002:map"))
	       (:content . (:mapping (((:properties (:anchor . "a1") (:tag . "tag:yaml.org,2002:str")) (:content . "foo"))
				      . ((:properties (:tag . "tag:yaml.org,2002:str")) (:content . "bar")))
				     (((:properties) (:content . "baz"))
				      . (:alias . "a1")))))
	     (nserialize (gen-shared-mapping1)))))


;;; Tests for loading of config files

(defun make-random-file-name (&optional (prefix "/tmp/cl-yy-"))
  (concatenate 'string
	       prefix
	       (iter (for i from 1 to 8)
		     (collect (code-char (+ (char-code #/a) (random 26)))))))

(defmacro with-tmp-file ((var &optional prefix) &body body)
  `(let ((,var (make-random-file-name ,@(if prefix `(,prefix)))))
     (unwind-protect (progn ,@body)
       (delete-file ,var))))

(defparameter *config-string*
  #?"a : b\nc : d\ne : f")

(test yaml-load-file
  (is (equal '(("a" . "b") ("c" . "d") ("e" . "f"))
	     (with-tmp-file (path)
	       (with-open-file (stream path :direction :output :if-exists :supersede)
		 (write-sequence *config-string* stream))
	       (hash->assoc (yaml-load-file path)))))
  (signals (cl-yy::yaml-load-file-error)
    (with-tmp-file (path)
      (with-open-file (stream path :direction :output :if-exists :supersede)
	(write-sequence *config-string* stream))
      (hash->assoc (yaml-load-file path :size-limit 2))))
  (is (equal nil
	     (with-tmp-file (path)
	       (with-open-file (stream path :direction :output :if-exists :supersede)
		 (write-sequence *config-string* stream))
	       (yaml-load-file path :size-limit 2 :on-size-exceed nil)))))
	     
    
