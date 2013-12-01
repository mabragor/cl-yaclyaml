cl-yaclyaml
===========

Yet Another Common Lisp YaML processor (so far, only loader, but not dumper)

Tries to adhere to YAML 1.2 specification (see www.yaml.org)

High-level interface is provided by exported YAML-LOAD function.

```lisp
CL-USER> (ql:quickload 'cl-yaclyaml)
CL-USER> (ql:quickload 'cl-interpol)
CL-USER> (cl-interpol:enable-interpol-syntax)
CL-USER> (cl-yy:yaml-load #?"- foo\n- bar\n- baz\n")
((:DOCUMENT ("foo" "bar" "baz")))
```

If you are sure, that your stream contains only one YAML document, use can also use

```lisp
CL-USER> (cl-yy:yaml-simple-load #?"- foo\n- bar\n- baz\n")
("foo" "bar" "baz")

Loader supports optional SCHEMA keyword parameter, which can now be :FAILSAFE :JSON and :CORE (default)
and affects tag implication and resolution.

Technically, process of loading is done in three stages: parsing of raw-text, composing of representation graph
and construction of native language structures.

What remains to be done:
  * :LISP tag-resolution schema, which would support lisp-specific data types, such as symbols, arrays, alists and so on.
  * user-friendly errors, when parsing fails
  * parsing not only of strings, but also of streams
  * YAML-DUMP, the complement of YAML-LOAD
  * UTF-16 and UTF-32 support (Yaml processor should support them, ouch)