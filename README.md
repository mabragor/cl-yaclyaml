cl-yaclyaml
===========

Yet Another Common Lisp YaML processor (so far, only loader, but not dumper)

Now completely implementation-independent!

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
```

Loader supports optional SCHEMA keyword parameter, which can now be :FAILSAFE :JSON and :CORE (default)
and affects tag implication and resolution.

Technically, process of loading is done in three stages: parsing of raw-text, composing of representation graph
and construction of native language structures.


Also, (mainly for loading of config files) there is YAML-LOAD-FILE function

```lisp
CL-USER> (cl-yy::yaml-load-file "~/.my-config.yml" :size-limit 100 :on-size-exceed :warn)
```
where SIZE-LIMIT (default 1024) is the critical size of file in bytes,
 above which parsing will not be performed
(so as not to be DDoS'ed).

ON-SIZE-EXCEED can be either :ERROR (default), or :WARN or NIL. In case of :ERROR YAML-LOAD-FILE-ERROR
is signalled, in case of :WARN warning is printed and NIL is returned, as if config file was absent,
and in case of NIL, NIL is silently returned with no warning whatsoever.


TODO:
-----

  * :LISP tag-resolution schema, which would support lisp-specific data types, such as symbols, arrays, alists and so on.
  * user-friendly errors, when parsing fails
  * (done, thanks to upgrade of ESRAP-LIQUID) parsing not only of strings, but also of streams
  * YAML-DUMP, the complement of YAML-LOAD
  * UTF-16 and UTF-32 support (Yaml processor should support them, ouch)