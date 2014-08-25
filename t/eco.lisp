(in-package :cl-user)
(defpackage eco-test
  (:use :cl :eco.parser :eco.compiler :fiveam))
(in-package :eco-test)

(def-suite parser)
(in-suite parser)

(defmacro is-type (expr type)
  `(is-true (typep ,expr ',type)))

(test rules
  (is-type (esrap:parse 'eco.parser::block "{1 2 3}") <block>)
  (is-type (esrap:parse 'eco.parser::block "{1 2 3}") <block>)
  (is-type (esrap:parse 'eco.parser::code-char "1") character)
  (is-type (esrap:parse 'eco.parser::raw-text "1") string)
  (is-type (esrap:parse 'eco.parser::raw-text "232323") string))

(test parsing
  (is-type (first (parse-template "232323")) string)
  (is-type (first (parse-template "@derp{1 2 3}")) <statement>)
  (is-type (first (parse-template "@derp{a}{b}")) <statement>)
  (is-type (first (parse-template "@derp{a}{b}")) <statement>)
  (is-type (first (parse-template "@a{b}{@c{d}{e}}")) <statement>))

(def-suite compiler)
(in-suite compiler)

(test compiling
  (is (equal (eco-template::test t "test" (list 1 2 3) nil)
             (format nil "~%tb~%test~%"))))

(run! 'parser)
(run! 'compiler)
