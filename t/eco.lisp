(in-package :cl-user)
(defpackage eco-test
  (:use :cl :eco.parser :fiveam))
(in-package :eco-test)

(def-suite parser
  :description "Testing the parser.")
(in-suite parser)

(test rules
  (is-true (typep (esrap:parse 'eco.parser::block "{1 2 3}") '<block>))
  (is-true (typep (esrap:parse 'eco.parser::block "{1 2 3}   ") '<block>)))

(test parsing
  (is-true (typep (parse-template "") 'string))
  (is-true (typep (parse-template "232323") 'string))
  (is-true (typep (parse-template "@derp{1 2 3}") '<statement>))
  (is-true (typep (parse-template "@derp{a}{b}") '<statement>)))

(run! 'parser)
