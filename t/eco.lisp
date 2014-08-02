(in-package :cl-user)
(defpackage eco-test
  (:use :cl :eco.parser :fiveam))
(in-package :eco-test)

(def-suite parser
  :description "Testing the parser.")
(in-suite parser)

(test parsing
  (is-true (typep (parse-template "232323") 'string))
  (is-true (typep (parse-template "@derp{1 2 3}") '<statement>)))

(run! 'parser)
