(in-package :cl-user)
(defpackage eco-test
  (:use :cl :eco.parser :fiveam))
(in-package :eco-test)

(def-suite parser
  :description "Testing the parser.")
(in-suite parser)

(test expressions
  (is-true (typep (parse-template "<%%>")
                  '<expr-tag>)))

(run! 'parser)
