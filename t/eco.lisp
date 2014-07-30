(in-package :cl-user)
(defpackage eco-test
  (:use :cl :eco.parser :fiveam))
(in-package :eco-test)

(def-suite parser
  :description "Testing the parser.")
(in-suite parser)

(test expression-tag
  (is-true (typep (parse-template "<%%>")
                  '<expr-tag>))
  (is-true (typep (parse-template "<% a b c %>")
                  '<expr-tag>))
  (is (equal "a b c "
             (content (parse-template "<% a b c %>")))))

(test content-tag
  (is-true (typep (parse-template "{% a b c %}")
                  '<content-tag>)))

(run! 'parser)
