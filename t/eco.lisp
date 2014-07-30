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
  (is (equal (content (parse-template "<% a b c %>"))
             "a b c ")))

(test content-tag
  (is-true (typep (parse-template "{% a b c %}")
                  '<content-tag>))
  (is-true
   (let* ((parsed (parse-template "{% test 1 2 3 %}"))
          (name (name parsed))
          (content (content parsed)))
     (and (equal name "test")
          (equal content "1 2 3 ")))))

(test end-tag
  (is-true (typep (parse-template "{% endtest %}")
                  '<end-tag>))
  (is (equal (name (parse-template "{% endtest %}"))
             "test")))

(run! 'parser)
