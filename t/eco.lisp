(in-package :cl-user)
(defpackage eco-test
  (:use :cl :eco.parser :eco.compiler :fiveam))
(in-package :eco-test)

(def-suite parser)
(in-suite parser)

(defmacro is-type (expr type)
  `(is-true (typep ,expr ',type)))

(test rules
  (equal (esrap:parse 'eco.parser::block-string " 1 2 3") " 1 2 3")
  (equal (esrap:parse 'eco.parser::block-string "(fn (f (g 1) (h 1)))")
         "(fn (f (g 1) (h 1)))")
  (is-type (esrap:parse 'eco.parser::tag "<%@ test %>") <expr-tag>)
  (is-type (esrap:parse 'eco.parser::tag "<% test %>") eco.parser::<block-tag>)
  (is-type (esrap:parse 'eco.parser::raw-text "1") string)
  (is-type (esrap:parse 'eco.parser::raw-text "232323") string))

(test parsing
  (is-type (parse-template "232323") vector)
  (is-type (elt (parse-template "232323") 0) string)
  (is-type (elt (parse-template "a b c") 0) string))

(def-suite compiler)
(in-suite compiler)

;(test compiling
;  (is (equal (eco-template::test t "test" (list 1 2 3) nil)
;             (format nil "~%tb~%test~%"))))

(run! 'parser)
(run! 'compiler)
