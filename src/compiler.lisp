(in-package :cl-user)
(defpackage eco.compiler
  (:use :cl :eco.parser))
(in-package :eco.compiler)

(defmethod emit ((tag <expr-tag>))
  (format nil "(format *eco-stream* \"~~A\" (~A))" (content tag)))
