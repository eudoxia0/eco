(in-package :cl-user)
(defpackage eco.compiler
  (:use :cl :eco.parser))
(in-package :eco.compiler)

(defmethod emit ((tag <expr-tag>))
  (format nil "(format *eco-stream* \"~~A\" (~A))" (content tag)))

(defmethod emit ((block <block>))
  (format nil "(format *eco-stream* \"~~A\" (~A ~A ~{~A ~}))"
          (name block) (content block) (body block)))
