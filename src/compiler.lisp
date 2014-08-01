(in-package :cl-user)
(defpackage eco.compiler
  (:use :cl :eco.parser)
  (:export :compile-template))
(in-package :eco.compiler)

(defparameter +template-def+
"(defun ~A (~A)
  (with-output-to-string (*eco-parser*) ~{~A ~}))")

(defmethod emit ((str string)) str)

(defun emit-expression (expr)
  (format nil "(format *eco-stream* \"~~A\" ~A)"
          (emit expr)))

(defun emit-statement (code body)
  (format nil "(format *eco-stream* \"~~A\" (~A ~{~A ~})"
          code body))

(defmethod emit ((statement <statement>))
  (cond
    ((equal (code statement) "")
     ;; The code is empty, so the statement is of the form '@{block}'. In these
     ;; cases, what we do is just emit the first block with not parentheses
     (emit-expression (first (body statement))))
    (t
     ;; Arbitrary statement. The code of the form '@<code><block>+' becomes
     ;; (<code< <block>+).
     (emit-statement (code statement) (body statement)))))

(defun compile-template (element)
  (read-from-string (emit element)))
