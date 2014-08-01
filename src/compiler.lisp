(in-package :cl-user)
(defpackage eco.compiler
  (:use :cl :eco.parser)
  (:export :compile-template))
(in-package :eco.compiler)

(defmethod emit ((str string)) str)

(defmethod emit ((tag <expr-tag>))
  (format nil "(format *eco-stream* \"~~A\" (~A))" (content tag)))

(defun emit-list (list)
  (mapcar #'(lambda (elem) (emit elem)) list))

(defparameter +template-def+
"(defun ~A (~A)
  (with-output-to-string (*eco-parser*) ~{~A ~}))")

(defun emit-template-definition (name arg-text body)
  (format nil +template-def+ name arg-text (emit-list body)))

(defmethod emit ((block <block>))
  (if (equal (name block) "template")
      (let* ((text (content block))
             (template-name (subseq text
                                    0
                                    (position #\Space text)))
             (arg-text (subseq text (1+ (position #\Space text)))))
        (emit-template-definition template-name arg-text (body block)))
      (format nil "(format *eco-stream* \"~~A\" (~A ~A ~{~A ~}))"
              (name block)
              (content block)
              (emit-list (body block)))))

(defun compile-template (node)
  (read-from-string (emit node)))
