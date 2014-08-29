(in-package :cl-user)
(defpackage eco.compiler
  (:use :cl :eco.parser)
  (:export :compile-template))
(in-package :eco.compiler)

;;; Compiler

(defmethod emit ((str string))
  (format nil "(write-string ~S *eco-stream*)" str))

(defmethod emit ((vec vector))
  (format nil "(progn ~{~A ~})"
          (loop for elem across vec collecting (emit elem))))

(defmethod emit ((expr <expr-tag>))
  (format nil "(write-string ~A *eco-stream*)" (code expr)))

(defmethod emit ((else <else-tag>)) "")

(defmethod emit ((block <block>))
  (let ((body (body block)))
    (if (typep body 'string)
        body
        (format nil "(~A ~{~A ~})" (code block)
                (loop for elem across body collecting (emit elem))))))

(defun emit-toplevel (code)
  (format nil "~{~A~%~}"
          (loop for elem across code collecting
            (if (typep elem '<block>)
                (emit elem)
                ""))))

;;; Packages

(defun insert-package (code package-name)
  (declare (type string code))
  (concatenate 'string
               (format nil "(in-package :~A)~%~%" package-name)
               code))

;;; Interface

(defun compile-template (element &optional package-name)
  (if package-name
      (insert-package (emit-toplevel element)
                      package-name)
      (emit-toplevel element)))

;;; eco-template: The package where templates are compiled

(defpackage eco-template
  (:use :cl)
  (:export :deftemplate))
(in-package :eco-template)

(defmacro deftemplate (name args &rest body)
  `(defun ,name ,args
     (with-output-to-string (*eco-stream*)
       ,@body)))
