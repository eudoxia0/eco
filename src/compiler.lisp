(in-package :cl-user)
(defpackage eco.compiler
  (:use :cl :eco.parser)
  (:export :compile-template))
(in-package :eco.compiler)

;;; Utilities

(defun code-fn (str)
  "\"herp derp berp\" -> \"herp\""
  (subseq str 0 (position #\Space str)))

(defun code-args (str)
  "\"herp derp berp\" -> \"derp berp\""
  (subseq str (1+ (position #\Space str))))

(defparameter +template-def+
"(defun ~A (~A)
  (with-output-to-string (*eco-stream*) ~A))")

(defun define-template (name arg-text body)
  (format nil +template-def+ name arg-text (emit body)))

(defun emit-expression (expr)
  (format nil "~A" (emit expr)))

(defun emit-statement (code body)
  (format nil "(~A ~{~A~})" code (mapcar #'(lambda (elem) (emit elem)) body)))

;;; Compiler

(defmethod emit ((str string))
  (format nil "(write-string ~S *eco-stream*)" str))

(defmethod emit ((list list))
  (format nil "(progn ~{~A ~})" (mapcar #'(lambda (elem) (emit elem))
                                       list)))

(defmethod emit ((block <block>))
  (let ((body (body block)))
    (if (typep body 'string)
        body
        (format nil "~{~A ~}"
                (mapcar #'(lambda (elem) (emit elem))
                        body)))))

(defmethod emit ((statement <statement>))
  (cond
    ((equal (code statement) "")
     ;; The code is empty, so the statement is of the form '@{block}'. In these
     ;; cases, what we do is just emit the first block with not parentheses
     (emit-expression (first (body statement))))
    ((equal (code-fn (code statement)) "template")
     ;; Template definition
     ;; The `let` below may be a little confusing
     (let ((template-name (code-fn (code-args (code statement))))
           (template-args (code-args (code-args (code statement)))))
       (define-template template-name template-args (body statement))))
    (t
     ;; Arbitrary statement. The code of the form '@<code><block>+' becomes
     ;; (<code> <block>+).
     (emit-statement (code statement) (body statement)))))

(defun emit-toplevel (code)
  (format nil "~{~A~%~}"
          (loop for elem in code collecting
                (if (typep elem '<statement>)
                    (emit elem)
                    ""))))

;;; Packages

(defun insert-package (code package-name)
  (declare (type string code))
  (concatenate 'string
               (format nil "(in-package ~A)~%~%" package-name)
               code))

;;; Interface

(defun compile-template (element &optional package-name)
  (if package-name
      (insert-package (emit-toplevel element)
                      package-name)
      (emit-toplevel element)))
