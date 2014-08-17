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
  (with-output-to-string (*eco-parser*) ~{~A ~}))")

(defun define-template (name arg-text body)
  (format nil +template-def+ name arg-text (emit body)))

(defun emit-expression (expr)
  (format nil "(format *eco-stream* \"~~A\" ~A)"
          (emit expr)))

(defun emit-statement (code body)
  (format nil "(format *eco-stream* \"~~A\" (~A ~{~A ~}))"
          code (emit body)))

;;; Compiler

(defmethod emit ((str string)) str)

(defmethod emit ((list list))
  (mapcar #'(lambda (elem) (emit elem)) list))

(defmethod emit ((block <block>))
  (emit (body block)))

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
     ;; (<code< <block>+).
     (emit-statement (code statement) (body statement)))))

;;; Packages

(defun insert-package (code package-def)
  (declare (type string code))
  (concatenate 'string
               (format t "~A~&(in-package ~A)~&"
                       `(cl:defpackage ,@package-def)
                       (first package-def))
               code))

(defun compile-template (element &optional package-def)
  (if package-def
      (insert-package (emit element)
                      package-def)
      (emit element)))
