
;;; eco-template: The package where templates are compiled

(defpackage eco-template
  (:use :cl)
  (:export :deftemplate
           :e))
(in-package :eco-template)

(defun e (string)
  "Escape a string."
  (who:escape-string string))

(defmacro deftemplate (name args (&key (escape-html t)) &rest body)
  `(progn
     (defun ,name ,args
       (with-output-to-string (%eco-stream)
         ,(if (not escape-html)
              `(flet ((e (string)
                        string))
                 ,@body)
              `(progn
                 ,@body))))
     (compile ',name)
     (export ',name (symbol-package ',name))))

(defpackage eco.compiler
  (:use :cl :eco.parser :eco-template)
  (:import-from :split-sequence
                :split-sequence-if)
  (:import-from :eco-template
                :%eco-stream)
  (:export :compile-template))
(in-package :eco.compiler)

(defparameter *template-package*
  (find-package 'eco-template))

(defun read-template-expressions (string)
  (let ((*package* *template-package*)
        (end '#:eof))
    (with-input-from-string (s string)
      (loop
         for form = (read s nil end)
         until (eq form end)
         collect form))))

;;; Compiler

(defmethod emit ((str string))
  `(write-string ,str %eco-stream))

(defmethod emit ((vec vector))
  `(progn ,@(loop for elem across vec collecting (emit elem))))

(defmethod emit ((expr <expr-tag>))
  (let ((expressions (read-template-expressions (code expr))))
    (assert (= 1 (length expressions))
            (expressions) "Only one expression is allowed in <%= <expr> %>, got ~{~S~%~}"
            expressions)
    (alexandria:with-unique-names (string-stream)
      `(write-string (e (with-output-to-string (,string-stream)
                          (princ ,(first expressions) ,string-stream)))
                     %eco-stream))))

(defun else-tag-p (element)
  (typep element '<else-tag>))

(defmethod emit ((block <block>))
  (let ((body (body block)))
    (let ((else-tag-pos (position-if 'else-tag-p body)))
      `(,@(read-template-expressions (code block))
        ,@(loop
             for elem in (if else-tag-pos
                             (split-sequence-if 'else-tag-p body)
                             (coerce body 'list))
             collecting (emit elem))))))

(defmethod emit ((call <call>))
  `(princ (,@(read-template-expressions (code call))
           ,@(let ((children (loop
                                for elem across (body call)
                                collecting (emit elem))))
               (and children
                    `((with-output-to-string (%eco-stream)
                        ,@children)))))
          %eco-stream))

(defun template-element-p (element)
  (typep element '<block>))

(defun emit-toplevel (code)
  `(progn ,@(map 'list 'emit (remove-if-not 'template-element-p code))))

;;; Interface

(defun compile-template (element &optional (package-name 'eco-template))
  (let ((*template-package* (find-package package-name)))
    (emit-toplevel element)))

