(in-package :cl-user)

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
     (export ',name (find-package 'eco-template))))

(defpackage eco.compiler
  (:use :cl :eco.parser)
  (:import-from :split-sequence
                :split-sequence-if)
  (:export :compile-template))
(in-package :eco.compiler)

;;; Compiler

(defmethod emit ((str string))
  (format nil "(write-string ~S eco-stream)" str))

(defmethod emit ((vec vector))
  (format nil "(progn ~{~A ~})"
          (loop for elem across vec collecting (emit elem))))

(defmethod emit ((expr <expr-tag>))
  (format nil "(write-string (e ~A) eco-stream)" (code expr)))

(defmethod emit ((else <else-tag>)) "")

(defmethod emit ((block <block>))
  (let ((body (body block)))
    (if (typep body 'string)
        body
        (let ((else-tag-pos (position nil body :test #'(lambda (a b)
                                                         (typep b '<else-tag>)))))
          (if else-tag-pos
              ;; We have an else tag
              (format nil "(~A ~{~A ~})"
                      (code block)
                      (loop for elem in (split-sequence-if #'(lambda (elem)
                                                               (typep elem '<else-tag>))
                                                           body)
                            collecting (format nil "(progn ~A)" (emit elem))))
              ;; Just a regular block
              (format nil "(~A ~{~A ~})"
                      (code block)
                      (loop for elem across body collecting (emit elem))))))))


(defun emit-toplevel (code)
  (format nil "(progn ~{~A~%~})"
          (loop for elem across code collecting
            (if (typep elem '<block>)
                (emit elem)
                ""))))

;;; Interface

(defun read-string-in-package (string package-name)
  (let ((cur-package *package*))
    (setf *package* (find-package package-name))
    (prog1
        (read-from-string string)
      (setf *package* cur-package))))

(defun compile-template (element &optional (package-name 'eco-template))
  (read-string-in-package (emit-toplevel element) package-name))

