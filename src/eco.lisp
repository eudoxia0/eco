(in-package :cl-user)
(defpackage eco
  (:use :cl)
  (:export :compile-string
           :compile-pathname))
(in-package :eco)

(defun generic-compile (ast)
  (eval (eco.compiler:compile-template ast)))

(defun compile-string (string)
  (generic-compile (eco.parser:parse-template string)))

(defun compile-pathname (pathname)
  (generic-compile (eco.parser:parse-pathname pathname)))
