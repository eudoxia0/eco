(in-package :cl-user)
(defpackage eco.parser
  (:use :cl :esrap))
(in-package :eco.parser)

;;; Utility rules

(defparameter +whitespace-chars+
  (list #\Space #\Tab #\Newline #\Linefeed #\Backspace
        #\Page #\Return #\Rubout))

(defrule ws (+ (or #\Space #\Tab #\Newline #\Linefeed #\Backspace
                   #\Page #\Return #\Rubout))
  (:constant nil))

;;; Expression tags

(defrule expr-tag-char (not "%>"))

(defrule expr-tag (and "<%" (* ws) (* expr-tag-char) (* ws) "%>")
  (:destructure (open ws1 text ws2 close)
    (text text)))

;;; Block tags

(defrule block-tag-char (not "%}"))

(defmacro no-content-tag (name)
  "Return an esrap matcher for a content-free tag of a given name."
  `("{%" (* ws) ,name (* ws) "%}"))

(defmacro content-tag (name)
  "Return an esrap matcher for a tag of a given name."
  `("{%" (* ws) ,name (+ ws) (* block-tag-char) (* ws) "%}"))

(defmacro end-tag (name)
  "Esrap matcher for an end tag of a given name."
  `("{%" (* ws) ,(concatenate 'string "end" name) (* ws) "%}"))

(defmacro body-block ()
  "Return an esrap matcher for a body"
  `(+ character))

(defmacro define-block (rule-name (&rest rules)
                                  (&rest args)
                                  &rest destructure)
  `(defrule ,rule-name (and ,rules)
     (:destructure ,args ,destructure)))

(define-block simple-if ((content-tag "if")
                         (body-block)
                         (end-tag "if"))
    (
