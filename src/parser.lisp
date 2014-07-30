(in-package :cl-user)
(defpackage eco.parser
  (:use :cl :esrap)
  (:export :parse-template))
(in-package :eco.parser)

;;; Block classes

(defclass <tag> () ())

(defclass <expr-tag> (<tag>)
  ((content :reader content
            :initarg :content
            :type string)))

(defclass <no-content-tag> (<tag>)
  ((name :reader name :initarg :name :type string)))

(defclass <content-tag> (<no-content-tag>)
  ((content :reader content :initarg :content :type string)))

(defclass <end-tag> (<no-content-tag>) ())

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
    (list :expt (text text))))

;;; Block tags

(defrule tag-name (+ (not (or "%}" ws))))

(defrule tag-text (+ (not "%}")))

(defrule no-content-tag (and "{%" (* ws) tag-name (* ws) "%}")
  (:destructure (open ws1 name ws2 close)
    (list :nct (text name))))

(defrule content-tag (and "{%" (* ws) tag-name (+ ws) tag-text (* ws) "%}")
  (:destructure (open ws1 name ws2 content ws3 close)
    (list :ct (text name) (text content))))

(defrule end-tag (and "{%" (* ws) "end" tag-name (* ws) "%}")
  (:destructure (open ws1 end text ws2 close)
    (list :et (concatenate 'string "end" (text text)))))

(defrule body-block (+ character)
  (:destructure (&rest text)
    (text text)))

;;; Toplevel rules

(defrule tag (or expr-tag no-content-tag content-tag end-tag))

(defrule expression (or tag body-block))

(defun parse-template (template-string)
  (parse 'expression template-string))
