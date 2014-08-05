(in-package :cl-user)
(defpackage eco.parser
  (:use :cl :esrap)
  (:export :<block>
           :<statement>
           :code
           :body
           :parse-template))
(in-package :eco.parser)

;;; Element classes

(defclass <block> ()
  ((body :reader body :initarg :body)))

(defclass <statement> ()
  ((code :reader code :initarg :code)
   (body :reader body :initarg :body)))

;;; Parsing rules

(defrule ws (+ (or #\Space #\Tab #\Newline #\Linefeed #\Backspace
                   #\Page #\Return #\Rubout)))

;; Block: { ... }
(defrule block (and "{" expression "}" (* ws))
  (:destructure (open body close ws)
    (declare (ignore open close ws))
    (make-instance '<block> :body body)))

;; Statement: @...{ ... }
(defrule code-char (not (or "{" "}")))

(defrule statement (and "@" (* code-char) (+ block))
  (:destructure (at code body)
    (declare (ignore at))
    (make-instance '<statement>
                   :code (text code)
                   :body body)))

;; Raw text
(defrule raw-text (* (not (or "{" "}")))
  (:destructure (&rest text)
    (text text)))

(defrule expression (or statement raw-text))

;;; Pretty-printing

(defmethod print-object ((block <block>) stream)
  (format stream "{~&~A~&}" (body block)))

(defmethod print-object ((statement <statement>) stream)
  (format stream "@~A~{~A~}" (code statement) (body statement)))

;;; Interface

(defun parse-template (template-string)
  (parse 'expression template-string))
