(in-package :cl-user)
(defpackage eco.parser
  (:use :cl :esrap)
  (:export :<block>
           :<statement>
           :code
           :body
           :parse-template
           :parse-pathname))
(in-package :eco.parser)

;;; Utilities

(defun one-or-many (list)
  "If the list has one element, return it. If it has more than one, return the
list."
  (if (rest list)
      list
      (first list)))

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
(defrule block (and "{" (+ expression) "}" (* ws))
  (:destructure (open body close ws)
    (declare (ignore open close ws))
    (make-instance '<block> :body (one-or-many body))))

;; Statement: @...{ ... }
(defrule code-char (not (or "{" "}")))

(defrule statement (and "@" (* code-char) (+ block))
  (:destructure (at code body)
    (declare (ignore at))
    (make-instance '<statement>
                   :code (text code)
                   :body body)))

;; Raw text
(defrule raw-text (+ (not "}"))
  (:destructure (&rest text)
    (text text)))

(defrule expression (+ (or statement raw-text)))

;;; Pretty-printing

(defmethod print-object ((block <block>) stream)
  (format stream "{~&~A~&}" (body block)))

(defmethod print-object ((statement <statement>) stream)
  (format stream "@~A~{~A~}" (code statement) (body statement)))

;;; Interface

(defun parse-template (template-string)
  (one-or-many (parse 'expression template-string)))

(defun slurp-file (path)
  ;; Credit: http://www.ymeme.com/slurping-a-file-common-lisp-83.html
  (with-open-file (stream path)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

(defun parse-pathname (template-pathname)
  (parse-template (slurp-file template-pathname)))
