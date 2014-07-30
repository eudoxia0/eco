(in-package :cl-user)
(defpackage eco.parser
  (:use :cl :esrap)
  (:export :<tag>
           :<expr-tag>
           :<content-tag>
           :<end-tag>
           :<block>
           :name
           :content
           :body
           :parse-template))
(in-package :eco.parser)

;;; Block classes

(defclass <tag> () ())

(defclass <expr-tag> (<tag>)
  ((content :reader content
            :initarg :content
            :type string)))

(defclass <content-tag> (<tag>)
  ((name :reader name :initarg :name :type string)
   (content :reader content :initarg :content :type string)))

(defclass <end-tag> (<no-content-tag>)
  ((name :reader name :initarg :name :type string)))

(defclass <block> (<content-tag>)
  ((body :reader body :initarg :body)))

;;; Pretty-printing

(defmethod print-object ((tag <expr-tag>) stream)
  (format stream "<% ~A%>" (content tag)))

(defmethod print-object ((tag <content-tag>) stream)
  (format stream "{% ~A ~A%}" (name tag) (content tag)))

(defmethod print-object ((tag <end-tag>) stream)
  (format stream "{% end~A%}" (name tag)))

(defmethod print-object ((block <block>) stream)
  (format stream "{% ~A ~A%}~&~{~A~&~}{% end~A%}"
          (name block) (content block) (body block)
          (name block)))

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
    (make-instance '<expr-tag> :content (text text))))

;;; Block tags

(defrule tag-name (+ (not (or "%}" ws))))

(defrule tag-text (* (not "%}")))

(defrule content-tag (and "{%" (* ws) tag-name (+ ws) tag-text (* ws) "%}")
  (:destructure (open ws1 name ws2 content ws3 close)
    (make-instance '<content-tag>
                   :name (text name)
                   :content (text content))))

(defrule end-tag (and "{%" (* ws) "end" tag-name (* ws) "%}")
  (:destructure (open ws1 end text ws2 close)
    (make-instance '<end-tag>
                   :name (text text))))

(defrule body-block (+ character)
  (:destructure (&rest text)
    (text text)))

(defrule tag (or end-tag expr-tag content-tag))

(defrule expression (* (or tag block body-block)))

(defrule block (and content-tag expression end-tag)
  (:destructure (ct body et)
    (if (equal (name ct) (name et))
        (make-instance '<block>
                       :name (name ct)
                       :content (content ct)
                       :body body)
        (error "End tag does not match start tag."))))

(defun parse-template (template-string)
  (let ((parsed (parse 'expression template-string)))
    (if (rest parsed)
        (make-instance '<block>
                       :name "progn"
                       :content ""
                       :body parsed)
        (first parsed))))
