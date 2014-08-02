(in-package :cl-user)
(defpackage eco.parser
  (:use :cl :esrap)
  (:export :parse-template))
(in-package :eco.parser)

;;; Element classes

(defclass <block> ()
  ((body :reader body :initarg :body)))

(defclass <statement> ()
  ((code :reader code :initarg :code)
   (body :reader body :initarg :body)))

;;; Parsing rules

;; Block: { ... }
(defrule block (and "{" expression "}")
  (:destructure (open body close)
    (make-instance '<block> :body body)))

;; Statement: @...{ ... }
(defrule code-char (not (or "{" "}")))

(defrule statement (and "@" (* code-char) (+ block))
  (:destructure (at code body)
    (make-instance '<statement>
                   :code (text code)
                   :body body)))

;; Raw text
(defrule raw-text (* (not (or "{" "}")))
  (:destructure (&rest text)
    (text text)))

(defrule expression (or statement raw-text))

#|

;;; Pretty-printing

(defmethod print-object ((tag <expr-tag>) stream)
  (format stream "<% ~A%>" (content tag)))

(defmethod print-object ((tag <content-tag>) stream)
  (format stream "{% ~A ~A%}" (name tag) (content tag)))

(defmethod print-object ((tag <end-tag>) stream)
  (format stream "{% end~A %}" (name tag)))

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

(defrule body-block (+ (not (and (or "<" "{") "%")))
  (:destructure (&rest text)
    (text text)))

(defrule tag (or expr-tag end-tag content-tag))

(defrule expression (* (or tag body-block)))

(defun contentp (tok)
  (typep tok '<content-tag>))

(defun delimp (tok end-name)
  (and (typep tok '<end-tag>)
       (equal (name tok) end-name)))

(defun process-tokens (tokens)
  (labels ((next-token ()
             (prog1 (first tokens)
               (setf tokens (rest tokens))))
           (parse-tokens (&optional end-name)
             (let ((list (list))
                   (tok (next-token)))
               (loop while (and tok
                                (if end-name
                                    (not (delimp tok end-name))
                                    t))
                 do
                 (push
                  (cond
                    ((contentp tok)
                     ;; Start a block
                     (make-instance '<block>
                                    :name (name tok)
                                    :content (content tok)
                                    :body (parse-tokens (name tok))))
                    ((and end-name (delimp tok end-name))
                     (error "Parsing error: Early termination."))
                    ((typep tok '<end-tag>)
                     (error "Parsing error."))
                    (t
                     tok))
                  list)
                 (setf tok (next-token)))
               (reverse list))))
    (first (parse-tokens))))

(defun parse-template (template-string)
  (process-tokens (parse 'expression template-string)))
|#
