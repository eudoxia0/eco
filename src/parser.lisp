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

;;; Pretty-printing

(defmethod print-object ((block <block>) stream)
  (format stream "{~&~A~&}" (body block)))

(defmethod print-object ((statement <statement>) stream)
  (format stream "@~A~{~A~}" (code statement) (body statement)))

#|
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
