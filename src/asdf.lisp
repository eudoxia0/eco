;;;; This is largely structured around the CFFI Groveller:
;;;;
;;;;     https://github.com/cffi/cffi/blob/master/grovel/asdf.lisp
;;;;

;;; Define the default package for templates
(in-package :cl-user)
(defpackage eco-template
  (:use :cl))

(in-package :asdf)

(defclass eco-template (source-file)
  ((type :initform "eco")
   (package :initform :eco-template
            :initarg :package
            :reader template-package)))

(defmethod output-files ((op compile-op) (component eco-template))
  (let* ((input-path (component-pathname component))
         (output-path (make-pathname :type "lisp"
                                     :defaults input-path)))
    (list output-path)))

(defmethod perform ((op compile-op) (component eco-template))
  (let ((compiled-template-path (first (output-files (make-instance 'compile-op)
                                                     component))))
    (with-open-file (stream compiled-template-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string (eco.compiler:compile-template
                     (eco.parser:parse-pathname
                      (component-pathname component))
                     (template-package component))
                     stream))))

(defmethod perform ((op load-op) (component eco-template))
  (let ((compiled-template-path (first (output-files (make-instance 'load-op)
                                                     component))))
    (perform op (make-instance 'cl-source-file
                               :name (component-name component)
                               :parent (component-parent component)
                               :pathname compiled-template-path))))

(import 'eco-test :asdf)
