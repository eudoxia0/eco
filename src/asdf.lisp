;;;; This is largely structured around the CFFI Groveller:
;;;;
;;;;     https://github.com/cffi/cffi/blob/master/grovel/asdf.lisp
;;;;

(in-package :asdf)

(defclass eco-template (source-file)
  ((type :initform "eco")
   (package :initform :eco-template
            :initarg :package
            :reader template-package)))

(defmethod compiled-template-path ((component eco-template))
  (make-pathname :type "lisp"
                 :defaults (component-pathname component)))

(defmethod output-files (op (component eco-template))
  nil)

(defmethod perform ((op compile-op) (component eco-template))
  (let ((compiled-template-path (compiled-template-path component)))
    (with-open-file (stream compiled-template-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (let* ((parsed (eco.parser:parse-pathname
                      (component-pathname component)))
             (compiled (eco.compiler:compile-template
                        parsed
                        (template-package component))))
        (print compiled stream)))))

(defmethod perform ((op load-op) (component eco-template))
  (let ((compiled-template-path (compiled-template-path component)))
    (perform (make-instance 'load-source-op)
             (make-instance 'cl-source-file
                            :name (component-name component)
                            :parent (component-parent component)
                            :pathname compiled-template-path))))

(import 'eco-test :asdf)
