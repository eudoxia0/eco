(in-package :eco-test)

(def-suite asdf)
(in-suite asdf)

(defmacro with-template ((call text) &rest tests)
  `(let ((,text))
    (finishes
      (setf ,text (string-trim (list #\Newline #\Space) ,call)))
    ,@tests))

(test asdf
  (with-template ((eco-template:test t "test" (list 1 2 3) nil) text)
    (is
     (equal text "true")))
  (with-template ((eco-template:test2 "&") text)
    (is
     (equal text "&amp;"))))

(run! 'asdf)
