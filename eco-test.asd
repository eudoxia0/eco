(asdf:defsystem eco-test
  :author "Fernando Borretti"
  :license "MIT"
  :description "Tests for eco"
  :depends-on (:eco :fiveam)
  :defsystem-depends-on (:eco)
  :components ((:module "t"
                :serial t
                :components
                ((:file "eco")
                 (:eco-template "test")
                 (:file "test-asdf")
                 (:eco-template "compiler")
                 (:file "test-compiler")))))
