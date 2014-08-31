(asdf:defsystem eco-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:eco :fiveam)
  :defsystem-depends-on (:eco)
  :components ((:module "t"
                :serial t
                :components
                ((:file "eco")
                 (:eco-template "test")
                 (:file "test-asdf")))))
