(asdf:defsystem eco-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:eco :fiveam)
  :components ((:module "t"
                :components
                ((:file "eco")))))
