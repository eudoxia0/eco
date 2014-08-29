(asdf:defsystem eco
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:esrap
               :split-sequence)
  :components ((:module "src"
                :serial t
                :components
                ((:file "parser")
                 (:file "compiler")
                 (:file "asdf"))))
  :description "Fast, flexible, designer-friendly templates."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op eco-test))))
