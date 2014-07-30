(asdf:defsystem eco
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:esrap)
  :components ((:module "src"
                :components
                ((:file "parser"))))
  :description "Fast, flexible, designer-friendly templates."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op eco-test))))
