(defsystem "reference-generator"
  :version "0.1.0"
  :author "Daniel Nussenbaum"
  :license "MIT"
  :depends-on ("closer-mop")
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "reference-generator"))))
  :description "CL Reference Generator and Utilities to Introspect CL Systems")
