(defsystem "senior-project"
  :version "0.0.1"
  :author "Parker Lee"
  :mailto "plee25006@byui.edu"
  :license "AGPL-3.0-only"
  :depends-on ("alexandria")
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "calculation" :depends-on ("utils" "enumeration"))
                 (:file "validation" :depends-on ("utils"))
                 (:file "enumeration" :depends-on ("utils"))
                 (:file "main" :depends-on ("validation" "enumeration" "calculation" "utils")))))
  :description "Code I made for my senior project in undergraduate reseearch."
  :in-order-to ((test-op (test-op "senior-project/tests"))))

(defsystem "senior-project/tests"
  :author "Parker Lee"
  :license "AGPL-3.0-only"
  :depends-on ("senior-project"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for senior-project"
  :perform (test-op (op c) (symbol-call :rove :run c)))
