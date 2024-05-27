;;;; nodgui-test.asd

(asdf:defsystem #:nodgui-test
  :description ""
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:nodgui #:utilities)
  :components ((:file "package")
               (:file "main")))
