(asdf:defsystem #:mjalen/site
  :description "A personal site for moi."
  :version "0.0.1"
  :author "Jalen Moore <jalennm@icloud.com>"
  :license "Public Domain"
  :pathname "src"
  :components
  ((:file "package")
   (:file "main" :depends-on ("package"))))
