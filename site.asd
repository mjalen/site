(asdf:load-system :spinneret/ps)

(asdf:defsystem #:site
  :description "A personal site for moi."
  :version "0.0.1"
  :author "Jalen Moore <jalennm@icloud.com>"
  :license "Public Domain"
  :depends-on ("hunchentoo"
	       "spinneret"
	       "lass"
	       "parenscript")
  :components
  ((:module "src"
    :components
    ((:file "main")))))
