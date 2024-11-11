(in-package :asdf-user)

(defsystem #:site
  :description "A personal site for moi."
  :version "0.0.1"
  :author "Jalen Moore <jalennm@icloud.com>"
  :license "Public Domain"
  :pathname "src"
  :depends-on ("hunchentoot"
	       "spinneret"
	       "lass"
	       "parenscript")
  :components ((:file "main")))
