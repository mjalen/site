(in-package :asdf-user)

(load-system :spinneret/ps)
(defsystem #:site
  :description "A personal site for moi."
  :version "0.0.1"
  :author "Jalen Moore <jalennm@icloud.com>"
  :license "Public Domain"
  :pathname "src"
  :depends-on ("hunchentoot"
	       "spinneret"
	       "lass"
	       "parenscript"
	       (:feature :ros.installing "site/main"))
  :output-files (image-op (o c)
			  (output-files o :site/command))
  :in-order-to ((build-op (program-op "site/command"))))

(defsystem #:site/command
  :depends-on ("site/main")
  :build-operation "program-op"
  :build-pathname "site"
  :entry-point "site/main:safe-start-server")
