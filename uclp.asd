(defsystem "uclp"
  :version "0.0.2"
  :author "Ravi D'Elia"
  :license "MIT"
  :depends-on (:alexandria :serapeum :trivia)
  :components ((:module "src"
                :components
		((:file "uclp")
		 (:file "util" :depends-on ("uclp"))
		 (:file "basics" :depends-on ("util"))
		 (:file "capture" :depends-on ("util"))
		 (:file "core" :depends-on ("basics" "capture")))))
  :description "An experimental implementation of Janet's PEG module in common lisp")
