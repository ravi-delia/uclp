(defsystem "peg"
  :version "0.0.1"
  :author "Ravi D'Elia"
  :license "MIT"
  :depends-on (:alexandria :serapeum :trivia)
  :components ((:module "src"
                :components
		((:file "peg")
		 (:file "util")
		 (:file "basics")
		 (:file "capture")
		 (:file "core"))))
  :description "An experimental implementation of Janet's PEG module in common lisp"
  :in-order-to ((test-op (test-op "peg/tests"))))
