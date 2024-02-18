(defsystem :uclp
  :version "0.0.2"
  :author "Ravi D'Elia"
  :license "MIT"
  :depends-on (:alexandria :serapeum :trivia)
  :pathname "src"
  :components
  ((:file "uclp")
   (:file "util" :depends-on ("uclp"))
   (:file "basics" :depends-on ("util"))
   (:file "capture" :depends-on ("util"))
   (:file "core" :depends-on ("basics" "capture")))
  :in-order-to ((test-op (test-op "uclp/test")))
  :description "An experimental implementation of Janet's PEG module in common lisp")

(defsystem :uclp/test
  :depends-on (:uclp :fiveam)
  :pathname "test"
  :components
  ((:file "init")
   (:file "util-test" :depends-on ("init"))
   (:file "core-test" :depends-on ("init"))
   (:file "basics-test" :depends-on ("init"))
   (:file "capture-test" :depends-on ("init")))
  :perform (test-op (o c) (symbol-call :uclp/test :run-tests!)))
