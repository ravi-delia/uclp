(in-package :uclp/test)

(def-suite utils :in uclp)
(in-suite utils)

(defun strform= (strform str)
  (string= (uclp::from-strform strform) str))

(test strform
  (is (strform= "a" "a"))
  (is (strform= #\a "a"))
  (is (strform= '("a" #\b "c" #\d) "abcd")))
