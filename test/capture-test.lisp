(in-package :uclp/test)

(def-suite capture :in uclp)
(in-suite capture)

(test backtrack-tags
  (check-pat `(* (+ (<- "b" :b)
		    (<- 1 :a))
		 (+ (* (backmatch :a) "c")
		    (backmatch :b))
		 -1)
    :match "bb" "aac"
    :fail "cc" "ab"))

(test nested-accum
  (check-pat `(% (* (+ (% (* '"a" '"b"))
		       (/ (% (* '"c" '"d")) ,#'string-upcase))
		    '"ef"))
    :match ("abef" :result '("abef")) ("cdef" :result '("CDef"))))
