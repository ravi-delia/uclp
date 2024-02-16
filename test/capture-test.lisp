(in-package :uclp/test)

(def-suite capture :in uclp)
(in-suite capture)

(test nested-accum
  (check-pat `(% (* (+ (% (* '"a" '"b"))
		       (/ (% (* '"c" '"d")) ,#'string-upcase))
		    '"ef"))
    :match ("abef" :result '("abef")) ("cdef" :result '("CDef"))))

(test backref
  (check-pat `(grammar
	       :pad (any "=")
	       :open (* "[" (<- :pad :n) "[")
	       :close (* "]" (cmt (* (backref :n) (<- :pad)) ,#'string=) "]")
	       :main (* :open (any (if-not :close 1)) :close -1))
    :match "[[]]" "[==[a]==]" "[[blark]]" "[[bl[ark]]" "[[bl]rk]]" "[===[]==]===]"
    :fail "[[bl]rk]] " "[=[bl]]rk]=] " "[=[bl]==]rk]=] " "[==[]===]")
  (check-pat `(* (+ (<- "b" :b)
		    (<- 1 :a))
		 (+ (* (backmatch :a) "c")
		    (backmatch :b))
		 -1)
    :match "bb" "aac"
    :fail "cc" "ab"))
