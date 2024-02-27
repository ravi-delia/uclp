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

(test lenprefix
  (check-pat `'(* (lenprefix (* (integer 1 nil :b) (integer 1)) "a") (? (backref :b)))
    :match "35aaa" "31aaaa" ("30aaaa" :result '("30aaa"))
    :fail "33aa" "3aab" "33ab" "v2aaa"))

(define-condition test-error (peg-error) ())
(test error
  (is-match '(* 1 (error -1) 1) "ab")
  (signals uclp:peg-error
    (match '(* 1 (error -1) 1) "a"))
  (signals test-error
    (match '(* 1 (error -1 test-error) 1) "a")))
