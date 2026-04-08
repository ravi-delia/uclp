(in-package :uclp/test)

(def-suite capture :in uclp)
(in-suite capture)

(asm-test nested-accum
  (check-pat `(% (* (+ (% (* '"a" '"b"))
		       (/ (% (* '"c" '"d")) ,#'string-upcase))
		    '"ef"))
    :match ("abef" :result '("abef")) ("cdef" :result '("CDef"))))

(asm-test backref
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

(asm-test argument
  (is-match '(* (argument 0 :a) (backmatch :a) -1) "abc" :args '("abc"))
  (isnt-match '(* (argument 1 :a) (backmatch :a) -1) "abc" :args '("abc" "efg"))
  (isnt-match '(* (argument 0)) "abc"))

(asm-test lenprefix
  (check-pat `'(* (lenprefix (* (integer 1 nil :b) (integer 1)) "a") (? (backref :b)))
    :match "35aaa" "31aaaa" ("30aaaa" :result '("30aaa"))
    :fail "33aa" "3aab" "33ab" "v2aaa"))

(define-condition test-error (peg-error) ())
(asm-test error
  (is-match '(* 1 (error -1) 1) "ab")
  (signals uclp:peg-error
    (match (compile-peg '(* 1 (error -1) 1)) "a"))
  (signals test-error
    (match (compile-peg '(* 1 (error -1 test-error) 1)) "a")))

(asm-test integer
  (check-pat '(integer 2)
    :match ("10" :result '(10)) ("1010" :result '(10))))
