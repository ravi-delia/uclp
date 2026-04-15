(in-package :uclp/test)

(def-suite basics :in uclp)
(in-suite basics)

(asm-test literal-matches-from-start
  (is-match "a" "a")
  (is-match #\a "ab")
  (isnt-match "ab" "a")
  (isnt-match "ab" "ab" :start 1))
(asm-test strform-converted
  (is-match #\Newline (string #\Newline))
  (is-match '(#\A #\a) "Aa")
  (is-match '("ab") "ab"))
(asm-test empty-string-matches
  (is-match "" "abc")
  (is-match "" "abc" :start 3))

(asm-test positive-count
  (is-match 1 "a")
  (is-match 2 "ab")
  (is-match 1 "ab")
  (isnt-match 3 "ab")
  (isnt-match 1 "")
  (isnt-match 1 "a" :start 1))
(asm-test zero-count
  (is-match 0 "")
  (is-match 0 "a")
  (is-match 0 "a" :start 1)
  (isnt-match 0 "a" :start 2))
(asm-test negative-count
  (is-match -1 "")
  (is-match -1 "a" :start 1)
  (is-match -2 "a")
  (isnt-match -2 "ab")
  (is-match -2 "a" :start 1))

(asm-test choice
  (check-pat '(+ "a" "b" "c")
    :match "a" "b" "c"))

(asm-test range
  (is-match '(range "ac") "a")
  (is-match '(range (#\a #\c)) "c")
  (is-match '(range "ac") "c")
  (isnt-match '(range "az") "A")
  (isnt-match '(range "az") "7")
  (isnt-match '(range "AZ") "a"))

(asm-test set
  (is-match '(set ("a1" #\Z #\_)) "18")
  (is-match '(set "a1Z_") "a7")
  (is-match '(set "a1Z_") "_1")
  (isnt-match '(set "az") "b")
  (isnt-match '(set "az") ""))

(asm-test grammar
  (check-pat `(grammar
	       :a (* "a" :b "a")
	       :b (* "b" (+ :a 0) "b")
	       :main (* "(" :b ")"))
    :match "(bb)" "(babbab)" "(babbab)" "(bababbabab)"
    :fail "()" "(aba" "(aa)" "(abbb)" "(bab)"))

(asm-test sub
  (is-match '(* (sub 3 1) "b") "aaab")
  (check-pat `(* (sub (<- 2 :b) (* (any "b") -1)) (backmatch :b))
    :match "bbbb" "bbbbc"
    :fail "bbb" "" "1" "bbcc"))

(asm-test split
  ; basic functionality
  (is-match '(split "," 1) "a,b,c")

  (is-match '(split "," '1) "a,b,c" 
	    :result '("a" "b" "c")) 

  ; drops captures from separator pattern
  (is-match '(split '"," '1) "a,b,c" 
	    :result '("a" "b" "c")) 

  ; can match empty subpatterns
  (is-match '(split "," ':w*) ",a,,bar,,,c,," 
	    :result '("" "a" "" "bar" "" "" "c" "" "")) 

  ; subpattern is limited to only text before the separator
  (is-match '(split "," '(to -1)) "a,,bar,c" 
	    :result '("a" "" "bar" "c")) 

  ; fails if any subpattern fails
  (isnt-match '(split "," '"a") "a,a,b") 

  ; separator does not have to match anything
  (is-match '(split "x" '(to -1)) "a,a,b" 
	    :result '("a,a,b")) 

  ; always consumes entire input
  (is-match '(split 1 '"") "abc" 
	    :result '("" "" "" "")) 

  ; separator can be an arbitrary PEG
  (is-match '(split :s+ '(to -1)) "a   b      c" 
	    :result '("a" "b" "c")) 

  ; does not advance past the end of the input
  (is-match '(* (split "," ':w+) 0) "a,b,c" 
	    :result '("a" "b" "c")))

;; Regression 2026-03-11
;; Assembled choice operator ignores options other than first or last
(asm-test middle-choice (is-match '(+ "a" "b" "c") "b"))
