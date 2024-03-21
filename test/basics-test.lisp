(in-package :uclp/test)

(def-suite basics :in uclp)
(in-suite basics)

(test literal-matches-from-start
  (is-match "a" "a")
  (is-match #\a "ab")
  (isnt-match "ab" "a")
  (isnt-match "ab" "ab" :start 1))
(test strform-converted
  (is-match #\Newline (string #\Newline))
  (is-match '(#\A #\a) "Aa")
  (is-match '("ab") "ab"))
(test empty-string-matches
  (is-match "" "abc")
  (is-match "" "abc" :start 3))

(test positive-count
  (is-match 1 "a")
  (is-match 2 "ab")
  (is-match 1 "ab")
  (isnt-match 3 "ab")
  (isnt-match 1 "")
  (isnt-match 1 "a" :start 1))
(test zero-count
  (is-match 0 "")
  (is-match 0 "a")
  (is-match 0 "a" :start 1)
  (isnt-match 0 "a" :start 2))
(test negative-count
  (is-match -1 "")
  (is-match -1 "a" :start 1)
  (is-match -2 "a")
  (isnt-match -2 "ab")
  (is-match -2 "a" :start 1))

(test range
  (is-match '(range "ac") "a")
  (is-match '(range (#\a #\c)) "c")
  (is-match '(range "ac") "c")
  (isnt-match '(range "az") "A")
  (isnt-match '(range "az") "7")
  (isnt-match '(range "AZ") "a"))

(test set
  (is-match '(set ("a1" #\Z #\_)) "18")
  (is-match '(set "a1Z_") "a7")
  (is-match '(set "a1Z_") "_1")
  (isnt-match '(set "az") "b")
  (isnt-match '(set "az") ""))

;; (test tail-call-elim
;;   (is-match '(grammar :main (+ -1 (* 1 :main)))
;; 	    (make-array 50000 :element-type 'character :initial-element #\a))) ; milage may vary

(test grammar
  (check-pat `(grammar
	       :a (* "a" :b "a")
	       :b (* "b" (+ :a 0) "b")
	       :main (* "(" :b ")"))
    :match "(bb)" "(babbab)" "(babbab)" "(bababbabab)"
    :fail "()" "(aba" "(aa)" "(abbb)" "(bab)"))

(test sub
  (check-pat `(* (sub (<- 2 :b) (* (any "b") -1)) (backmatch :b))
    :match "bbbb" "bbbbc"
    :fail "bbb" "" "1" "bbcc")
  ; After matching sub-pat, advance to end of super-pat
  (is-match '(* (sub 3 '1) "b") "aaab")) 

;; As of now (2024-03-20) the SPLIT pattern is not in the latest release of Janet,
;; but is in the current repo. The desired behavior doesn't seem to be documented
;; anywhere, but my faith in the design goals of Ian Henry far outstrips my faith
;; in my own. So I am trying to mimic the behavior of Janet's SPLIT as closely as
;; as possible. These tests are copied verbatim from Janet's test suite.

(test split
  ; basic functionality
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
