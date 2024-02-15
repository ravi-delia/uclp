(in-package :uclp/test)

(def-suite basics :in uclp)
(in-suite basics)

(test literal-matches-from-start
  (is-match "a" "a")
  (is-match "a" "ab")
  (isnt-match "ab" "a")
  (isnt-match "ab" "ab" :start 1))
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
  (is-match '(range "ae") "c")
  (is-match '(range "ac") "c")
  (isnt-match '(range "az") "A")
  (isnt-match '(range "az") "7")
  (isnt-match '(range "AZ") "a"))

(test set
  (is-match '(set "a1Z_") "18")
  (is-match '(set "a1Z_") "a7")
  (is-match '(set "a1Z_") "_1")
  (isnt-match '(set "az") "b")
  (isnt-match '(set "az") ""))

(test tail-call-elim
  (is-match '(grammar :main (+ -1 (* 1 :main)))
	    (make-array 50000 :element-type 'character :initial-element #\a))) ; milage may vary