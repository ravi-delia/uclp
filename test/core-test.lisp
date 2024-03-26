(in-package :uclp/test)
(def-suite core :in uclp)
(in-suite core)

;; Built ints are in core for now

(test build-ins
  (check-pat '(* :d :h :d? :!d :s :!s :w+ -1)
    :match "111a *l23ghS" "1aa d44Gg"
    :fail "1aaaaa" "1ad X wef " "aaaaa"))

(test user-aliases
  (let ((*aliases* *aliases*))
    (register-alias! :v "video")
    (register-alias! :w " not w!")
    (is-match :v "video")
    (is-match :w " not w!"))
  (signals uclp::undefined-rule
    (match :v "video"))
  (isnt-match :w " not w!"))

(test compile-peg
  (let ((comp-pat (compile-peg '(grammar
				 :a (* "a" :b "a")
				 :b (+ "bb" :a "|")
				 :main :a))))
    (is-match comp-pat "aaa|aaa") ; Can pass to match
    (is-match `(split "," ,comp-pat) "a|a,abba,aa|aa"))) ; Can compose

(test replace
  (is (string= (replace-one '(* (set "Rr") "avi" (? "'s")) "John" "Ravi's name is ravi")
	       "John name is ravi"))
  (is (string= (replace-all '(* (set "Rr") "avi" (? "'s")) "John" "Ravi's name is ravi")
	       "John name is John")))

(test find
  (let ((str "This is a secret waldo message with a man waldo hiding in waldo it"))
    (is (match "waldo" str (find-one "waldo" str))) ; find-one should return int
    (is (every (lambda (pos) (match "waldo" str pos)) ; find-all should return list
	       (find-all "waldo" str)))))
