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
