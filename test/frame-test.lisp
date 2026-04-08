(in-package :uclp/test)

(def-suite frame :in uclp)
(in-suite frame)

(defun as-pat (pat)
  (if *compiled*
      (compile-peg pat)
      (assemble-peg pat)))

(test holes
  (let* ((in-parens (as-pat '(* "(" (hole :aside) ")")))
	 (str-aside (fill-holes in-parens :aside "hi!"))
	 (aside-pat '(set "abc"))
	 (raw-aside (fill-holes in-parens :aside aside-pat))
	 (assembly-aside (fill-holes in-parens :aside (assemble-peg aside-pat)))
	 (compiled-aside (fill-holes in-parens :aside (compile-peg aside-pat))))
    (signals uclp:missing-hole-error
      (fill-holes in-parens))
    (signals uclp:unknown-hole-error
      (fill-holes in-parens :aside 1 :foo aside-pat))

    (fiveam:is (match str-aside "(hi!)"))
    (fiveam:is-false (match str-aside "((hi!)"))

    (loop for pat in (list raw-aside assembly-aside compiled-aside)
	  do (fiveam:is-false (match pat "(d)"))
	     (loop for middle across "abc"
		   do (is (match pat (format nil "(~a)" middle)))))))
