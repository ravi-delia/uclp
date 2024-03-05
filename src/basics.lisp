(in-package :uclp)

;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;; ;;
;; ;; Primitives ;; ;;
;; ;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun compile-literal (opts literal)
  (declare (ignore opts))
  (with-gensyms ($strc $litc)
    `(when (<= (+ curr ,(length literal)) strlen)
       (loop for ,$strc of-type character = (char str curr)
	     for ,$litc of-type character across ,literal
	     do (if (char/= ,$strc ,$litc) (return nil))
	        (incf curr)
	     finally (return t)))))
(defun compile-count (opts count)
  (declare (ignore opts))
  (if (< count 0)
      `(> (+ curr ,(- count)) strlen)
      `(when (<= (+ curr ,count) strlen)
	 (incf curr ,count)
	 t)))

(defpattern (range)
    (&rest (ranges (lambda (s) (and (strform? s) (length= s 2)))
		   "string of length 2"))
  (let ((range-strs (mapcar #'from-strform ranges)))
    (with-gensyms ($c)
      `(when (< curr strlen)
	 (let* ((,$c (char str curr)))
	   (when (or ,@(loop for s in range-strs
			     collect (list 'char<= (char s 0) $c (char s 1))))
	     (incf curr)
	     t))))))

(defpattern (set) ((set :string))
  (with-gensyms ($strc $setc)
    (let ((set (from-strform set)))
      `(if (>= curr strlen)
	   nil
	   (let ((,$strc (char str curr)))
	     ,(if (< (length set) 8)
		  `(if (or ,@(loop for c across set collect `(eq ,c ,$strc))) (incf curr))
		  `(loop for ,$setc of-type character across ,set
			 if (char= ,$strc ,$setc) do
			    (incf curr)
			    (return t)
			 finally (return nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;; ;;
;; ;; Combinators ;; ;;
;; ;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defpattern (sequence *) (&rest (pats :pat))
  `(and ,@(mapcar (lambda (p) (compile-expr opts p)) pats)))

(defpattern (choice +) (&rest (pats :pat))
  (let ((tail (last-elt pats))
	(buttail (butlast pats)))
    `(with-save (curr caps tags accum)
       (or ,@(mapcar
	      (lambda (pat)
		`(if ,(compile-expr opts pat)
		     t
		     (restore curr caps tags accum)))
	      buttail)
	   ,(compile-expr opts tail)))))

(defun compile-several (opts pat minimum &optional maximum)
  (with-gensyms ($matches $matched?)
    `(with-save (curr caps tags accum)
       (loop with ,$matches = 0
	     for ,$matched? = ,(compile-expr opts pat)
	     while ,$matched? do
		(incf ,$matches)
		(checkpoint curr caps tags accum)
		,@(list-if
		   maximum
		   `(if (= ,$matches ,maximum)
			(return t)))
	     finally
		(restore curr caps tags accum)
		(return (>= ,$matches ,minimum))))))

(defpattern (any) ((pat :pat))
  (compile-several opts pat 0))
(defpattern (some) ((pat :pat))
  (compile-several opts pat 1))
(defpattern (between) ((pat :pat) (low :reps) (high :reps))
  (compile-several opts pat low high))
(defpattern (at-least) ((pat :pat) (n :reps))
  (compile-several opts pat n))
(defpattern (at-most) ((pat :pat) (n :reps))
  (compile-several opts pat 0 n))
(defpattern (repeat) ((pat :pat) (n :reps))
  (compile-several opts pat n n))

(defpattern (opt ?) ((pat :pat))
  `(with-save (curr caps tags accum)
     (if ,(compile-expr opts pat)
	 t
	 (progn (restore curr caps tags accum) t))))

(defpattern (grammar) (&rest (pats :pat))
  (verify-args! '(&rest (name :tag) (pat :pat)) `(grammar ,@pats))
  (let* ((name-bodies (pairs pats))
	 (knames (mapcar #'first name-bodies))
	 (bodies (mapcar #'second name-bodies)))
    (unless (find :main knames)
      (throw-msg! (format nil "grammar error: no main in ~%~s" `(grammar ,@pats))))
    (push knames (compopts-env opts))
    (unless (compopts-prefix opts) (setf (compopts-prefix opts) (gensym)))
    (let* ((prefix (compopts-prefix opts))
	   (names (mapcar (lambda (n) (prefsym prefix n)) knames))
	   (comped (mapcar (lambda (b) (compile-expr opts b)) bodies))
	   (out `(labels ,(mapcar
			   (lambda (name body) `(,name () ,body))
			   names comped)
		   (declare (dynamic-extent
			     ,@(mapcar (lambda (n) `(function ,n)) names)))
		   (,(prefsym prefix :main)))))
      (pop (compopts-env opts))
      out)))

(defun compile-look (opts pat n)
  (with-gensyms ($matched?)
    `(with-save (curr caps tags accum)
       (,@(cond ((< n 0) `(when (<= ,n curr) (incf curr ,n))) ;; We need to check for underflow
	       ((> n 0) `(progn (incf curr ,n))) ;; Every primitive already checks for overflow
		 (t '(progn)))
	 (let ((,$matched? ,(compile-expr opts pat)))
	   (restore curr caps tags accum)
	   ,$matched?)))))
(defpattern (look >) ((pat :pat) (n :num))
  (compile-look opts pat n))
(defpattern (not !) ((pat :pat))
  `(not ,(compile-look opts pat 0)))
(defpattern (if) ((cond :pat) (pat :pat))
  `(and ,(compile-look opts cond 0) ,(compile-expr opts pat)))
(defpattern (if-not) ((cond :pat) (pat :pat))
  `(and (not ,(compile-look opts cond 0)) ,(compile-expr opts pat)))

;; With tail call elim these should compile to code just as efficient
;; as handwritten loops- maybe even more efficient
(defpattern (thru) ((pat :pat))
  (compile-expr opts `(grammar :main (+ ,pat (* 1 :main)))))
(defpattern (to) ((pat :pat))
  (compile-expr opts `(thru (look ,pat 0))))

(defpattern (sub) ((super-pat :pat) (sub-pat :pat))
  (with-gensyms ($matched)
    `(with-save (curr)
       (when ,(compile-expr opts super-pat)
	 (with-save (strlen)
	   (setf strlen curr)
	   (restore curr)
	   (let ((,$matched ,(compile-expr opts sub-pat)))
	     (restore strlen)
	     ,$matched))))))
