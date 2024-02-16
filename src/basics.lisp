(in-package :uclp)

;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;; ;;
;; ;; Primitives ;; ;;
;; ;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Right now none of these use OPTS but down the road they might need type info
;; and other such things.

(defun compile-literal (opts literal)
  (declare (ignore opts))
  (with-gensyms ($strc $litc $i)
    `(when (<= (+ curr ,(length literal)) strlen)
       (loop for ,$i of-type fixnum from curr below (+ curr ,(length literal))
	     for ,$strc of-type character = (char str ,$i)
	     for ,$litc of-type character across ,literal
	     if (not (char= ,$litc ,$strc)) do
		(return nil)
	     finally (setf curr ,$i) (return t)))))
(defun compile-count (opts count)
  (declare (ignore opts))
  (if (< count 0)
      `(> (+ curr ,(- count)) strlen)
      `(when (<= (+ curr ,count) strlen)
	 (incf curr ,count)
	 t)))

(define-condition compile-range (error) (expr datum text))
(defun compile-range (opts rexpr)
  (declare (ignore opts))
  (destructuring-bind (_ &rest range-strs) rexpr
    (declare (ignore _))
    (with-gensyms ($c)
      `(when (< curr strlen)
	 (let* ((,$c (char str curr)))
	   (when (or ,@(loop for s in range-strs
			     if (length= s 2)
			     collect (list 'char<= (char s 0) $c (char s 1))
			     else do
				(error 'compile-range :expr rexpr :datum s :text "Bad range")))
	     (incf curr)
	     t))))))

(defun compile-set (opts set)
  (declare (ignore opts))
  (with-gensyms ($strc $setc)
    `(if (= curr strlen)
	 nil
	 (let ((,$strc (char str curr)))
	   ,(if (< (length set) 8)
		`(if (or ,@(loop for c across set collect `(eq ,c ,$strc))) (incf curr))
		`(loop for ,$setc of-type character across ,set
		       if (char= ,$strc ,$setc) do
			  (incf curr)
			  (return t)
		       finally (return nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;; ;;
;; ;; Combinators ;; ;;
;; ;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define-condition missing-arguments (error) (expr))
(defun compile-sequence (opts expr)
  (when (emptyp (rest expr)) (error 'missing-arguments :expr expr))
  (let ((pats (rest expr)))
    `(and ,@(mapcar (lambda (p) (compile-expr opts p)) pats))))

(defun compile-choice (opts expr)
  (when (emptyp (rest expr)) (error 'missing-arguments :expr expr))
  (let ((pats (butlast (rest expr)))
	(tail (last-elt expr)))
    `(with-save (curr caps tags accum)
       (or ,@(mapcar
	      (lambda (pat)
		`(if ,(compile-expr opts pat)
		     t
		     (restore curr caps tags accum)))
	      pats)
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

(defun compile-opt (opts pat)
  `(with-save (curr caps tags accum)
     (if ,(compile-expr opts pat)
	 t
	 (progn (restore curr caps tags accum) t))))

(define-condition no-main-rule (error) (grammar))
(defun compile-grammar (opts expr)
  (let* ((name-bodies (pairs expr))
	 (knames (mapcar #'first name-bodies))
	 (bodies (mapcar #'second name-bodies)))
    (unless (find :main knames) (error 'no-main-rule :grammar expr))
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
(defun compile-not (opts pat)
  `(not ,(compile-look opts pat 0)))
(defun compile-if (opts cond pat)
  `(and ,(compile-look opts cond 0) ,(compile-expr opts pat)))
(defun compile-if-not (opts cond pat)
  `(and (not ,(compile-look opts cond 0)) ,(compile-expr opts pat)))

;; With tail call elim these should compile to code just as efficient
;; as handwritten loops- maybe even more efficient
(defun compile-thru (opts pat)
  (compile-expr opts `(grammar :main (+ ,pat (* 1 :main)))))
(defun compile-to (opts pat)
  (compile-thru opts `(look ,pat 0)))

(defun compile-sub (opts super-pat sub-pat)
  (with-gensyms ($matched)
    `(with-save (curr)
       (when ,(compile-expr opts super-pat)
	 (with-save (strlen)
	   (setf strlen curr)
	   (restore curr)
	   (let ((,$matched ,(compile-expr opts sub-pat)))
	     (restore strlen)
	     ,$matched))))))
