(in-package :peg)

;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;; ;;
;; ;; Primitives ;; ;;
;; ;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun compile-literal (opts literal)
  (with-opts (opts $strc $litc $i)
    `(when (<= (+ ,curr ,(length literal)) (length ,str))
       (loop for ,$i of-type fixnum from ,curr below (+ ,curr ,(length literal))
	     for ,$strc of-type character = (char ,str ,$i)
	     for ,$litc of-type character across ,literal
	     if (not (char= ,$litc ,$strc)) do
		(return nil)
	     finally (setf ,curr ,$i) (return t)))))
(defun compile-count (opts count)
  (with-opts (opts)
    (if (< count 0)
	`(> (+ ,curr ,(- count)) (length ,str))
	`(when (<= (+ ,curr ,count) (length ,str))
	   (incf ,curr ,count)
	   t))))

(define-condition compile-range (error) (expr datum text))
(defun compile-range (opts rexpr)
  (destructuring-bind (_ &rest range-strs) rexpr
    (declare (ignore _))
    (with-opts (opts $c)
      `(when (< ,curr (length ,str))
	 (let* ((,$c (char ,str ,curr)))
	   (when (or ,@(loop for s in range-strs
			     if (length= s 2)
			     collect (list 'char<= (char s 0) $c (char s 1))
			     else do
				(error 'compile-range :expr rexpr :datum s :text "Bad range")))
	     (incf ,curr)
	     t))))))

(defun compile-set (opts set)
  (with-opts (opts $strc $setc)
    `(if (= ,curr (length ,str))
	 nil
	 (let ((,$strc (char ,str ,curr)))
	   ,(if (< (length set) 8)
		`(if (or ,@(loop for c across set collect `(eq ,c ,$strc))) (incf ,curr))
		`(loop for ,$setc of-type character across ,set
		       if (char= ,$strc ,$setc) do
			  (incf ,curr)
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
    (with-opts (opts)
      `(and ,@(mapcar (lambda (p) (compile-expr opts p)) pats)))))

(defun compile-choice (opts expr)
  (when (emptyp (rest expr)) (error 'missing-arguments :expr expr))
  (let ((pats (butlast (rest expr)))
	(tail (last-elt expr)))
    (with-opts (opts)
      `(with-save (,curr ,caps ,tags ,accum)
	 (or ,@(mapcar
		(lambda (pat)
		  `(if ,(compile-expr opts pat)
		       t
		       ,(restore curr caps tags accum)))
		pats)
	     ,(compile-expr opts tail))))))

(defun compile-several (opts pat minimum &optional maximum)
  (with-opts (opts $matches $matched?)
    `(with-save (,curr ,caps ,tags ,accum)
       (loop with ,$matches = 0
	     for ,$matched? = ,(compile-expr opts pat)
	     while ,$matched? do
		(incf ,$matches)
		,(checkpoint curr caps tags accum)
		,@(list-if
		   maximum
		   `(if (= ,$matches ,maximum)
			(return t)))
	     finally
		,(restore curr caps tags accum)
		(return (>= ,$matches ,minimum))))))

(defun compile-opt (opts pat)
  (with-opts (opts)
    `(with-save (,curr ,caps ,tags ,accum)
       (if ,(compile-expr opts pat)
	   t
	   (progn ,(restore curr caps tags accum) t)))))

(define-condition no-main-rule (error) (grammar))
(defun compile-grammar (opts expr)
  (let* ((name-bodies (pairs expr))
	 (knames (mapcar #'first name-bodies))
	 (bodies (mapcar #'second name-bodies)))
    (unless (find :main knames) (error 'no-main-rule :grammar expr))
    (push knames (compopts-env opts))
    (with-opts (opts)
      (let* ((names (mapcar (lambda (n) (prefsym prefix n)) knames))
	     (comped (mapcar (lambda (b) (compile-expr opts b)) bodies))
	     (out `(labels ,(mapcar
			     (lambda (name body) `(,name () ,body))
			     names comped)
		     (declare (dynamic-extent
			       ,@(mapcar (lambda (n) `(function ,n)) names)))
		     (,(prefsym prefix :main)))))
	(pop (compopts-env opts))
	out))))

(defun compile-look (opts pat n)
  (with-opts (opts $matched?)
    `(with-save (,curr ,caps ,tags ,accum)
       (,@(cond ((< n 0) `(when (<= ,n ,curr) (incf ,curr ,n))) ;; We need to check for underflow
	       ((> n 0) `(progn (incf ,curr ,n))) ;; Every primitive already checks for overflow
		 (t '(progn)))
	 (let ((,$matched? ,(compile-expr opts pat)))
	   ,(restore curr caps tags accum)
	   ,$matched?)))))
(defun compile-not (opts pat)
  `(not ,(compile-look opts pat 0)))
(defun compile-if (opts cond pat)
  `(and ,(compile-look opts cond 0) ,(compile-expr opts pat)))
(defun compile-if-not (opts cond pat)
  `(and (not ,(compile-look opts cond 0)) ,(compile-expr opts pat)))

;; With tail call elim these should compile to code just as efficient
;; as handwritten loops- maybe even more effecient
(defun compile-thru (opts pat)
  (compile-expr opts `(grammar :main (+ ,pat (* 1 :main)))))
(defun compile-to (opts pat)
  (compile-thru opts `(look ,pat 0)))
