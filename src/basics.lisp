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
       (loop for ,$litc of-type character across ,literal
	     for ,$strc of-type character = (char str curr)
	     do (if (char/= ,$strc ,$litc) (return nil))
	        (incf curr)
	     finally (return t)))))

(defun assemble-literal (opts literal)
  (declare (ignore opts) (type string literal))
  (let ((litlen (length literal)))
    (pat-lambda (curr str strlen)
      (when (<= (+ curr litlen) strlen)
	(loop for litc of-type character across literal
	      for strc of-type character = (char str curr)
	      do (if (char/= strc litc) (return nil))
	         (incf curr)
	      finally (return t))))))

(defun compile-count (opts count)
  (declare (ignore opts))
  (if (< count 0)
      `(> (+ curr ,(- count)) strlen)
      `(when (<= (+ curr ,count) strlen)
	 (incf curr ,count)
	 t)))

(defun assemble-count (opts count)
  (declare (ignore opts) (type fixnum count))
  (if (< count 0)
      (pat-lambda (curr strlen) (> (+ curr (- count)) strlen))
      (pat-lambda (curr strlen) (when (<= (+ curr count) strlen)
				  (incf curr count)
				  t))))

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
	     t)))))
  (let* ((range-strs (mapcar #'from-strform ranges))
	 (starts (map 'vector (lambda (s) (char s 0)) range-strs))
	 (ends (map 'vector (lambda (s) (char s 1)) range-strs)))
    (pat-lambda (curr strlen str)
      (when (< curr strlen)
	(let* ((c (char str curr)))
	  (loop for s across starts
		for e across ends
		do (when (char<= s c e) (incf curr) (return t))
		finally (return nil)))))))
		

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
			 finally (return nil)))))))
  (let ((set (from-strform set)))
    (pat-lambda (curr str strlen)
      (when (< curr strlen)
	(let ((c (char str curr)))
	  (loop for s across set
		do (when (char= s c)
		     (incf curr)
		     (return t))
		finally (return nil)))))))
		     
		

;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;; ;;
;; ;; Combinators ;; ;;
;; ;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defpattern (sequence *) (&rest (pats :pat))
  `(and ,@(mapcar (lambda (p) (compile-expr opts p)) pats))
  (let* ((raw-pats (mapcar (lambda (p) (assemble-expr opts p)) pats))
	 (tail-pat (car (last raw-pats)))
	 (body-pats (coerce (butlast raw-pats) 'vector)))
    (pat-lambda ()
      (and (loop for fn of-type function across body-pats 
		 do (unless (funcall fn state) (return nil))
		 finally (return t))
	   (funcall tail-pat state)))))
		 

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
	   ,(compile-expr opts tail))))
  (let* ((raw-pats (mapcar (lambda (p) (assemble-expr opts p)) pats))
	 (tail (last-elt raw-pats))
	 (buttail (coerce (butlast raw-pats) 'vector)))
    (pat-lambda (curr caps tags accum)
      (with-save (curr caps tags accum)
	(or (loop for fn of-type function across buttail
		  do (when (funcall fn state) (return t))
		     (restore curr caps tags accum)
		     (return nil))
	    (funcall tail state))))))

(defun compile-several (opts pat minimum &optional maximum)
  (with-gensyms ($matches $matched?)
    `(with-save (curr caps tags accum)
       (loop with ,$matches fixnum = 0
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

(defun assemble-up-to (opts pat minimum maximum)
  (let ((assembly (assemble-expr opts pat)))
    (pat-lambda (curr caps tags accum)
      (with-save (curr caps tags accum)
	(loop with matches fixnum = 0
	      for matched? = (funcall assembly state)
	      while matched? do
		 (incf matches)
		 (checkpoint curr caps tags accum)
		 (if (= matches maximum) (return t))
	      finally
		 (restore curr caps tags accum)
		 (return (>= matches minimum)))))))
(defun assemble-several (opts pat minimum)
  (let ((assembly (assemble-expr opts pat)))
    (pat-lambda (curr caps tags accum)
      (with-save (curr caps tags accum)
	(loop with matches fixnum = 0
	      for matched? = (funcall assembly state)
	      while matched? do
		 (incf matches)
		 (checkpoint curr caps tags accum)
	      finally
		 (restore curr caps tags accum)
		 (return (>= matches minimum)))))))

(defpattern (any) ((pat :pat))
  (compile-several opts pat 0)
  (assemble-several opts pat 0))
  
  
(defpattern (some) ((pat :pat))
  (compile-several opts pat 1)
  (assemble-several opts pat 1))
(defpattern (between) ((pat :pat) (low :reps) (high :reps))
  (compile-several opts pat low high)
  (assemble-up-to opts pat low high))
(defpattern (at-least) ((pat :pat) (n :reps))
  (compile-several opts pat n)
  (assemble-several opts pat n))
(defpattern (at-most) ((pat :pat) (n :reps))
  (compile-several opts pat 0 n)
  (assemble-up-to opts pat 0 n))
(defpattern (repeat) ((pat :pat) (n :reps))
  (compile-several opts pat n n)
  (assemble-up-to opts pat n n))

(defpattern (opt ?) ((pat :pat))
  `(with-save (curr caps tags accum)
     (if ,(compile-expr opts pat)
	 t
	 (progn (restore curr caps tags accum) t)))
  (let ((p (assemble-expr opts pat)))
    (pat-lambda (curr caps tags accum)
      (with-save (curr caps tags accum)
	(if (funcall p state)
	    t
	    (progn (restore curr caps tags accum) t))))))

(defun assemble-grammar (opts pats)
  (verify-args! '(&rest (name :tag) (pat :pat)) `(grammar ,@pats))
  (let* ((name-bodies (pairs pats))
	 (knames (mapcar #'first name-bodies))
	 (bodies (mapcar #'second name-bodies))
	 (all-pats (asmopts-all-patterns opts))
	 (main-pos nil))
    (push-env-scope opts)
    (loop for fndx of-type fixnum from (length all-pats)
	  for kname in knames
	  do (push-env-binding
	      opts kname
	      (let ((fndx fndx)) ; Loop variable capture in the wild, and it actually got me!
		(lambda (state) 
		  (funcall (the function (aref all-pats fndx)) state))))
	  (when (eq kname :main) (setf main-pos fndx)))
    (loop for body in bodies
	  do (vector-push-extend (assemble-expr opts body) all-pats))
    (pop (asmopts-env opts))
    (unless main-pos
      (throw-msg! (format nil "grammar error: no main in ~%~s" `(grammar ,@pats))))
    (aref all-pats main-pos)))

(defpattern (grammar) (&rest (pats :pat))
  (progn
    (verify-args! '(&rest (name :tag) (pat :pat)) `(grammar ,@pats))
    (let* ((name-bodies (pairs pats))
	   (knames (mapcar #'first name-bodies))
	   (bodies (mapcar #'second name-bodies)))
      (unless (find :main knames)
	(throw-msg! (format nil "grammar error: no main in ~%~s" `(grammar ,@pats))))
      (push-simple-names opts knames)
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
  (assemble-grammar opts pats))

(defun compile-look (opts pat n)
  (with-gensyms ($matched?)
    `(with-save (curr caps tags accum)
       (,@(cond ((< n 0) `(when (<= ,n curr) (incf curr ,n))) ;; We need to check for underflow
	       ((> n 0) `(progn (incf curr ,n))) ;; Every primitive already checks for overflow
		 (t '(progn)))
	 (let ((,$matched? ,(compile-expr opts pat)))
	   (restore curr caps tags accum)
	   ,$matched?)))))

(defun assemble-look (opts pat n)
  (let ((p (assemble-expr opts pat)))
    (cond
      ((< n 0) (pat-lambda (curr caps tags accum)
		 (with-save (curr caps tags accum)
		   (when (<= n curr)
		     (incf curr n)
		     (let ((matched? (funcall p state)))
		       (restore curr caps tags accum)
		       matched?)))))
      ((> n 0) (pat-lambda (curr caps tags accum)
		 (with-save (curr caps tags accum)
		   (incf curr n)
		   (let ((matched? (funcall p state)))
		     (restore curr caps tags accum)
		     matched?))))
      (t (pat-lambda (curr caps tags accum)
	   (with-save (curr caps tags accum)
	     (let ((matched? (funcall p state)))
	       (restore curr caps tags accum)
	       matched?)))))))

(defpattern (look >) ((pat :pat) (n :num))
  (compile-look opts pat n)
  (assemble-look opts pat n))
(defpattern (not !) ((pat :pat))
  `(not ,(compile-look opts pat 0))
  (let ((p (assemble-look opts pat 0)))
    (pat-lambda () (not (funcall p state)))))
(defpattern (if) ((cond :pat) (pat :pat))
  `(and ,(compile-look opts cond 0) ,(compile-expr opts pat))
  (let ((c (assemble-look opts cond 0))
	(p (assemble-expr opts pat)))
    (pat-lambda ()
      (and (funcall c state) (funcall p state)))))
(defpattern (if-not) ((cond :pat) (pat :pat))
  `(and (not ,(compile-look opts cond 0)) ,(compile-expr opts pat))
  (let ((c (assemble-look opts cond 0))
	(p (assemble-expr opts pat)))
    (pat-lambda ()
      (and (not (funcall c state))
	   (funcall p state)))))

;; With tail call elim these should compile to code just as efficient
;; as handwritten loops- maybe even more efficient
(defpattern (thru) ((pat :pat))
  (compile-expr opts `(grammar :main (+ ,pat (* 1 :main))))
  (assemble-expr opts `(grammar :main (+ ,pat (* 1 :main)))))
(defpattern (to) ((pat :pat))
  (compile-expr opts `(thru (look ,pat 0)))
  (assemble-expr opts `(thru (look ,pat 0))))

(defpattern (sub) ((super-pat :pat) (sub-pat :pat))
  (with-gensyms ($matched $after-curr)
    `(with-save (curr)
       (let ((,$after-curr 0))
	 (declare (fixnum ,$after-curr))
	 (when ,(compile-expr opts super-pat)
	   (with-save (strlen)
	     (setf strlen curr)
	     (setf ,$after-curr curr)
	     (restore curr)
	     (let ((,$matched ,(compile-expr opts sub-pat)))
	       (restore strlen)
	       (setf curr ,$after-curr)
	       ,$matched))))))
  (let ((super (assemble-expr opts super-pat))
	(sub (assemble-expr opts sub-pat)))
    (pat-lambda (curr strlen)
      (with-save (curr)
	(let ((after-curr 0))
	  (declare (fixnum after-curr))
	  (when (funcall super state)
	    (with-save (strlen)
	      (setf strlen curr)
	      (setf after-curr curr)
	      (restore curr)
	      (let ((matched (funcall sub state)))
		(restore strlen)
		(setf curr after-curr)
		matched))))))))

(defpattern (split) ((div :pat) (inside :pat))
  (with-gensyms ($init-curr $after-curr $cont? $matched? $ret $fill)
    `(let ((,$init-curr 0)
	   (,$after-curr 0)
	   (,$ret nil)
	   (,$cont? nil)
	   (strlen-bak strlen)
	   (curr-bak curr))
       (declare (fixnum ,$init-curr ,$after-curr strlen-bak curr-bak)
		(boolean ,$ret ,$cont?))
       (tagbody
	  ,$fill
	  (setf curr-bak curr)
	  (setf ,$init-curr curr)
	  (loop until (setf ,$cont? ,(compile-expr opts `(drop ,div)))
		while (< curr strlen) do
		   (incf curr)
		   (incf ,$init-curr)
		finally
		   (setf strlen ,$init-curr)
		   (setf ,$after-curr curr)
		   (setf curr curr-bak))
	  (when-let ((,$matched? ,(compile-expr opts inside)))
	    (setf curr ,$after-curr)
	    (setf strlen strlen-bak)
	    (when ,$cont?
	      (go ,$fill))
	    (setf ,$ret t)))
       ,$ret))
  (let ((sep (assemble-expr opts `(drop ,div)))
	(in (assemble-expr opts inside)))
    (pat-lambda (curr strlen)
      (let ((init-curr 0)
	    (after-curr 0)
	    (ret nil)
	    (cont? nil)
	    (strlen-bak strlen)
	    (curr-bak curr))
	(declare (type fixnum init-curr after-curr strlen-bak curr-bak)
		 (type boolean ret cont?))
	(tagbody
	 fill
	   (setf curr-bak curr)
	   (setf init-curr curr)
	   (loop until (setf cont? (funcall sep state))
		 while (< curr strlen) do
		    (incf curr)
		    (incf init-curr)
		 finally
		    (setf strlen init-curr)
		    (setf after-curr curr)
		    (setf curr curr-bak))
	   (when-let ((matched? (funcall in state)))
	     (setf curr after-curr)
	     (setf strlen strlen-bak)
	     (when cont?
	       (go fill))
	     (setf ret t)))
	ret))))
		 
		 
