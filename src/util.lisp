(in-package :uclp)

;; Semi-Norvig Queues: We need to implement the capture stack as a queue, but we will
;; never actually need to dequeue! So we can make optimizations which are otherwise
;; impossible.
;; A queue is just a cons where the head points to the last cell and the tail points to
;; the first, UNLESS its empty, in which cass the head points to the cell itself.


(defmacro dec-inlines (&rest fn-decs)
  (flet ((to-ftype (typedec)
	   (destructuring-bind (fname argtypes outtype) typedec
	     `(ftype (function ,argtypes ,outtype) ,fname))))
    (let ((fn-names (mapcar #'first fn-decs))
	  (ftypes (mapcar #'to-ftype fn-decs)))
      `(declaim ,@ftypes
		,(cons 'inline fn-names)))))

(deftype queue () 'cons)
(dec-inlines
 (make-queue () queue)
 (qempty? (queue) boolean)
 (qitems (queue) list)
 (qpush! (queue t) null)
 (qsave (queue) cons)
 (qrestore! (queue cons) null))
(defun make-queue ()
  (let ((out (cons nil nil)))
    (setf (car out) out)
    out))
(defun qempty? (q) (eq q (car q)))
(defun qitems (q) (cdr q))

(defun qpush! (q item)
  (setf (car q) (setf (cdar q) (cons item nil)))
  nil)
(defun qpush-all! (q items) (loop for i in items do (qpush! q i)))

(defun qsave (q) (car q))
(defun qrestore! (q saved)
  "Should take a queue and the car of that queue at some earlier point"
  (setf (car q) saved)
  (setf (cdar q) nil))

;; Accum String: Just an extendable string with save and reload semantics

(deftype accum () '(vector character *))
(dec-inlines
 (make-accum () accum)
 (aempty? (accum) boolean)
 (apush! (accum string fixnum fixnum) null)
 (asave (accum) fixnum)
 (arestore! (accum fixnum) null))

(defun make-accum () (make-array 0 :adjustable t :fill-pointer t :element-type 'character))
(defun aempty? (a) (= (length a) 0))

(defun apush! (a s start end)
  (loop for i from start below end
	do (vector-push-extend (char s i) a)))

(defun asave (a) (length a))
(defun arestore! (a saved) (setf (fill-pointer a) saved) nil)

;; Tag Stack: Literally just an alist searched for a tag from beginning to end

(defstruct tbind (tag :scope :type keyword) value)

(deftype tstack () '(vector tbind *))
(dec-inlines
 (make-tstack () tstack)
 (tpush! (tstack keyword t) null)
 (tpush-scope! (tstack) null)
 (backref (tstack keyword) (or tbind null))
 (tsave (tstack) fixnum)
 (trestore! (tstack fixnum) null)
 (tscope-tag! (tstack fixnum keyword) null))

(defun make-tstack () (make-array 0 :adjustable t :fill-pointer t :element-type 'tbind))

(defun tpush! (tstack key item)
  (vector-push-extend (make-tbind :tag key :value item) tstack)
  nil)

(defun backref (tstack key)
  (loop for bind across tstack
	if (eq (tbind-tag bind) key) do
	   (return bind)))

(defun tsave (tstack)
  (length tstack))
(defun trestore! (tstack save)
  (setf (fill-pointer tstack) save)
  nil)
(defun tscope-tag! (tstack save tag)
  (loop with goodc = save
	for i from save below (length tstack)
	for bind = (aref tstack i)
	if (not (eq (tbind-tag bind) tag)) do
	;; goodc points to the end of the known good binds. If we encounter a bad bind,
	;; i will advance and goodc won't. Most of the library is ripped from Bakpakin, but
	;; this specific implementation I never would have thought of on my own. Seeing it
	;; in the codebase made me confident it would work.
	   (rotatef (aref tstack i) (aref tstack goodc))
	   (incf goodc)
	finally (setf (fill-pointer tstack) goodc))
  nil)

(defun strform? (strform)
  (or (stringp strform)
      (characterp strform)
      (and (listp strform) (every #'strform? strform))))
(defun from-strform (strlikes)
  "Any place we normally take a string, also accept a list of strings and/or chars"
  (if (or (stringp strlikes) (characterp strlikes))
      (string strlikes)
      (reduce (lambda (s1 s2) (concatenate 'string s1 s2))
	      strlikes
	      :key #'string)))

(defun pairs (list)
  (cond
    ((null list) nil)
    ((null (cdr list)) (list list))
    (t (destructuring-bind (a b &rest rest) list
	 (cons (list a b) (pairs rest))))))

(define-condition generic-error (error)
  ((msg :initarg :msg :reader error-msg))
  (:report (lambda (c s) (format s "~a" (error-msg c)))))
(defun throw-msg! (msg) (error 'generic-error :msg msg))
  

;; str str-end args curr curr-bak caps caps-bak tags tags-bak accum accum? accum-bak

(defstruct (compopts) prefix env)
(defun copts (prefix env)
  (make-compopts :prefix prefix :env env))

(defun to-keyword (name)
  (intern (if (symbolp name) (symbol-name name) name)
	  (find-package :keyword)))

(defun prefsym (prefix symbol)
  (intern (concatenate
	   'string
	   (symbol-name prefix)
	   "/"
	   (symbol-name symbol))))
(defun to->back (symbol)
  (intern (concatenate 'string (symbol-name symbol) "-BAK")
	  (symbol-package symbol)))

(defun list-if (x obj)
  (when x (list obj)))

(define-condition bad-place (error) (place))
(defun save-for (place)
  (case (to-keyword place)
    (:curr place)
    (:caps `(qsave ,place))
    (:tags `(tsave ,place))
    (:accum? place)
    (:strlen place)
    (:accum `(asave ,place))
    (t (error 'bad-place :place place))))

(defun save (&rest places)
  (if (= (length places) 1)
      (let ((place (first places)))
	(list (to->back place) (save-for place)))
      (mapcar #'save places)))
(defmacro with-save (places &body body)
  (let* ((save-slots (mapcar #'to->back places))
	 (save-methods (mapcar #'save-for places))
	 (bindings (mapcar #'list save-slots save-methods)))
    `(let ,bindings
       ,@body)))

(defmacro checkpoint (&rest places)
  `(setf ,@(mapcan
	    (lambda (p)
	      (list (to->back p) (save-for p)))
	    places)))

(defun restore-for (place)
  (case (to-keyword place)
    (:curr `(setf ,place ,(to->back place)))
    (:caps `(qrestore! ,place ,(to->back place)))
    (:tags `(trestore! ,place ,(to->back place)))
    (:accum? `(setf ,place ,(to->back place)))
    (:accum `(arestore! ,place ,(to->back place)))
    (:strlen `(setf ,place ,(to->back place)))
    (t (error 'bad-place :place place))))

(defmacro restore (&rest places)
  `(progn ,@(mapcar #'restore-for places)))
