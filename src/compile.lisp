(in-package :uclp)

;; Facilities for defining new combinators
(defparameter *argtype*
  `((:string ,#'strform? "string")
    (:tag ,#'keywordp "keyword")
    (:pat ,(lambda (x) (declare (ignore x)) t))))

(defun check-arg (argspec arg)
  (cond
    ((symbolp argspec) t)
    ((listp argspec)
      (destructuring-bind (name fn/type &optional expectstr) argspec
	(declare (ignore name))
	(if (keywordp fn/type)
	    (check-arg (assoc fn/type *argtype*) arg)
	    (if (funcall fn/type arg)
		t
		(values nil expectstr)))))))

(define-condition grammar-error (error)
  ((expr :initarg :expr :reader expr)
   (expected :initarg :expected :reader expected)
   (got :initarg :got :reader got))
  (:report (lambda (c s)
	     (format
	      s "grammar error in ~s, expected ~a, got ~s"
	      (expr c) (expected c) (got c)))))

(defun grammar-error (expr expected got)
  (error 'grammar-error :expr expr :expected expected :got got))

(defun verify-positionals! (spec expr &optional (reqarity (length spec) inexact?))
  "Verify that the positional arguments in SPEC line up with EXPR. REQARITY
should be one more than the last position index. On fail, signal appropriate
grammar-error"
  (let ((args (rest expr)))
    (when (and (not inexact?) (/= (length args) reqarity))       ; all positional
      (grammar-error expr (format nil "~s args" reqarity) (length args))) 
    (unless (>= (length args) reqarity)                                   ; not all positional
      (grammar-error expr (format nil "at least ~s args" reqarity) (length args)))
    (loop repeat reqarity
	  for argspec in spec
	  for arg in args
	  do (multiple-value-bind (match? errstr) (check-arg argspec arg)
	       (unless match? (grammar-error expr errstr arg))))
    t))

(defun verify-optionals! (spec expr reqarity)
  (let* ((args (rest expr))
	 (optspec (subseq spec (1+ reqarity)))
	 (optargs (subseq args reqarity)))
    (unless (<= (length optargs) (length optspec))
      (grammar-error expr (format nil "at most ~a args" (1- (length spec))) (length args)))
    (loop for argspec in optspec
	  for arg in optargs
	  do (multiple-value-bind (match? errstr) (check-arg argspec arg)
	       (unless match? (grammar-error expr errstr arg))))
    t))

(defun verify-rest! (spec expr reqarity)
  (let ((argspec (nth (1+ reqarity) spec))
	(restargs (subseq expr (1+ reqarity))))
    (loop for arg in restargs
	  do (multiple-value-bind (match? errstr) (check-arg argspec arg)
	       (unless match? (grammar-error expr errstr arg)))))
  t)

(defun verify-args! (spec expr)
  (if-let ((reqarity (position-if
		      (lambda (p)
			(and (symbolp p)
			     (case (to-keyword p)
			       (:&optional t)
			       (:&rest t)
			       (t nil))))
		      spec)))
    (and (verify-positionals! spec expr reqarity)
	 (case (to-keyword (nth reqarity spec))
	   (:&optional (verify-optionals! spec expr reqarity))
	   (:&rest (verify-rest! spec expr reqarity))))
    (verify-positionals! spec expr)))

(defstruct (pattern) name spec compile-fn)
(defparameter *patterns* (make-hash-table :test 'eq))

(defmacro defpattern ((name &rest aliases) spec &body body)
  (let ((destruct (mapcar (lambda (o) (if (listp o) (first o) o)) spec)))
    (with-gensyms ($pat $expr $n)
      `(let ((,$pat (make-pattern
		     :name ,name :spec ,spec
		     :compile-fn
		     (lambda (opts ,$expr)
		       (destructuring-bind ,destruct expr
			 ,@body)))))
	 (loop for ,$n in ,(cons name aliases)
	       do (setf (gethash ,$n *patterns*) ,$pat))))))
