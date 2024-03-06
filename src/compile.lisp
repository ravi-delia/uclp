(in-package :uclp)

;; Facilities for defining new combinators
(defparameter *argtype*
  `((:string ,#'strform? "string")
    (:tag ,#'keywordp "keyword")
    (:num ,#'integerp "integer")
    (:reps ,#'positive-integer-p "positive integer")
    (:pat ,(lambda (x) (declare (ignore x)) t))
    (:any ,(lambda (x) (declare (ignore x)) t))))

(defun add-type! (type typeform)
  (if-let ((bind (assoc type *argtype*)))
    (setf (cdr bind) typeform)
    (push (cons type typeform) *argtype*)))

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

(defun circularize! (list) (setf (cdr (last list)) list) list)
(defun verify-rest! (spec expr reqarity)
  (let ((argspecs (circularize! (subseq spec (1+ reqarity))))
	(restargs (subseq expr (1+ reqarity))))
    (loop for arg in restargs
	  for argspec in argspecs
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
(defvar *patterns* (make-hash-table :test 'eq))

(defmacro defpattern ((name &rest aliases) spec &body body)
  "Bind pattern with name and aliases whose arguments obey SPEC.
In BODY, OPTS is anaphorically bound"
  (let ((destruct (mapcar (lambda (o) (if (listp o) (first o) o)) spec))
	(qspec `(list ,@(loop for s in spec
			      if (listp s)
			      collect `(list ',(first s) ,@(rest s))
			      else collect `(quote ,s)))))
    (with-gensyms ($pat $expr $n)
      `(let ((,$pat (make-pattern
		     :name ',name :spec ,qspec
		     :compile-fn
		     (lambda (opts ,$expr)
		       (declare (ignorable opts))
		       (destructuring-bind ,destruct (rest ,$expr)
			 ,@body)))))
	 (loop for ,$n in ',(cons name aliases)
	       do (setf (gethash (to-keyword ,$n) *patterns*) ,$pat))))))

(defparameter *aliases* nil)
(defun register-alias! (alias pattern)
  "Does not check pattern, but DO NOT put a recursive pattern. Probably
you shouldn't capture either"
  (unless (null pattern)
    (push (cons alias pattern) *aliases*)))
(defun register-alias-suite! (alias pattern)
  "Adds pattern under alias, but also !alias, alias*, alias+, !alias*, !alias+"
  (let* ((sn (symbol-name alias))
	 (!sn (concatenate 'string "!" sn))
	 (sn+ (concatenate 'string sn "+"))
	 (!sn+ (concatenate 'string !sn "+"))
	 (sn* (concatenate 'string sn "*"))
	 (!sn* (concatenate 'string !sn "*"))
	 (!pattern `(if-not ,pattern 1)))
    (mapcar #'register-alias!
	    (mapcar #'to-keyword (list sn !sn sn+ !sn+ sn* !sn*))
	    (list pattern !pattern
		  `(some ,pattern) `(some ,!pattern)
		  `(any ,pattern) `(any ,!pattern)))))

(defparameter *base-aliases*
  '(:d (range "09")
    :a (range "az" "AZ")
    :w (range "az" "AZ" "09")
    :s (set (#\tab #\return #\newline #\null #\page #\vt #\space))
    :h (range "09" "af" "AF")))

(mapcar (lambda (pair) (register-alias-suite! (first pair) (second pair)))
	(pairs *base-aliases*))

(defstruct (compopts) prefix env)
(defun copts (prefix env)
  (make-compopts :prefix prefix :env env))

(defun print-peg-state (state stream depth)
  (declare (ignore depth state))
  (print "peg state" stream))
(defstruct (peg-state (:print-function print-peg-state))
  (str "" :type simple-string)
  (curr 0 :type fixnum)
  (args #() :type vector)
  (strlen 0 :type fixnum)
  (caps nil :type queue)
  (accum "" :type accum)
  (tags #() :type tstack)
  (accum? nil :type boolean))
(defun initialize-peg-state (str curr args)
  (make-peg-state
   :str str
   :curr curr
   :args args
   :strlen (length str)
   :caps (make-queue)
   :accum (make-accum)
   :tags (make-tstack)
   :accum? nil))

(defun compile-toplevel (expr &key (quiet? t) debug?)
  `(lambda (state)
     (declare ,@(list-if
		 quiet?
		 '(sb-ext:muffle-conditions sb-ext:compiler-note))
	      (optimize ,@(if debug?
			      '((speed 0))
			      '((debug 0) (speed 3))))
	      (peg-state state))
     (with-slots (str curr args strlen caps tags accum accum?) state
       (declare (ignorable str curr args strlen caps tags accum accum?))
       (if ,(compile-expr (make-compopts) expr)
	   (values t (qitems caps))
	   nil))))

(defun env-lookup (env name)
  (when env
    (or (find name (first env))
	(env-lookup (rest env) name))))

(define-condition unknown-pattern (error)
  ((name :initarg :name :reader ukpat-name))
  (:report (lambda (c s)
	     (format s "~s is not a recognized pattern"
		     (ukpat-name c)))))

(define-condition undefined-rule (error)
  ((rule-name :initarg :name :reader rule-name))
  (:report (lambda (c s)
	     (format s "~s is not defined in the current environment."
		     (rule-name c)))))

(defun compile-expr (opts expr)
  (cond
    ((functionp expr) `(funcall ,expr state))
    ((strform? expr) (compile-literal opts (from-strform expr)))
    ((keywordp expr) (or (if (env-lookup (compopts-env opts) expr)
			     (list (prefsym (compopts-prefix opts) expr)))
			 (if-let ((assoced (assoc expr *aliases*)))
			   (compile-expr opts (cdr assoced))) 
			 (error 'undefined-rule :rule-name expr)))
    ((integerp expr) (compile-count opts expr))
    ((listp expr) (if-let ((pattern (gethash (to-keyword (first expr)) *patterns*)))
		    (and (verify-args! (pattern-spec pattern) expr)
			 (funcall (pattern-compile-fn pattern) opts expr))
		    (error 'unknown-pattern :name (first expr))))))
