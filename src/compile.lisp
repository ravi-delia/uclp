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

(defstruct (pattern) name spec compile-fn closure-fn)
(defvar *patterns* (make-hash-table :test 'eq))

(defmacro defpattern ((name &rest aliases) spec compile-body &optional closure-body)
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
			 ,compile-body))
		     :closure-fn
		     ,(when closure-body
			`(lambda (opts ,$expr)
			   (declare (ignorable opts))
			   (destructuring-bind ,destruct (rest ,$expr)
			     ,closure-body))))))
	 (loop for ,$n in ',(cons name aliases)
	       do (setf (gethash (to-keyword ,$n) *patterns*) ,$pat))))))

(defparameter *aliases* nil)

(defstruct (compopts) prefix env holes)
(defun copts ()
  (make-compopts :prefix (gensym)
		 :env nil
		 :holes (make-array 0 :element-type 'keyword :adjustable t :fill-pointer t)))
(defstruct (asmopts (:include compopts)) all-patterns hole-args)
(defun aopts ()
  (make-asmopts :prefix (gensym)
		:env nil
		:holes (make-array 0 :element-type 'keyword :adjustable t :fill-pointer t)
		:all-patterns (make-array 0 :element-type 'function :adjustable t :fill-pointer t)
		:hole-args (make-array 0 :element-type 'function :adjustable t :fill-pointer t)))

(defun env-lookup (env name)
  (when env
    (or (cadr (find name (first env) :key #'first))
	(env-lookup (rest env) name))))
(defun push-env-scope (opts)
  (push (list) (compopts-env opts)))
(defun push-env-binding (opts name call-code)
  (let ((top-scope (pop (compopts-env opts))))
    (push `(,name ,call-code) top-scope)
    (push top-scope (compopts-env opts))))
(defun push-simple-names (opts names)
  (push-env-scope opts)
  (let ((prefix (compopts-prefix opts)))
    (loop for name in names
	  do (push-env-binding opts name `(,(prefsym prefix name))))))

(defun get-hole-idx! (opts name)
  (let* ((holes (compopts-holes opts)))
    (or (position name holes)
	(vector-push-extend name holes))))

(defstruct (pat) fn peg-src)
(defstruct (frame) hole-names frame-fn peg-src)
(defstruct (closure-peg) fn src)

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
  (accum? nil :type boolean)
  (line-map? nil :type boolean)
  (line-to 0 :type index)
  (line-map nil :type line-map))
(defun initialize-peg-state (str curr args)
  (make-peg-state
   :str str
   :curr curr
   :args args
   :strlen (length str)
   :caps (make-queue)
   :accum (make-accum)
   :tags (make-tstack)
   :accum? nil
   :line-map? nil
   :line-map (make-array 1 :element-type 'index :adjustable t :fill-pointer t)))

(deftype compiled-pattern () '(function (peg-state) boolean))
(deftype hole-args () '(simple-array compiled-pattern))

(defun top-declaration (quiet? debug? &rest items)
  (let ((muffler #+sbcl '(sb-ext:muffle-conditions sb-ext:compiler-note) #-sbcl nil))
    `(declare ,@(list-if quiet? muffler)
	      (optimize ,@(if debug?
			      '((speed 0))
			      '((debug 0) (speed 3))))
	      ,@items)))

(defun wrap-pattern-lambda (compiled quiet? debug?)
  `(lambda (state)
     ,(top-declaration quiet? debug? '(type peg-state state))
     (with-slots (str curr args strlen caps tags accum accum? line-map? line-map) state
       (declare (ignorable str curr args strlen caps tags accum accum? line-map line-map?))
       (if ,compiled
	   (values t (qitems caps))
	   nil))))

(defun wrap-frame-closure (compiled opts quiet? debug?)
  (let (($holeargs (prefsym (compopts-prefix opts) 'hole-args)))
    `(lambda (,$holeargs)
       ,(top-declaration quiet? debug? `(type hole-args ,$holeargs))
       ,(wrap-pattern-lambda compiled quiet? debug?))))

(defun compile-toplevel (expr &key (quiet? t) debug?)
  (unless expr
    (error "Nil is not a valid PEG"))
  (let* ((opts (copts))
	 (result (compile-expr opts expr)))
    (if (length= (compopts-holes opts) 0)
	(make-pat
	 :fn (compile nil (wrap-pattern-lambda result quiet? debug?))
	 :peg-src expr)
	(make-frame
	 :hole-names (compopts-holes opts)
	 :peg-src expr
	 :frame-fn (compile nil (wrap-frame-closure result opts quiet? debug?))))))

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
    ((pat-p expr) `(funcall ,(pat-fn expr) state))
    ((strform? expr) (compile-literal opts (from-strform expr)))
    ((keywordp expr) (or (env-lookup (compopts-env opts) expr)
			 (if-let ((assoced (assoc expr *aliases*)))
			   (compile-expr opts (cadr assoced)))
			 (error 'undefined-rule :name expr)))
    ((integerp expr) (compile-count opts expr))
    ((listp expr) (if-let ((pattern (gethash (to-keyword (first expr)) *patterns*)))
		    (and (verify-args! (pattern-spec pattern) expr)
			 (funcall (pattern-compile-fn pattern) opts expr))
		    (error 'unknown-pattern :name (first expr))))))

(defmacro pat-lambda (slots &body body)
  `(lambda (state)
     (declare (type peg-state state))
     (with-slots ,slots state ,@body)))

(defun assemble-expr (opts expr)
  (cond
    ; If we get an existing pat we unwrap it. This loses source info, which will be fixed later
    ((pat-p expr) (pat-fn expr)) 
    ((strform? expr) (assemble-literal opts (from-strform expr)))
    ((keywordp expr) (or (env-lookup (asmopts-env opts) expr)
			 (if-let (assoced (assoc expr *aliases*))
			   (pat-fn (caddr assoced)))
			 (error 'undefined-rule :name expr)))
    ((integerp expr) (assemble-count opts expr))
    ((listp expr) (if-let ((pattern (gethash (to-keyword (first expr)) *patterns*)))
		    (and (verify-args! (pattern-spec pattern) expr)
			 (if-let ((assembler (pattern-closure-fn pattern)))
			   (funcall assembler opts expr)
			   (error (format nil "~s is not implemented" (first expr)))))))))

(defun assemble-toplevel (expr &key (quiet? t) debug?)
  (declare (ignore quiet? debug?))
  (unless expr
    (error "Nil is not a valid PEG"))
  (let* ((opts (aopts))
	 (result (assemble-expr opts expr)))
    (if (length= (asmopts-holes opts) 0)
	(make-pat
	 :fn (pat-lambda (caps) (when (funcall result state) (values t (qitems caps))))
	 :peg-src expr)
	(make-frame
	 :hole-names (asmopts-holes opts)
	 :frame-fn (lambda (args)
		     (loop for arg across args
			   do (vector-push-extend arg (asmopts-hole-args opts)))
		     result)
	 :peg-src expr))))

(defun register-alias! (alias pattern)
  "Does not check pattern, but DO NOT put a recursive pattern. Probably
you shouldn't capture either"
  (unless (null pattern)
    (push (list alias pattern (compile-toplevel pattern)) *aliases*)))
(defun register-alias-suite! (alias pattern)
  "Adds pattern under alias, but also !alias, alias*, alias+, !alias*, !alias+"
  (let* ((sn (symbol-name alias))
	 (!sn (concatenate 'string "!" sn))
	 (sn+ (concatenate 'string sn "+"))
	 (!sn+ (concatenate 'string !sn "+"))
	 (sn* (concatenate 'string sn "*"))
	 (!sn* (concatenate 'string !sn "*"))
	 (sn? (concatenate 'string sn "?"))
	 (!sn? (concatenate 'string !sn "?"))
	 (!pattern `(if-not ,pattern 1)))
    (mapcar #'register-alias!
	    (mapcar #'to-keyword (list sn !sn sn+ !sn+ sn* !sn* sn? !sn?))
	    (list pattern !pattern
		  `(some ,pattern) `(some ,!pattern)
		  `(any ,pattern) `(any ,!pattern)
		  `(? ,pattern) `(? ,!pattern)))))

