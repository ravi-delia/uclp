(in-package :uclp)

(defun match (rule str &optional (start 0) &rest args)
  (funcall (if (functionp rule) rule (compile-peg rule))
	   str start (loop with out = (make-array 0 :fill-pointer t :adjustable t)
			   for a in args do (vector-push-extend a out)
			   finally (return out))))
(defun captured (rule str &optional (start 0) &rest args)
  (multiple-value-bind (matched? caps) (apply #'match rule str start args)
    (and matched? caps)))

(defun replacing (match replace str &optional (start 0) &rest args)
  (multiple-value-bind (_ caps)
      (apply #'match `(% (any (+ (/ ',match ,replace) '1)))
	     str start args)
    (declare (ignore _))
    (and caps (first caps))))

(defun compile-peg (expr &optional (quiet? t))
  "Compile EXPR to a peg matcher, for use with uclp:match."
  (let ((out (compile nil (compile-toplevel expr :quiet? quiet?))))
    out))


(defun compile-toplevel (expr &key quiet?)
  `(lambda (str curr args)
     (declare ,@(list-if
		 quiet?
		 '(sb-ext:muffle-conditions sb-ext:compiler-note))
	      (ignorable str curr args)
	      (dynamic-extent curr)
	      (fixnum curr)
	      (string str)
	      (vector args))
     (let ((strlen (length str)) ; Modifiable for use in SUB
	   (caps (make-queue))
	   (tags (make-tstack))
	   (accum (make-accum))
	   (accum? nil))
       (declare (ignorable strlen caps tags accum accum?)
		(dynamic-extent accum? strlen)
		(fixnum strlen)
		(tstack tags)
		(accum accum)
		(boolean accum?))
       (if ,(compile-expr (make-compopts) expr)
	   (values t (qitems caps))
	   nil))))

(defun env-lookup (env name)
  (when env
    (or (find name (first env))
	(env-lookup (rest env) name))))

(define-condition unknown-pattern (error) (bad-sym text))
(define-condition undefined-rule (error) (rule-name))

(defun compile-expr (opts expr)
  (cond
    ((strform? expr) (compile-literal opts (from-strform expr)))
    ((keywordp expr) (if (env-lookup (compopts-env opts) expr)
			 (list (prefsym (compopts-prefix opts) expr))
			 (error 'undefined-rule :rule-name expr)))
    ((integerp expr) (compile-count opts expr))
    ((listp expr)
      (case (to-keyword (first expr))
	(:range (compile-range opts expr))
	(:set (compile-set opts (from-strform (second expr))))

	(:sequence (compile-sequence opts expr))
	(:* (compile-sequence opts expr))

	(:choice (compile-choice opts expr))
	(:+ (compile-choice opts expr))

	(:any (compile-several opts (second expr) 0))
	(:some (compile-several opts (second expr) 1))
	(:between (destructuring-bind (pat min &optional max) (rest expr)
		    (compile-several opts pat min max)))
	(:at-most (compile-several opts (second expr) (third expr)))
	(:at-least (compile-several opts (second expr) (third expr)))
	(:repeat (compile-several opts (second expr) (third expr) (third expr)))
	(:opt (compile-opt opts (second expr)))
	(:? (compile-opt opts (second expr)))

	(:look (compile-look opts (second expr) (third expr)))
	(:> (compile-look opts (second expr) (third expr)))
	(:not (compile-not opts (second expr)))
	(:! (compile-not opts (second expr)))
	(:if (compile-if opts (second expr) (third expr)))
	(:if-not (compile-if-not opts (second expr) (third expr)))

	(:thru (compile-thru opts (second expr)))
	(:to (compile-to opts (second expr)))

	(:sub (compile-sub opts (second expr) (third expr)))

	(:capture (compile-capture opts expr))
	(:quote (compile-capture opts expr))
	(:<- (compile-capture opts expr))

	(:drop (compile-drop opts (second expr)))

	(:constant (compile-constant expr))

	(:position (compile-position (second expr)))
	(:$ (compile-position (second expr)))

	(:argument (compile-argument opts expr))

	(:replace (compile-replace opts expr))
	(:/ (compile-replace opts expr))
	(:cmt (compile-cmt opts expr))

	(:accumulate (compile-accum opts expr))
	(:% (compile-accum opts expr))

	(:group (compile-group opts expr))

	(:backref (destructuring-bind (tag &optional other-tag) (rest expr)
		    (compile-backref opts tag other-tag)))
	(:backmatch (compile-backmatch opts (second expr)))
	(:unref (compile-unref opts expr))

	(:integer (compile-integer opts expr))
	(:lenprefix (compile-lenprefix opts expr))

	(:grammar (compile-grammar opts (rest expr)))

	(t (error 'unknown-pattern
		  :bad-sym (first expr)
		  :text "Symbol is not recognized primitive pattern or combinator."))))))
		       

