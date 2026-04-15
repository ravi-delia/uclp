(in-package :uclp)

;; Aliases are kind of a part of the public interface
;; but mostly this is because they need all my patterns defined
(defparameter *base-aliases*
  '(:d (range "09")
    :a (range "az" "AZ")
    :w (range "az" "AZ" "09")
    :s (set (#\tab #\return #\newline #\null #\page #\vt #\space))
    :h (range "09" "af" "AF")))

(mapcar (lambda (pair) (register-alias-suite! (first pair) (second pair)))
	(pairs *base-aliases*))

(defun compile-peg (expr &key (quiet? t) debug?)
  "Compile EXPR to a peg matcher, for use with uclp:match."
  (compile-toplevel expr :quiet? quiet? :debug? debug?))

(defun assemble-peg (expr &key (quiet? t) debug?)
  "Assemble EXPR to a peg matcher, for use with uclp:match."
  (assemble-toplevel expr :quiet? quiet? :debug? debug?))

(defun fill-holes (frame &rest args)
  (unless (frame-p frame)
    (error (format nil "~a is not a frame" frame)))

  (%fill-holes frame args))

(defun match (rule str &optional (start 0) &rest args)
  (let ((fn (cond
	      ((pat-p rule) (pat-fn rule))
	      (t (pat-fn (assemble-toplevel rule))))))
    (funcall fn
	     (initialize-peg-state
	      str
	      start
	      (loop with out = (make-array 0 :fill-pointer t :adjustable t)
		    for a in args do (vector-push-extend a out)
		    finally (return out))))))
(defun captured (rule str &optional (start 0) &rest args)
  (multiple-value-bind (matched? caps) (apply #'match rule str start args)
    (values caps matched?)))

(defun replace-all (match replace str &optional (start 0) &rest args)
  (multiple-value-bind (matched? caps)
      (apply #'match
	     (compile-peg `(% (any (+ (/ '(drop ,match) ,replace) '1))))
	     str start args)
    (values (first caps) matched?)))

(defun replace-one (match replace str &optional (start 0) &rest args)
  (multiple-value-bind (matched? caps)
      (apply #'match
	     (compile-peg `(% (* (<- (to ,match))
				 (/ '(drop ,match) ,replace)
				 (<- (any 1)))))
	     str start args)
    (values (first caps) matched?)))

(defparameter *find-all-frame*
  (compile-peg `(any (+ (* ($) (drop (hole :looking-for))) 1))))

(defun find-all (match str &optional (start 0) &rest args)
  (multiple-value-bind (_ caps)
      (apply #'match
	     (fill-holes *find-all-frame* :looking-for match)
	     str start args)
    (declare (ignore _))
    caps))

(defparameter *find-one-frame*
  (compile-peg '(* (to (hole :looking-for)) ($))))

(defun find-one (match str &optional (start 0) &rest args)
  (multiple-value-bind (_ caps)
      (apply #'match
	     (fill-holes *find-one-frame* :looking-for match)
	     str start args)
    (declare (ignore _))
    (first caps)))
