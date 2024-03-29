(in-package :uclp)

(defun match (rule str &optional (start 0) &rest args)
  (funcall (if (functionp rule) rule (compile-peg rule))
	   (initialize-peg-state
	    str
	    start
	    (loop with out = (make-array 0 :fill-pointer t :adjustable t)
		  for a in args do (vector-push-extend a out)
		  finally (return out)))))
(defun captured (rule str &optional (start 0) &rest args)
  (multiple-value-bind (matched? caps) (apply #'match rule str start args)
    (values caps matched?)))

(defun replace-all (match replace str &optional (start 0) &rest args)
  (multiple-value-bind (matched? caps)
      (apply #'match `(% (any (+ (/ '(drop ,match) ,replace) '1)))
	     str start args)
    (values (first caps) matched?)))

(defun replace-one (match replace str &optional (start 0) &rest args)
  (multiple-value-bind (matched? caps)
      (apply #'match `(% (* (<- (to ,match))
			    (/ '(drop ,match) ,replace)
			    (<- (any 1))))
	     str start args)
    (values (first caps) matched?)))

(defun find-all (match str &optional (start 0) &rest args)
  (multiple-value-bind (_ caps)
      (apply #'match `(any (* (to ,match) ($) (drop ,match))) ; we'd rather match twice than push and pop
	     str start args)
    (declare (ignore _))
    caps))
(defun find-one (match str &optional (start 0) &rest args)
  (multiple-value-bind (_ caps)
      (apply #'match `(* (to ,match) ($) (drop ,match))
	     str start args)
    (declare (ignore _))
    (first caps)))

(defun compile-peg (expr &rest opts)
  "Compile EXPR to a peg matcher, for use with uclp:match."
  (let ((out (compile nil (apply #'compile-toplevel expr opts))))
    out))
