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

(defun compile-peg (expr &rest opts)
  "Compile EXPR to a peg matcher, for use with uclp:match."
  (let ((out (compile nil (apply #'compile-toplevel expr opts))))
    out))

