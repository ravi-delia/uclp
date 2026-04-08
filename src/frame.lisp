(in-package :uclp)

(define-condition missing-hole-error (error)
  ((name :initarg :name :reader name))
  (:report (lambda (c s)
	     (format s "filling error: missing ~s" (name c)))))

(define-condition unknown-hole-error (error)
  ((name :initarg :name :reader name)
   (names :initarg :names :reader names))
  (:report (lambda (c s)
	     (if (<= (length (names c)) 5)
		 (format s "filling error: name ~s not among allowed names ~S" (name c) (names c))
		 (format s "filling error: ~S is not the name of any hole" (name c))))))

(defpattern (hole) ((name :tag))
  (let ((hole-args-varname (prefsym (compopts-prefix opts) 'hole-args))
	(idx (get-hole-idx! opts name)))
    `(funcall (the function (aref ,hole-args-varname ,idx)) state))
  (let ((idx (get-hole-idx! opts name))
	(args (asmopts-hole-args opts)))
    (pat-lambda () (funcall (the function (aref args idx)) state))))

(defun %fill-holes (frame args)
  (let* ((arg-alist (pairs args))
	 (holes (frame-hole-names frame))
	 (hole-args (make-array (length holes)
				:element-type 'function
				:initial-element #'identity)))
    (loop for hole across holes
	  for i from 0
	  do (if-let ((pat (cadr (assoc hole arg-alist))))
	       (if-let ((val (assemble-toplevel pat))) 
		 (setf (aref hole-args i) (pat-fn val))
		 (error 'missing-hole-error :name hole))
	       (error 'missing-hole-error :name hole)))
    (loop for (param nil) in arg-alist
	  do (unless (find param holes)
	       (error 'unknown-hole-error :name param :names holes)))
    (make-pat
     :fn (funcall (frame-frame-fn frame) hole-args)
     :peg-src (frame-peg-src frame))))

