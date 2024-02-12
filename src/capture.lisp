(in-package :peg)

(defun push-item! (opts item-place &optional tag)
  (with-opts (opts)
    `(progn
       ,@(list-if tag `(peg:tpush! ,tags ,tag ,item-place))
       (if ,accum?
	   (if (stringp ,item-place)
	       (peg:apush! ,accum ,item-place 0 (length ,item-place))
	       (format ,accum "~a" ,item-place))
	   (peg:qpush! ,caps ,item-place)))))
       

(defun compile-capture (opts expr)
  (destructuring-bind (pat &optional tag) (rest expr)
    (with-opts (opts)
      `(let (,(save curr))
	 (when ,(compile-expr opts pat)
	   (if ,accum?
	       (peg:apush! ,accum ,str ,curr-bak ,curr)
	       (peg:qpush! ,caps (subseq ,str ,curr-bak ,curr)))
	   ,(when tag `(peg:tpush! ,tags ,tag (subseq ,str ,curr-bak ,curr)))
	   t)))))

(defun compile-argument (opts expr)
  (destructuring-bind (n &optional tag) (rest expr)
    (with-opts (opts)
      `(when (< -1 ,n (length ,args))
	 ,(push-item! opts `(aref ,args ,n) tag)))))

(defun compile-accum (opts expr)
  (destructuring-bind (pat &optional tag) (rest expr)
    (with-opts (opts $matched $accumed)
      `(if ,accum?
	   ,(compile-expr opts pat)
	   (progn
	     (setf ,accum? t)
	     (peg:with-save (,accum)
	       (let ((,$matched ,(compile-expr opts pat)))
		 (when ,$matched
		   (let ((,$accumed (subseq ,accum ,(to->back accum) (length ,accum))))
		     (unless (emptyp ,$accumed)
		       (peg:qpush! ,caps ,$accumed)
		       ,(if tag `(peg:tpush! ,tags ,tag ,$accumed)))))
		 ,(restore accum)
		 (setf ,accum? nil)
		 ,$matched)))))))

(defun compile-group (opts expr)
  (destructuring-bind (pat &optional tag) (rest expr)
    (with-opts (opts $matched $capped)
      `(peg:with-save (,caps ,accum?)
	 (setf ,accum? nil)
	 (let ((,$matched ,(compile-expr opts pat))
	       (,$capped (cdr ,(to->back caps))))
	   ,(restore accum? caps)
	   (when ,$matched
	     ,(push-item! opts $capped tag)
	     t))))))

(defun compile-replace (opts expr)
  (destructuring-bind (pat replacer &optional tag) (rest expr)
    (with-opts (opts $matched? $capped $result)
      `(peg:with-save (,caps ,accum?)
	 (setf ,accum? nil)
	 (let ((,$matched? ,(compile-expr opts pat))
	       ,@(list-if (functionp replacer) `(,$capped (cdr ,(to->back caps)))))
	   ,(restore caps accum?)
	   (when ,$matched?
	     (let ((,$result ,(if (functionp replacer)
				  `(apply ,replacer ,$capped)
				  replacer)))
	       ,(push-item! opts $result tag)
	       t)))))))

(defun compile-backref (opts tag &optional other-tag)
  (with-opts (opts $bind $val)
    `(let ((,$bind (peg:backref ,tags ,tag)))
       (when ,$bind
	 (let ((,$val (peg:tbind-value ,$bind)))
	   ,(push-item! opts $val other-tag))
	 t))))

(defun compile-backmatch (opts tag)
  (with-opts (opts $bind $val $strc $valc)
    `(let ((,$bind (peg:backref ,tags ,tag)))
       (when ,$bind
	 (let ((,$val (peg:tbind-value ,$bind)))
	   (when (and (stringp ,$val)
		      (<= (+ (length ,$val) ,curr) (length ,str)))
	     (loop for ,$valc of-type character across ,$val
		   for ,$strc of-type character = (char ,str ,curr)
		   do (if (char/= ,$valc ,$strc) (return nil))
		      (incf ,curr)
		   finally (return t))))))))

(defun compile-unref (opts expr)
  (destructuring-bind (pat &optional tag) (rest expr)
    (with-opts (opts $result)
      `(peg:with-save (,tags)
	 (let ((,$result ,(compile-expr opts pat)))
	   (when ,$result
	     ,(if tag
		  `(peg::tscope-tag! ,tags ,(to->back tags) ,tag)
		  (restore tags))
	     ,$result))))))
