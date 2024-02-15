(in-package :uclp)

(defmacro push-item! (item-place &optional tag)
  `(progn
     ,@(list-if tag `(tpush! tags ,tag ,item-place))
     (if accum?
	 (if (stringp ,item-place)
	     (apush! accum ,item-place 0 (length ,item-place))
	     (format accum "~a" ,item-place))
	 (qpush! caps ,item-place))))
       

(defun compile-capture (opts expr)
  (destructuring-bind (pat &optional tag) (rest expr)
    `(with-save (curr)
       (when ,(compile-expr opts pat)
	 (if accum?
	     (apush! accum str curr-bak curr)
	     (qpush! caps (subseq str curr-bak curr)))
	 ,(when tag `(tpush! tags ,tag (subseq str curr-bak curr)))
	 t))))

(defun compile-drop (opts pat)
  (with-gensyms ($matched)
    `(with-save (caps accum tags)
       (let ((,$matched ,(compile-expr opts pat)))
	 (restore caps accum tags)
	 ,$matched))))

(defun compile-accum (opts expr)
  (destructuring-bind (pat &optional tag) (rest expr)
    (with-gensyms ($matched $accumed)
      `(if accum?
	   ,(compile-expr opts pat)
	   (progn
	     (setf accum? t)
	     (with-save (accum)
	       (let ((,$matched ,(compile-expr opts pat)))
		 (when ,$matched
		   (let ((,$accumed (subseq accum ,(to->back 'accum) (length accum))))
		     (unless (emptyp ,$accumed)
		       (qpush! caps ,$accumed)
		       ,(if tag `(tpush! tags ,tag ,$accumed)))))
		 (restore accum)
		 (setf accum? nil)
		 ,$matched)))))))

(defun compile-group (opts expr)
  (destructuring-bind (pat &optional tag) (rest expr)
    (with-gensyms ($matched $capped)
      `(with-save (caps accum?)
	 (setf accum? nil)
	 (let ((,$matched ,(compile-expr opts pat))
	       (,$capped (cdr ,(to->back 'caps))))
	   (restore accum? caps)
	   (when ,$matched
	     (push-item! ,$capped ,tag)
	     t))))))

(defun compile-replace (opts expr)
  (destructuring-bind (pat replacer &optional tag) (rest expr)
    (with-gensyms ($matched? $capped $result)
      `(with-save (caps accum?)
	 (setf accum? nil)
	 (let ((,$matched? ,(compile-expr opts pat))
	       ,@(list-if (functionp replacer) `(,$capped (cdr ,(to->back 'caps)))))
	   (restore caps accum?)
	   (when ,$matched?
	     (let ((,$result ,(if (functionp replacer)
				  `(apply ,replacer ,$capped)
				  `',replacer)))
	       (push-item! ,$result ,tag)
	       t)))))))
(defun compile-cmt (opts expr)
  (destructuring-bind (pat replacer &optional tag) (rest expr)
    (with-gensyms ($matched $capped $result)
      `(with-save (caps accum?)
	 (setf accum? nil)
	 (let ((,$matched ,(compile-expr opts pat))
	       ,@(list-if (functionp replacer) `(,$capped (cdr caps-bak))))
	   (restore caps accum?)
	   (when ,$matched
	     (let ((,$result ,(if (functionp replacer)
				  `(apply ,replacer ,$capped)
				  `',replacer)))
	       (when ,$result
		 (push-item! ,$result ,tag)
		 t))))))))

(defun compile-backref (opts tag &optional other-tag)
  (declare (ignore opts))
  (with-gensyms ($bind $val)
    `(let ((,$bind (backref tags ,tag)))
       (when ,$bind
	 (let ((,$val (tbind-value ,$bind)))
	   (push-item! ,$val ,other-tag))
	 t))))

(defun compile-backmatch (opts tag)
  (declare (ignore opts))
  (with-gensyms ($bind $val $strc $valc)
    `(let ((,$bind (backref tags ,tag)))
       (when ,$bind
	 (let ((,$val (tbind-value ,$bind)))
	   (when (and (stringp ,$val)
		      (<= (+ (length ,$val) curr) (length str)))
	     (loop for ,$valc of-type character across ,$val
		   for ,$strc of-type character = (char str curr)
		   do (if (char/= ,$valc ,$strc) (return nil))
		      (incf curr)
		   finally (return t))))))))

(defun compile-unref (opts expr)
  (destructuring-bind (pat &optional tag) (rest expr)
    (with-gensyms ($result)
      `(with-save (tags)
	 (let ((,$result ,(compile-expr opts pat)))
	   (when ,$result
	     ,(if tag
		  `(tscope-tag! tags tags-bak ,tag)
		  `(restore tags))
	     ,$result))))))

(defun compile-constant (expr)
  (destructuring-bind (thing &optional tag) (rest expr)
    (with-gensyms ($thing)
      `(let ((,$thing (quote ,thing)))
	 (push-item! ,$thing ,tag)
	 t))))

(defun compile-position (?tag)
  `(progn (push-item! curr ,?tag) t))

(defun compile-argument (opts expr)
  (declare (ignore opts))
  (destructuring-bind (n &optional tag) (rest expr)
    `(when (< -1 ,n (length args))
       (push-item! (aref args ,n) ,tag)
       t)))
