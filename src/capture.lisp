(in-package :uclp)

(defmacro push-item! (item-place &optional tag)
  `(progn
     ,@(list-if tag `(tpush! tags ,tag ,item-place))
     (if accum?
	 (if (stringp ,item-place)
	     (apush! accum ,item-place 0 (length ,item-place))
	     (format accum "~a" ,item-place))
	 (qpush! caps ,item-place))))

(defmacro defcap (names spec &body body)
  `(defpattern ,names ,`((pat :pat) ,@spec &optional (tag :tag))
     ,@body))

(defcap (capture <- quote) ()
  `(with-save (curr)
     (when ,(compile-expr opts pat)
       (if accum?
	   (apush! accum str curr-bak curr)
	   (qpush! caps (subseq str curr-bak curr)))
       ,(when tag `(tpush! tags ,tag (subseq str curr-bak curr)))
       t)))

(defpattern (drop) ((pat :pat))
  (with-gensyms ($matched)
    `(with-save (caps accum tags)
       (let ((,$matched ,(compile-expr opts pat)))
	 (restore caps accum tags)
	 ,$matched))))

(defcap (accum %) ()
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
	       ,$matched))))))

(defcap (group) ()
  (with-gensyms ($matched $capped)
    `(with-save (caps accum?)
       (setf accum? nil)
       (let ((,$matched ,(compile-expr opts pat))
	     (,$capped (cdr ,(to->back 'caps))))
	 (restore accum? caps)
	 (when ,$matched
	   (push-item! ,$capped ,tag)
	   t)))))

(defcap (replace /) ((replacer :any))
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
	     t))))))

(defcap (cmt) ((replacer :any))
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
	       t)))))))

(defpattern (backref ->) ((tag :tag) &optional (other-tag :tag))
  (with-gensyms ($bind $val)
    `(let ((,$bind (backref tags ,tag)))
       (when ,$bind
	 (let ((,$val (tbind-value ,$bind)))
	   (push-item! ,$val ,other-tag))
	 t))))

(defpattern (backmatch) ((tag :tag))
  (with-gensyms ($bind $val $strc $valc)
    `(let ((,$bind (backref tags ,tag)))
       (when ,$bind
	 (let ((,$val (tbind-value ,$bind)))
	   (when (and (stringp ,$val)
		      (<= (+ (length ,$val) curr) strlen))
	     (loop for ,$valc of-type character across ,$val
		   for ,$strc of-type character = (char str curr)
		   do (if (char/= ,$valc ,$strc) (return nil))
		      (incf curr)
		   finally (return t))))))))

(defpattern (unref) ((pat :pat) &optional (tag :tag))
  (with-gensyms ($result)
    `(with-save (tags)
       (let ((,$result ,(compile-expr opts pat)))
	 (when ,$result
	   ,(if tag
		`(tscope-tag! tags tags-bak ,tag)
		`(restore tags))
	   ,$result)))))

(defpattern (constant) ((thing :any) &optional (tag :tag))
  (with-gensyms ($thing)
    `(let ((,$thing (quote ,thing)))
       (push-item! ,$thing ,tag)
       t)))

(defpattern (position $) (&optional (tag :tag))
  `(progn (push-item! curr ,tag) t))

(add-type! :index (list (lambda (n) (and (integerp n)
					 (>= n 0)))
			"a non-negative integer"))
(defpattern (argument) ((n :index) &optional (tag :tag))
  `(when (< -1 ,n (length args))
     (push-item! (aref args ,n) ,tag)
     t))

(defun peg-error? (name)
  (or (and (symbolp name)
	   (subtypep name 'condition))
      (stringp name)))

(add-type! :error (list #'peg-error? "error message or symbol denoting condition"))

(define-condition peg-error (error)
  ((pat :initarg :pat :reader error-pat)
   (matched :initarg :matched :reader error-matched)
   (caps :initarg :caps :reader error-caps))
  (:report (lambda (e s)
	     (let ((p (error-pat e))
		   (m (error-matched e)))
	       (when p
		 (when (/= (length m) 0)
		   (format s "~s is " m))
		 (format s "~s" p))))))

(defpattern (error) (&optional (pat :pat) (err :error))
  (with-gensyms ($matched? $capped)
    (if (and pat (subtypep (or err 'peg-error) 'peg-error))
	`(with-save (curr caps tags accum accum?)
	   (setf accum? nil)
	   (let ((,$matched? ,(compile-expr opts pat))
		 (,$capped (cdr caps-bak)))
	     (when ,$matched?
	       (error ',(or err 'peg-error)
		      :pat ',pat
		      :matched (subseq str curr-bak curr)
		      :caps ,$capped))
	     (restore curr caps tags accum accum?)
	     t))
	`(error ',(or err 'peg-error)))))

(defpattern (lenprefix) ((n-pat :pat) (r-pat :pat)) ;;Grouped in with capture because it manipulates the stack
  (with-gensyms ($matched $capped $count)
    `(with-save (accum? caps tags)
       (setf accum? nil)
       (let ((,$matched ,(compile-expr opts n-pat))
	     (,$capped (cdr caps-bak)))
	 (restore accum? caps tags)
	 (when (and ,$matched ,$capped)
	   (let ((,$count (first ,$capped)))
	     (when (positive-integer-p ,$count)
	       (loop repeat ,$count
		     for ,$matched = ,(compile-expr opts r-pat)
		     unless ,$matched do (return nil)
		     finally (return t)))))))))

(add-type! :radix (list (lambda (n)
			  (or (null n)
			      (and (integerp n) (<= 2 n 36)))
			  "integer between 2 and 36")))

(defpattern (integer) ((pat :pat) &optional (radix :radix) (tag :tag))
  (with-gensyms ($region $reg-len $int $int-len)
    `(with-save (curr)
       (when ,(compile-expr opts pat)
	 (let ((,$region (subseq str curr-bak curr))
	       (,$reg-len (- curr curr-bak)))
	   (multiple-value-bind (,$int ,$int-len)
	       (parse-integer ,$region :radix ,(or radix 10) :junk-allowed t)
	     (when (and ,$int (= ,$int-len ,$reg-len))
	       (push-item! ,$int ,tag)
	       t)))))))
