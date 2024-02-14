(defpackage :uclp/test
  (:use :cl :uclp :fiveam :alexandria))
(in-package :uclp/test)

(defun matches? (pat str start args &optional (expect-caps nil check-caps?))
  (multiple-value-bind (success? caps) (apply #'uclp:match pat str start args)
    (and success?
	 (if check-caps?
	     (equalp caps expect-caps)
	     t))))
(defmacro is-match (pat str &key (start 0) (args nil) (result nil check-result?))
  `(fiveam:is (matches? ,pat ,str ,start ,args ,@(if check-result? (list result)))))
(defmacro isnt-match (pat str &key (start 0) (args nil))
  `(fiveam:is (not (matches? ,pat ,str ,start ,args))))

(defmacro check-pat (pat &body inputs)
  `(progn
     ,@(loop with mode = nil
	     for input in inputs
	     if (eq input :match) do (setf mode 'is-match)
	     else if (eq input :fail) do (setf mode 'isnt-match)
	     else if (stringp input) collect (list mode pat input)
	     else if (consp input) collect `(,mode ,pat ,@input))))
					      
(fiveam:def-suite uclp)

(defun run-tests! () (fiveam:run! 'uclp))
