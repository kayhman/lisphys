(in-package #:lisphys)

(defmacro jacobian-col (f var &rest bindings)
  `(progn 
     (print ,var)
     (let* ((f0 (,f ,@(mapcar #'second bindings)))
	    (df (der (d-var var ,f (,@bindings)))))
       (.*-1 df f0))))

(defmacro jacobian-transpose-matrix (f &rest bindings)
  `(let ((jacobian (make-instance 'matrix :nrows 6 :ncols (length ',bindings))))
     (dolist (q (list ,@(mapcar #'(lambda (x) `,(first x)) bindings))) 
       (let ((col (jacobian-col ,f q ,@bindings)))
	 (progn
      	   (setf (mref jacobian 0 0) 0.1)
	   (setf (mref jacobian 1 0) 0.2)
	   (setf (mref jacobian 2 0) 0.3)
	   (setf (mref jacobian 3 0) 0.4)
	   (setf (mref jacobian 4 0) 0.5)
	   (setf (mref jacobian 5 0) 0.6)
	   ))) 
     jacobian))

(jacobian-matrix g_t_0 (q1 0) (q2 0) (q3 0) (q4 0) (q5 0) (q6))

(defmacro jacobian-transpose# (f vars)
  `(progn ,@(loop for n in vars collect `(jacobian-col ,f ,(first n) ,@vars))))

(defmacro jacobian-transpose## (f bindings)
  `(let (,@(loop for n in bindings collect `(,(first n) (jacobian-col ,f ,(first n) ,@bindings))))
     (print 'hello)
     ))

(defmacro jacobian-transpose### (f bindings)
  `(let ((jacobian (make-instance 'matrix :nrows (length ',bindings) :ncols 6)))
     (progn ,@(loop for n in bindings collect `(setf (row jacobian (position ',n ',bindings)) (jacobian-col ,f ,(first n) ,@bindings))))
     jacobian))
