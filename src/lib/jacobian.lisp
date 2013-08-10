(in-package #:lisphys)

(defmacro jacobian-col (f var &rest bindings)
  `(progn 
     (print ,var)
     (let* ((f0 (,f ,@(mapcar #'second bindings)))
	    (df (der (d-var var ,f (,@bindings)))))
       (.*-1 df f0))))

(defmacro jacobian-transpose (f bindings)
  `(let ((jacobian (make-instance 'matrix :nrows (length ',bindings) :ncols 6)))
     (progn ,@(loop for n in bindings collect `(setf (row jacobian (position ',n ',bindings)) (jacobian-col ,f ,(first n) ,@bindings))))
     jacobian))
