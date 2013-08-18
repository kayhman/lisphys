(in-package #:lisphys)

(defmacro jacobian-col (f var &rest bindings)
  `(let* ((f0 (,f ,@(mapcar #'second bindings)))
	    (df (der (d-var ,var ,f (,@bindings)))))
       (.*-1 df f0)))

(defmacro jacobian-transpose (f bindings)
  `(let ((jacobian (make-instance 'matrix :nrows (length ',bindings) :ncols 6)))
     (progn ,@(loop for n in bindings collect `(setf (row jacobian (position ',n ',bindings)) (jacobian-col ,f ,(first n) ,@bindings))))
     jacobian))

(macroexpand-1 '(jacobian-transpose g2_t_0 ((q0 0) (q1 0) (q2 0) (q3 0) (q4 0) (q5 0) (q6 0))))
(jacobian-transpose g2_t_0 ((q0 0) (q1 0) (q2 0) (q3 0) (q4 0) (q5 0) (q6 0)))
