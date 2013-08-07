(in-package #:lisphys)

(defmacro jacobian-col (f var &rest bindings)
  `(progn 
     (let ((f0 (,f ,@(mapcar #'second bindings)))
	   (dh (der (d-var ,var ,f (,@bindings				    
				    )))))
       (.*-1 dh f0))))

