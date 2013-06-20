(in-package #:lisphys)

(defclass twist (math)
  ((linear :accessor lin :initarg :lin)
   (angular :accessor ang :initarg :ang)))

(setq tw (make-instance 'twist :lin (make-instance 'vector3) :ang (make-instance 'vector3)))


(defmethod .exp ((tw twist) (eps number))
  (with-slots ((ang angular) (lin linear)) tw
    (let* ((angNorm (norm ang))
	   (angNorm2 (* angNorm angNorm))
	   (q (make-instance 'quaternion)))
      (cond 
	((< angNorm2 eps)
	 (let ((scale  (!! (1.0 + (-1.0 + 0.0125 * angNorm2) * angNorm2 / 24.0)
			   / 2.0)))
	   (progn
	     (setf (quat-x q) (* scale (vector3-x ang)))
	     (setf (quat-y q) (* scale (vector3-y ang)))
	     (setf (quat-z q) (* scale (vector3-z ang)))
	     (setf (quat-w q) (!! (1.0 + (-1.0 + angNorm2 / 48.0) * angNorm2 / 8.0)))
	     (let ((trans (cross ang lin))
		   (s1 (!! (1.0 + (-1.0 + angNorm2 / 20.0) * angNorm2 / 6.0)))
		   (s2 (!! (((1.0 + (-1.0 + angNorm2 / 42.0) * angNorm2 * 0.05) / 6.0) * (dot ang lin)))))
	       (setf trans (.* trans (!! (0.5 + (-1.0 + angNorm2 / 30.0) * angNorm2 / 24.0))))
	       (setf trans (axpy s1 lin trans))
	       (setf trans (axpy s2 ang trans ))
	        (values trans q) )
	     ))
	 
	 )
	(else
	 (print "hello"))
	))))


