(in-package #:lisphys)

(defclass twist ()
  ((linear :accessor lin :initarg :lin)
   (angular :accessor ang :initarg :ang)))

(setq tw (make-instance 'twist :lin (make-instance 'vector3 :x 1.0 :y 2.0 :z 3.0) :ang (make-instance 'vector3 :x 1e-3)))

(setq twad (make-instance 'twist :lin (make-instance 'vector3ad :x 1.0 :y 2.0 :z 3.0) :ang (make-instance 'vector3ad :x 3.0)))

(defmethod .exp ((tw twist) (eps number))
  (with-slots ((ang angular) (lin linear)) tw
    (with-slots ((!* mult)) ang
      (let* ((angNorm (if (is-null ang ) 0. (val (norm ang))))
	     (angNorm2  (val (* angNorm angNorm)))
	     (q (make-instance (pick-class math-ad ang "quaternion"))))
	(cond 
	  ((< angNorm2 eps)
	   (let ((scale  (!! (1.0 + (-1.0 + 0.0125 * angNorm2) * angNorm2 / 24.0)
			     / 2.0)))
	     (progn
	       (setf (quat-x q) (funcall !* scale (vector3-x ang)))
	       (setf (quat-y q) (funcall !* scale (vector3-y ang)))
	       (setf (quat-z q) (funcall !* scale (vector3-z ang)))
	       (setf (quat-w q) (!! (1.0 + (-1.0 + angNorm2 / 48.0) * angNorm2 / 8.0)))
	       (let ((trans (cross ang lin))
		     (s1 (!! (1.0 + (-1.0 + angNorm2 / 20.0) * angNorm2 / 6.0)))
		     (s2 (!! (((1.0 + (-1.0 + angNorm2 / 42.0) * angNorm2 * 0.05) / 6.0) * (val (dot ang lin))))))
		 (setf trans (.* trans (!! (0.5 + (-1.0 + angNorm2 / 30.0) * angNorm2 / 24.0))))
		 (setf trans (axpy s1 lin trans))
		 (setf trans (axpy s2 ang trans ))
		 (values trans q))
	       )))
	  (t
	   (let* ((angNorm-1  (/ 1.0 angNorm))
		 (angNorm2-1  (/ 1.0 angNorm2))
		 (sinVal (sin (* (sin (/ angNorm 2.0)) angNorm-1)))
		 (cosVal (cos (/ angNorm 2.0)))
		 )
	     (progn
	       (setf (quat-x q) (funcall !* sinVal (vector3-x ang)))
	       (setf (quat-y q) (funcall !* sinVal (vector3-y ang)))
	       (setf (quat-z q) (funcall !* sinVal (vector3-z ang)))
	       (setf (quat-w q) cosVal)
	       (let* ((trans (cross ang lin))
		     (s1 (* (sin angNorm) angNorm-1))
		     (s2 (* (- 1.0 s1) (val (dot ang lin)) angNorm2-1)))
		 (setf trans (axpy s1 lin trans))
		 (setf trans (axpy s2 ang trans ))
		 (values trans q))
	       ))

	   ))))))


(defmethod print-object ((tw twist) stream)
  (with-slots (linear angular) tw
       (format t "~a : ~%  l: ~a ~%  a: ~a ~%" (type-of tw) linear angular)))


(defun pos-exp (q1 q2)
  (.exp (make-instance 'twist :lin (.* (.* e1a 0.666) q1) 
		       :ang (.* e2a q2))
	1e-3))


(der (vector3-x (d-var q2 pos-exp ((q1 0.7) (q2 0.3) ))))
(der (vector3-x (d-var q1 pos-exp ((q1 0.7) (q2 0.3) ))))
