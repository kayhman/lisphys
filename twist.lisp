(in-package #:lisphys)

(defclass twist ()
  ((linear :accessor lin :initarg :lin)
   (angular :accessor ang :initarg :ang)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Arithmetic                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod .* ((tw twist) a)
"Multiply twist tw by scalar a."
  (with-slots (linear angular) tw
      (make-instance 'twist 
		     :lin (.* linear a)
		     :ang (.* angular a))))



(defmethod .exp ((tw twist) (eps number))
  (with-ad (ang tw) 
    (with-slots ((ang angular) (lin linear)) tw
      (let* ((angNorm (if (is-null ang )  0.0 (norm ang)))
	     (angNorm2 (funcall !* angNorm angNorm))
	     (q (make-instance (pick-class math-ad ang "quaternion"))))
	(cond 
	  ((< (val angNorm2) eps)
	   (let ((scale  (!!! ((1.0 + (-1.0 + 0.0125 * angNorm2) * angNorm2 / 24.0) / 2.0))))
	     (progn
	       (setf (quat-x q) (funcall !* scale (vector3-x ang)))
	       (setf (quat-y q) (funcall !* scale (vector3-y ang)))
	       (setf (quat-z q) (funcall !* scale (vector3-z ang)))
	       (setf (quat-w q) (!!! (1.0 + (-1.0 + angNorm2 / 48.0) * angNorm2 / 8.0)))
	       (let ((trans (cross ang lin))
		     (s1 (!!! (1.0 + (-1.0 + angNorm2 / 20.0) * angNorm2 / 6.0)))
		     (s2 (!!! (((1.0 + (-1.0 + angNorm2 / 42.0) * angNorm2 * 0.05) / 6.0) * (val (dot ang lin))))))
		 (setf trans (.* trans (!!! (0.5 + (-1.0 + angNorm2 / 30.0) * angNorm2 / 24.0))))
		 (setf trans (axpy s1 lin trans))
		 (setf trans (axpy s2 ang trans ))
		 (values trans q))
	       )))
	  (t
	   (let* ((angNorm-1  (!!! ( 1.0 / angNorm)))
		 (angNorm2-1  (!!! (1.0 / angNorm2)))
		 (sinVal (funcall !* (funcall !sin (funcall !/ angNorm 2.0)) angNorm-1))
		 (cosVal (funcall !cos (funcall !/ angNorm 2.0)))
		 )
	     (progn
	       (setf (quat-x q) (funcall !* sinVal (vector3-x ang)))
	       (setf (quat-y q) (funcall !* sinVal (vector3-y ang)))
	       (setf (quat-z q) (funcall !* sinVal (vector3-z ang)))
	       (setf (quat-w q) cosVal)
	       (let* ((trans (cross ang lin))
		      (s0 (funcall !* (funcall !- 1.0 (funcall !cos angNorm))  angNorm2-1))
		      (s1 (funcall !* (funcall !sin angNorm) angNorm-1))
		     (s2 (funcall !* (funcall !- 1.0 s1) (funcall !* (dot ang lin) angNorm2-1))))
		 (setf trans (.* trans s0))
		 (setf trans (axpy s1 lin trans))
		 (setf trans (axpy s2 ang trans ))
		 (make-instance 'displacement
				:pos trans
				:rot q))))))))))




(defmethod print-object ((tw twist) stream)
  (with-slots (linear angular) tw
       (format t "~a : ~%  l: ~a ~%  a: ~a ~%" (type-of tw) linear angular)))


(defun pos-exp (q1 q2)
  (.exp (make-instance 'twist :lin (.* (.* e1a 0.666) q1) 
		       :ang (.* e2a q2))
	1e-3))


(der (vector3-x (pos (d-var q2 pos-exp ((q1 0.7) (q2 0.3) )))))
(der (vector3-x (pos (d-var q1 pos-exp ((q1 0.7) (q2 0.3) )))))
