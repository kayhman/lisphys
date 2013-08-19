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

;; MLS P.54
(defmethod .*-1 ((d-der displacement) (d displacement))
"Multiply twist tw by displacement d."
  (with-slots ((drot rot) pos) d
    (with-slots ((der-rot rot) (der-pos pos)) d-der
      (with-ad pos
	(let* ((quat-pure #q ((der (quat-x der-rot))
			      (der (quat-y der-rot))
			      (der (quat-z der-rot))
			      '(0. 0.)
			      ad))
	       (ang-vel (.* (.* quat-pure (conj drot)) 2.0))
	       (ang-vel-vec #v ((quat-x ang-vel)
				(quat-y ang-vel)
				(quat-z ang-vel)
				ad
				)))
	  (make-instance 'twist 
			 :lin (der der-pos)
			 :ang ang-vel-vec))))))
  



(defmethod .exp ((tw twist) &optional (eps 1e-6))
"Map an element of se(3) to SE(3) using the exponential map"
  (with-ad (ang tw) 
    (with-slots ((angl angular) (line linear)) tw
      (let* ((angNorm (if (is-null angl ) '(0.0 0.0) (norm angl)))
	     (angNorm2 (funcall !* angNorm angNorm))
	     (q (make-instance (pick-class math-ad angl "quaternion"))))
	(cond 
	  ((< (val angNorm2) eps)
	   (let ((scale  (!!! ((1.0 + (-1.0 + 0.0125 * angNorm2) * angNorm2 / 24.0) / 2.0))))
	     (progn
	       (setf (quat-x q) (funcall !* scale (vector3-x angl)))
	       (setf (quat-y q) (funcall !* scale (vector3-y angl)))
	       (setf (quat-z q) (funcall !* scale (vector3-z angl)))
	       (setf (quat-w q) (!!! (1.0 + (-1.0 + angNorm2 / 48.0) * angNorm2 / 8.0)))
	       (let ((trans (cross angl line))
		     (s1 (!!! (1.0 + (-1.0 + (angNorm2 / 20.0)) * angNorm2 / 6.0)))
		     (s2 (!!! (((1.0 + (-1.0 + (angNorm2 / 42.0)) * (angNorm2 * 0.05)) / 6.0) * (val (dot angl line))))))
		 (setf trans (.* trans (!!! (0.5 + (-1.0 + angNorm2 / 30.0) * angNorm2 / 24.0))))
		 (setf trans (axpy s1 line trans))
		 (setf trans (axpy s2 angl trans ))
		 (make-instance 'displacement
				:pos trans
				:rot q))
	       )))
	  (t
	   (let* ((angNorm-1  (!!! ( 1.0 / angNorm)))
		 (angNorm2-1  (!!! (1.0 / angNorm2)))
		 (sinVal (funcall !* (funcall !sin (funcall !/ angNorm 2.0)) angNorm-1))
		 (cosVal (funcall !cos (funcall !/ angNorm 2.0)))
		 )
	     (progn
	       (setf (quat-x q) (funcall !* sinVal (vector3-x angl)))
	       (setf (quat-y q) (funcall !* sinVal (vector3-y angl)))
	       (setf (quat-z q) (funcall !* sinVal (vector3-z angl)))
	       (setf (quat-w q) cosVal)
	       (let* ((transl (cross angl line))
		      (s0 (funcall !* (funcall !- 1.0 (funcall !cos angNorm))  angNorm2-1))
		      (s1 (funcall !* (funcall !sin angNorm) angNorm-1))
		     (s2 (funcall !* (funcall !- 1.0 s1) (funcall !* (dot angl line) angNorm2-1))))
		 (setf transl (.* transl s0))
		 (setf transl (axpy s1 line transl))
		 (setf transl (axpy s2 angl transl ))
		 (make-instance 'displacement :pos transl :rot q)
		 )))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Helper                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-dispatch-macro-character #\# #\t
			      #'(lambda (stream subchar arg)
				  (let* ((sexp (read stream t))
					 (lin (first sexp))
					 (ang (second sexp))
					 (tyl (case (fourth lin)
					       ('ad 'vector3ad)
					       (otherwise 'vector3 )))
					 (tya (case (fourth ang)
					       ('ad 'vector3ad)
					       (otherwise 'vector3 ))))
				    `(make-instance 'twist
						    :lin (make-instance ',tyl :x ,(first lin) :y ,(second lin) :z ,(third lin))
						    :ang (make-instance ',tya :x ,(first ang) :y ,(second ang) :z ,(third ang))
						    ))))


(defmethod print-object ((tw twist) stream)
  (with-slots (linear angular) tw
       (format t "~a : ~%  l: ~a ~%  a: ~a ~%" (type-of tw) linear angular)))


(defun pos-exp (q1 q2)
  (.exp (make-instance 'twist :lin (.* (.* e1a 0.666) q1) 
		       :ang (.* e2a q2))
	1e-3))


(der (vector3-x (pos (d-var q2 pos-exp ((q1 0.7) (q2 0.3) )))))
(der (vector3-x (pos (d-var q1 pos-exp ((q1 0.7) (q2 0.3) )))))
