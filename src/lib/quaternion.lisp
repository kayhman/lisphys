(in-package #:lisphys)

(defclass quaternion (math)
  ((x :reader quat-x :initarg :x :initform 0.0)
   (y :reader quat-y :initarg :y :initform 0.0)
   (z :reader quat-z :initarg :z :initform 0.0)
   (w :reader quat-w :initarg :w :initform 1.0)))


(defmethod (setf quat-x) (x (q quaternion))
  (setf (slot-value q 'x) x))

(defmethod (setf quat-y) (y (q quaternion))
  (setf (slot-value q 'y) y))

(defmethod (setf quat-z) (z (q quaternion))
  (setf (slot-value q 'z) z))

(defmethod (setf quat-w) (w (q quaternion))
  (setf (slot-value q 'w) w))

(setf q (make-instance 'quaternion :x 0. :y 0. :z 0. :w 1.))

(defmethod norm2 ((q quaternion))
"Compute the squared norm of a quaternion"
  (with-ad q
    (with-slots (x y z w) q
      (!+ (!* x x) (!* y y) (!* z z) (!* w w) )
      )))


(defmethod norm ((q quaternion))
  "Compute the norm of a quaternion"
  (with-ad q
     (!sqrt (norm2 q) )))

(defmethod conj ((q quaternion))
"Compute the conjugate of a quaternion"
  (with-slots (x y z w (.- sub)) q
    (with-ad q
	(make-instance (type-of q) :x (!- x) :y (!- y) :z (!- z) :w w))))

(defmethod .* ((q quaternion) (a number))
  "Multiply a quaternion by a scalar"
  (with-ad q
    (with-slots (x y z w) q
      (make-instance (pick-class math-ad q "quaternion")
		     :x (!* x a)
		     :y (!* y a)
		     :z (!* z a)
		     :w (!* w a)))))

(defmethod .* ((qa quaternion) (qb quaternion))
"Multiply two quaternions"
  (with-slots ((qax x) (qay y) (qaz z) (qaw w) ) qa
    (with-slots ((qbx x) (qby y) (qbz z) (qbw w)) qb
      (with-ad qa
	(make-instance (type-of qa) 
		       :x (!+ (!* qaw qbx) (!* qax qbw) (!* qay qbz) (!- (!* qaz qby)) ) 
		       :y (!+ (!* qaw qby) (!* qay qbw) (!* qaz qbx) (!- (!* qax qbz)) ) 
		       :z (!+ (!* qaw qbz) (!* qaz qbw) (!* qax qby) (!- (!* qay qbx)) )
		       :w (!- (!* qaw qbw) (!* qax qbx) (!* qay qby) (!* qaz qbz)) )))))


(defmethod .* ((q quaternion) (v vector3))
"Multiply a vector by a quaternion"
  (with-slots ((qx x) (qy y) (qz z) (qw w) ) q
    (with-slots ((vx x) (vy y) (vz z) ) v
      (let* ((qconj (conj q)) 
	    (qv (make-instance (type-of q) :x vx :y vy :z vz :w 0.))
	    (rqv (.* q (.* qv qconj))) )
       (make-instance (type-of v)
		     :x (quat-x rqv)  
		     :y (quat-y rqv)  
		     :z (quat-z rqv) )))))

(defmethod from-axis ((v vector3) a )
  (with-ad v
    (let ((sinA (!sin (!* 0.5 a)))
	  (vn (normalize v)) )
      (with-slots (x y z) vn
	  (make-instance (pick-class math-ad v "quaternion")
			 :x (!* x sinA)
			 :y (!* y sinA)
			 :z (!* z sinA)
			 :w (!cos (!* 0.5 a) ))) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 AD methods                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod der ((q quaternion))
  (with-slots (x y z w) q 
	(make-instance (pick-class math-ad q "quaternion")
		  :x (der x)
		  :y (der y)
		  :z (der z)
		  :w (der w))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Helper                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-dispatch-macro-character #\# #\q
			      #'(lambda (stream subchar arg)
				  (let* ((sexp (read stream t))
					 (ty (case (fifth sexp)
					       ('ad 'quaternionad)
					       (otherwise 'quaternion ))))
				    `(make-instance ',ty
						    :x ,(first sexp)
						    :y ,(second sexp)
						    :z ,(third sexp)
						    :w ,(fourth sexp)
						    ))))


(defmethod print-object ((q quaternion) stream)
  (with-slots (x y z w) q
       (format t "~a : ~f ~f ~f ~f ~%" (type-of q) x y z w)))

(setf e1 (make-instance 'vector3 :x 1.0 :y 0. :z 0. ))
(setf q1 (make-instance 'quaternion :x 0.0 :y 1. :z 0. :w 0. ))

(setq e1 (make-instance 'vector3 :x 1.0 :y 0. :z 0. ))
(setq e2 (make-instance 'vector3 :x 0.0 :y 1. :z 0. ))
(setq e3 (make-instance 'vector3 :x 0.0 :y 0. :z 1. ))

(= 0.0 (norm (.- e3 (cross e1 e2))))
