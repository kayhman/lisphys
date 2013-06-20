(in-package #:lisphys)

(defclass quaternion (math)
  ((x :accessor quat-x :initarg :x)
   (y :accessor quat-y :initarg :y)
   (z :accessor quat-z :initarg :z)
   (w :accessor quat-w :initarg :w)))

(setf q (make-instance 'quaternion :x 0. :y 0. :z 0. :w 1.))

(defmethod norm2 ((q quaternion))
"Compute the squared norm of a quaternion"
  (with-slots (x y z w (.+ add) (.* mult)) q
    (.+ (.* x x) (.* y y) (.* z z) (.* w w) )
      )
  )

(defmethod norm ((q quaternion))
  "Compute the norm of a quaternion"
  (with-slots ((.sqrt sqrt)) q
    (.sqrt (norm2 q) ))
  )

(defmethod conj ((q quaternion))
"Compute the conjugate of a quaternion"
  (with-slots (x y z w (.- sub)) q
    (make-instance (type-of q) :x (funcall .- x) :y (funcall .- y) :z (funcall .- z) :w w)))


(defmethod .* ((qa quaternion) (qb quaternion))
"Multiply two quaternions"
  (with-slots ((qax x) (qay y) (qaz z) (qaw w) (.+ add) (.* mult) (.- sub)) qa
    (with-slots ((qbx x) (qby y) (qbz z) (qbw w)) qb
      (make-instance (type-of qa) 
		     :x (funcall .+ (funcall .* qaw qbx) (funcall .* qax qbw) (funcall .* qay qbz) (funcall .- (funcall .* qaz qby)) ) 
		     :y (funcall .+ (funcall .* qaw qby) (funcall .* qay qbw) (funcall .* qaz qbx) (funcall .- (funcall .* qax qbz)) ) 
		     :z (funcall .+ (funcall .* qaw qbz) (funcall .* qaz qbw) (funcall .* qax qby) (funcall .- (funcall .* qay qbx)) )
		     :w (funcall .- (funcall .* qaw qbw) (funcall .* qax qbx) (funcall .* qay qby) (funcall .* qaz qbz)) ))))


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
  (with-slots ((.* mult) (.cos cos) (.sin sin)) v
    (let ((sinA (funcall .sin (funcall .* 0.5 a)))
	  (vn (normalize v)) )
      (with-slots (x y z) vn
	(make-instance (pick-class math-ad v "quaternion")
		       :x (funcall .* x sinA)
		       :y (funcall .* y sinA)
		       :z (funcall .* z sinA)
		       :w (funcall .cos (funcall .* 0.5 a) )) ))))

(defmethod print-object ((q quaternion) stream)
  (with-slots (x y z w) q
       (format t "~a : ~f ~f ~f ~f~%" (type-of q) x y z w)))

(setf e1 (make-instance 'vector3 :x 1.0 :y 0. :z 0. ))
(setf q1 (make-instance 'quaternion :x 0.0 :y 1. :z 0. :w 0. ))

(setq e1 (make-instance 'vector3 :x 1.0 :y 0. :z 0. ))
(setq e2 (make-instance 'vector3 :x 0.0 :y 1. :z 0. ))
(setq e3 (make-instance 'vector3 :x 0.0 :y 0. :z 1. ))

(= 0.0 (norm (.- e3 (cross e1 e2))))
