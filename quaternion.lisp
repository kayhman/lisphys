(defclass quaternion ()
  ((x :accessor quat-x :initarg :x)
   (y :accessor quat-y :initarg :y)
   (z :accessor quat-z :initarg :z)
   (w :accessor quat-w :initarg :w))
)

(setf q (make-instance 'quaternion :x 0. :y 0. :z 0. :w 1.))

(defmethod norm2 ((q quaternion))
"Compute the squared norm of a quaternion"
  (with-slots (x y z w) q
    (+ (* x x) (* y y) (* z z) (* w w) )
      )
  )

(defmethod norm ((q quaternion))
"Compute the norm of a quaternion"
  (sqrt (norm2 q) )
)

(defmethod conj ((q quaternion))
"Compute the conjugate of a quaternion"
  (with-slots (x y z w ) q
    (make-instance 'quaternion :x (- x) :y (- y) :z (- z) :w w)
    )
  )


(defmethod .* ((qa quaternion) (qb quaternion))
"Multiply two quaternions"
  (with-slots ((qax x) (qay y) (qaz z) (qaw w)) qa
    (with-slots ((qbx x) (qby y) (qbz z) (qbw w)) qb
      (make-instance 'quaternion 
		     :x (+ (* qaw qbx) (* qax qbw) (* qay qbz) (- (* qaz qby)) ) 
		     :y (+ (* qaw qby) (* qay qbw) (* qaz qbx) (- (* qax qbz)) ) 
		     :z (+ (* qaw qbz) (* qaz qbw) (* qax qby) (- (* qay qbx)) )
		     :w (+ (* qaw qbw) (* qax qbx) (* qay qby) (* qaz qbz)) )
      )
    )
  )


(defmethod .* ((q quaternion) (v vector3))
"Multiply a vector vy a quaternion"
  (with-slots ((qx x) (qy y) (qz z) (qw w)) q
    (with-slots ((vx x) (vy y) (vz z)) v
      (let* ((qconj (conj q)) 
	    (qv (make-instance 'quaternion :x vx :y vy :z vz :w 0.))
	    (rqv (.* q (.* qv qconj))) )
       (make-instance 'vector3
		     :x (quat-x rqv)  
		     :y (quat-y rqv)  
		     :z (quat-z rqv) ))
      )
    )
  )

(defmethod from-axis ((v vector3) (a float))
  (let ((sinA (sin a))
	(vn (normalize v)) )
    (with-slots (x y z) vn
      (make-instance 'quaternion
		     :x (* x sinA)
		     :y (* y sinA)
		     :z (* z sinA)
		     :w (cos a) ) 
      )
    )
  )


(setf e1 (make-instance 'vector3 :x 1.0 :y 0. :z 0. ))
(setf q1 (make-instance 'quaternion :x 0.0 :y 1. :z 0. :w 0. ))

(defmethod .- ((va vector3) (vb vector3))
"Substract vector3 vb from va."
  (with-slots (x y z) va
    (with-slots ((tx x) (ty y) z(tz z)) vb
      (make-instance 'vector3 :x (- x tx) :y (- y ty) :z (- z tz) )
      )
  )

    )

(setq e1 (make-instance 'vector3 :x 1.0 :y 0. :z 0. ))
(setq e2 (make-instance 'vector3 :x 0.0 :y 1. :z 0. ))
(setq e3 (make-instance 'vector3 :x 0.0 :y 0. :z 1. ))

(= 0.0 (norm (.- e3 (cross e1 e2))))
