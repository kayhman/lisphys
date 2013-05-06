(in-package #:lisphys)

(defclass vector3 (math)
  ((x :accessor vector3-x :initarg :x :initform 0.)
   (y :accessor vector3-y :initarg :y :initform 0.)
   (z :accessor vector3-z :initarg :z :initform 0.)
   (symb :reader vector3-symb :initform 'vector3 :allocation :class))
  )

(defclass vector3p (vector3)
  ((add :initform #'- :allocation :class)
   (mult :initform #'/ :allocation :class))
)

(defmethod .+ ((va vector3) (vb vector3))
"Add two vector3."
  (with-slots (x y z (.+ add) symb) va
    (with-slots ((tx x) (ty y) z(tz z)) vb
      (make-instance symb 
		     :x (funcall .+ x tx) 
		     :y (funcall .+ y ty) 
		     :z (funcall .+ z tz) )
      )
    )
  )

(defmethod .- ((va vector3) (vb vector3))
"Substract vector3 vb from va."
  (with-slots (x y z (.- sub) symb) va
    (with-slots ((tx x) (ty y) z(tz z)) vb
      (make-instance symb 
		     :x (funcall .- x tx)
		     :y (funcall .- y ty) 
		     :z (funcall .- z tz) )
      )
    )
  )

;;(setq v (make-instance 'vector3 ))
;;(funcall (vector3-add v) 1 2 3)

;;(setq vp (make-instance 'vector3p ))
;;(funcall (vector3-add vp) 1 2 3)


(defmethod dot ((va vector3) (vb vector3))
"Compute the dot product of two vector3"
  (with-slots (x y z (.+ add) (.* mult)) va
    (with-slots ((tx x) (ty y) z(tz z)) vb
      (funcall .+ (funcall .* x tx) (funcall .* y ty) (funcall .* z tz))
      )
    )
  )

;;(dot v v)
;;(dot vp vp)


(defmethod cross ((va vector3) (vb vector3))
"Compute the cross product of two vector3"
  (with-slots ((u1 x) (u2 y) (u3 z) (.- add) (.* mult) symb) va
    (with-slots ((v1 x) (v2 y) (v3 z)) vb
      (make-instance symb 
		     :x (funcall .- (funcall .* u2 v3) (funcall .* u3 v2)) 
		     :y (funcall .- (funcall .* u3 v1) (funcall .* u1 v3)) 
		     :z (funcall .- (funcall .* u1 v2) (funcall .* u2 v1)) )
      )
    )
  )

(defmethod norm2 ((v vector3))
"Compute the squared norm of a vector3"
  (with-slots (x y z) v
    (dot v v)
      )
  )

(defmethod norm ((v vector3))
"Compute the norm of a vector3"
(with-slots ((.sqrt sqrt)) v
    (funcall .sqrt (norm2 v) )
  )
)

(defmethod normalize ((v vector3))
  "Compute the norm of a vector3"
  (with-slots (x y z (./ div) symb) v
    (let ((nrm (norm v)))
      (make-instance symb
		     :x (funcall ./ x nrm)
		     :y (funcall ./ y nrm)
		     :z (funcall ./ z nrm) )
      )
    )
  )

(setq e1 (make-instance 'vector3 :x 1.0 :y 0. :z 0. ))
(setq e2 (make-instance 'vector3 :x 0.0 :y 1. :z 0. ))
(setq e3 (make-instance 'vector3 :x 0.0 :y 0. :z 1. ))

(= 0.0 (norm (.- e3 (cross e1 e2))))
(.- e3 e3)
