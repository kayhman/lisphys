(in-package #:lisphys)

(defclass vector3 (math)
  ((x :reader vector3-x :initarg :x :initform 0.0)
   (y :reader vector3-y :initarg :y :initform 0.0)
   (z :reader vector3-z :initarg :z :initform 0.0)))

(defmethod (setf vector3-x) (x (v vector3))
  (setf (slot-value v 'x) x))

(defmethod (setf vector3-y) (y (v vector3))
  (setf (slot-value v 'y) y))

(defmethod (setf vector3-z) (z (v vector3))
  (setf (slot-value v 'z) z))


(defmethod .+ ((va vector3) (vb vector3))
"Add two vector3."
  (with-slots (x y z) va
    (with-slots ((tx x) (ty y) (tz z)) vb
      (with-ad va
	(make-instance (type-of va)
		       :x (!+ x tx) 
		       :y (!+ y ty) 
		       :z (!+ z tz) )))))

(defmethod .- ((va vector3) (vb vector3))
"Substract vector3 vb from va."
  (with-slots (x y z) va
    (with-slots ((tx x) (ty y) (tz z)) vb
      (with-ad va
	(make-instance (type-of va) 
		       :x (!- x tx)
		       :y (!- y ty) 
		       :z (!- z tz) )))))


(defmethod .* ((va vector3) a)
"Multiply vector3 va by scalar a."
  (with-ad va
    (with-slots (x y z) va
      (make-instance (type-of va) 
		     :x (!* x a)
		     :y (!* y a) 
		     :z (!* z a )))))

(defmethod axpy (a (x vector3) (y vector3))
  (.+ (.* x a) y))

;;(setq v (make-instance 'vector3 ))
;;(funcall (vector3-add v) 1 2 3)

;;(setq vp (make-instance 'vector3p ))
;;(funcall (vector3-add vp) 1 2 3)


(defmethod dot ((va vector3) (vb vector3))
"Compute the dot product of two vector3"
  (with-slots (x y z) va
    (with-slots ((tx x) (ty y) (tz z)) vb
      (with-ad va
	(!+ (!* x tx) (!* y ty) (!* z tz))))))

(defmethod cross ((va vector3) (vb vector3))
"Compute the cross product of two vector3"
  (with-slots ((u1 x) (u2 y) (u3 z)) va
    (with-slots ((v1 x) (v2 y) (v3 z)) vb
      (with-ad va
	(make-instance (type-of va)
		       :x (!- (!* u2 v3) (!* u3 v2)) 
		       :y (!- (!* u3 v1) (!* u1 v3)) 
		       :z (!- (!* u1 v2) (!* u2 v1)) )))))


(defmethod norm2 ((v vector3))
"Compute the squared norm of a vector3"
  (with-slots (x y z) v
    (dot v v)
      )
  )

(defmethod norm ((v vector3))
"Compute the norm of a vector3"
(with-ad v
    (!sqrt (norm2 v) )
  )
)

(defmethod normalize ((v vector3))
  "Compute the norm of a vector3"
  (with-slots (x y z ) v
    (let ((nrm (norm v)))
      (with-ad v
	(make-instance (type-of v)
		      :x (!/ x nrm)
		      :y (!/ y nrm)
		      :z (!/ z nrm) )))))

(defmethod is-null ((v vector3))
  "Return t if vectir i null. Avoid to compute the norm and its undefined derivative in 0."
  (with-slots (x y z) v
    (and (= (val x ) 0) (= (val y) 0) (= (val z) 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 AD methods                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod der ((v vector3))
  (with-slots (x y z) v 
	(progn
	  (print x)
	  (print y)
	  (print z)
	  (make-instance (pick-class math-ad v "vector3")
			 :x (der x)
			 :y (der y)
			 :z (der z)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Helper                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-dispatch-macro-character #\# #\v
			      #'(lambda (stream subchar arg)
				  (let* ((sexp (read stream t))
					 (ty (case (fourth sexp)
					       ('ad 'vector3ad)
					       (otherwise 'vector3 ))))
				    `(make-instance ',ty
						    :x ,(first sexp)
						    :y ,(second sexp)
						    :z ,(third sexp)
						    ))))

(defmethod print-object ((v vector3) stream)
  (with-slots (x y z) v
       (format t "~a : ~f ~f ~f ~%" (type-of v) x y z)))


