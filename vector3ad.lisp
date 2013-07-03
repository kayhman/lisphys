(in-package #:lisphys)

(defclass vector3ad (math-ad vector3)
  ())

(defmethod (setf vector3-x) (x (v vector3ad))
  (setf (slot-value v 'x) 
	(if (numberp x)
	    (list x 0.)
	    x
	    )))

(defmethod (setf vector3-y) (y (v vector3ad))
  (setf (slot-value v 'y) 
	(if (numberp y)
	    (list y 0.)
	    y
	    )))

(defmethod (setf vector3-z) (z (v vector3ad))
  (setf (slot-value v 'z) 
	(if (numberp z)
	    (list z 0.)
	    z
	    )))




(defmethod initialize-instance :after ( (v vector3ad) &key x y z )
  (macrolet ((set-coord (c) 
	       `(if ,c (setf (,(symbolicate 'vector3- c) v) ,c)
			(setf (,(symbolicate 'vector3- c) v) '(0. 0.)))))
    (progn 
      (set-coord x)
      (set-coord y)
      (set-coord z))))
