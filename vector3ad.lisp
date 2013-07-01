(in-package #:lisphys)

(defclass vector3ad (math-ad vector3)
  ())

(defmethod initialize-instance :after ( (v vector3ad) &key x y z )
  (macrolet ((set-coord (c) 
	       `(if (numberp ,c) (setf (,(symbolicate 'vector3- c) v)  `(,,c 0.))
		    (if ,c (setf (,(symbolicate 'vector3- c) v) ,c)
			(setf (,(symbolicate 'vector3- c) v) '(0. 0.))))))
    (progn 
      (set-coord x)
      (set-coord y)
      (set-coord z))))
