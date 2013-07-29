(in-package #:lisphys)

(defclass vector3ad (math-ad vector3)  ())


(macrolet ((vector3ad-setf (c)
	     `(defmethod (setf ,(symbolicate 'vector3- c)) (,c (v vector3ad))
			  (setf (slot-value v ',c) 
				(if  (numberp ,c)
				    (list ,c 0.0)
				     ,c
				    )))))
  (progn
    (vector3ad-setf x)
    (vector3ad-setf y)
    (vector3ad-setf z)))

(defmethod initialize-instance :after ( (v vector3ad) &key x y z )
  (macrolet ((set-coord (c) 
	       `(if ,c (setf (,(symbolicate 'vector3- c) v) ,c)
			(setf (,(symbolicate 'vector3- c) v) '(0.0 0.0)))))
    (progn 
      (set-coord x)
      (set-coord y)
      (set-coord z))))
