(in-package #:lisphys)

(defclass vector3ad (math-ad vector3)
  ())

(defmethod initialize-instance :after ( (v vector3ad) &key x y z )
	   (progn 
	     (if (numberp x) (setf (vector3-x v ) `(,x 0.)) 
		 (if x (setf (vector3-x v ) x) 
		     (setf (vector3-x v ) '(0. 0.))))
	     (if (numberp y) (setf (vector3-y v ) `(,y 0.)) 
		 (if y (setf (vector3-y v ) y) 
		     (setf (vector3-y v ) '(0. 0.))))
	     (if (numberp z) (setf (vector3-z v ) `(,z 0.)) 
		 (if z (setf (vector3-z v ) x) 
		     (setf (vector3-z v ) '(0. 0.))))))

(setq e1a (make-instance 'vector3ad :x '(1 0) :y '(0 0) :z '(0 0)) )
(setq e2a (make-instance 'vector3ad :x '(0 0) :y '(1 0) :z '(0 0)) )
(setq e3a (make-instance 'vector3ad :x '(0 0) :y '(0 0) :z '(1 0)) )

(vector3-x (.+ e1a e2a))
(.- e1a e2a)
(vector3-x (.- e1a e2a))
(vector3-x (cross e1a e2a))

(vector3-x (.- e2a e1a))
(cross e1a e2a)
(norm (cross e1a e2a))
(class-of (cross e1a e2a))

(vector3-x (normalize e2a))
