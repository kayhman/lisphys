(in-package #:lisphys)

(defclass quaternionad (math-ad quaternion)
  ())

(defmethod initialize-instance :after ( (q quaternionad) &key x y z w)
	   (macrolet ((set-coord (c) 
	       `(if ,c (setf (,(symbolicate 'quat- c) q) ,c)
		    (setf (,(symbolicate 'quat- c) q) '(0.0 0.0)))))
	     (progn 
	       (set-coord x)
	       (set-coord y)
	       (set-coord z)
	       (set-coord w))))


(macrolet ((quat-setf (c)
	     `(defmethod (setf ,(symbolicate 'quat- c)) (,c (q quaternionad))
			  (setf (slot-value q ',c) 
				(if  (numberp ,c)
				    (list ,c 0.0)
				     ,c
				    )))))
  (progn
    (quat-setf x)
    (quat-setf y)
    (quat-setf z)
    (quat-setf w)))

(setf q1a (make-instance 'quaternionad :x '(0. 0.) :y '(1. 0.) :z '(0. 0.) :w '(0. 0.)))


(quat-x (from-axis e1a '(3.1416 0)))
(quat-y (from-axis e1a '(3.1416 0)))
(quat-z (from-axis e1a '(3.1416 0)))
(quat-w (from-axis e1a '(3.1416 0)))


(vector3-x (.* (from-axis e1a '(3.1416 0)) e3a))
(vector3-y (.* (from-axis e1a '(3.1416 0)) e3a))
(vector3-z (.* (from-axis e1a '(3.1416 0)) e3a))

(vector3-y e2a)

(defun-mem pos-quat (q1 q2)
     (.* (from-axis e1a q1) (make-instance 'vector3ad :x 1. :y .7 :z '.6)))


(defun-mem pos-quat2 (q1 q2)
  (.+ 
   (.* (from-axis e1a q1) (make-instance 'vector3ad :x 1 :y .6 :z .7))
   (.* e1a q2)
   ))

(defun pos-quat3 (q1 q2)
    (.+ 
     (.* e1a q1)
     (.* e2a q2)
    ))

(type-of q1a)

(conj q1a)

(.* q1a q1a)

(.* q1a e1a)

(type-of q1a)
(type-of (from-axis e1a '(1 2)))

(.* (from-axis e1a '(1 2)) e1a)

(vector3-x (d-var q1 pos-quat2 ((q1 3.1416) (q2 0.))))
(vector3-y (d-var q1 pos-quat2 ((q1 3.1416) (q2 0.))))
(vector3-z (d-var q1 pos-quat2 ((q1 3.1416) (q2 0.))))


(macroexpand-1 '(d-var q2 pos-quat2 ((q1 0.) (q2 0.3) )))


(vector3-x (pos-quat2 '(0. 0.) '(0.3 1.0)))


(vector3-x (d-var q2 pos-quat2 ((q1 0.) (q2 0.3) )))
(vector3-y (d-var q2 pos-quat2 ((q1 0.) (q2 0.3) )))
(vector3-z (d-var q2 pos-quat2 ((q1 0.) (q2 0.3) )))
