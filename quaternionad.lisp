(in-package #:lisphys)

(defclass quaternionad (math-ad quaternion)
  ())

(defmethod initialize-instance :after ( (q quaternionad) &key x y z w)
	   (progn 
	     (if (numberp x) (setf (quat-x q ) `(,x 0.)))
	     (if (numberp y) (setf (quat-y q ) `(,y 0.)))
	     (if (numberp z) (setf (quat-z q ) `(,z 0.)))
	     (if (numberp w) (setf (quat-w q ) `(,w 0.)))))

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
