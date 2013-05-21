(in-package #:lisphys)

(defclass quaternionad (math-ad quaternion)
  ((symb :initform 'quaternionad :allocation :class))
  )

(setf q1a (make-instance 'quaternionad :x '(0. 0.) :y '(1. 0.) :z '(0. 0.) :w '(0. 0.)))


(quat-x (from-axis e1a '(3.1416 0)))
(quat-y (from-axis e1a '(3.1416 0)))
(quat-z (from-axis e1a '(3.1416 0)))
(quat-w (from-axis e1a '(3.1416 0)))


(vector3-x (.* (from-axis e1a '(3.1416 0)) e3a))
(vector3-y (.* (from-axis e1a '(3.1416 0)) e3a))
(vector3-z (.* (from-axis e1a '(3.1416 0)) e3a))

(vector3-y e2a)

(defun pos-quat (q1 q2)
     (.* (from-axis e1a q1) (make-instance 'vector3ad :x '(1 0) :y '(.7 0) :z '(.6 0)))
    )


(defun pos-quat2 (q1 q2)
    (.+ 
     (.* (from-axis e1a q1) (make-instance 'vector3ad :x '(1 0) :y '(.6 0) :z '(.7 0)))
     (.* e1a q2)
    ))

(type-of q1a)

(conj q1a)

(.* q1a q1a)

(.* q1a e1a)

(type-of q1a)
(type-of (from-axis e1a '(1 2)))

(.* (from-axis e1a '(1 2)) e1a)

(vector3-x (d-first pos-quat ((q1 0.) (q2 0.))))
(vector3-y (d-first pos-quat ((q1 0.) (q2 0.))))
(vector3-z (d-first pos-quat2 ((q1 0.) (q2 0.))))
