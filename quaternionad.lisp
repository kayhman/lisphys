(in-package #:lisphys)

(defclass quaternionad (math-ad quaternion)
  ((symb :initform 'quaternionad :allocation :class))
  )

(setf q1a (make-instance 'quaternionad :x '(0. 0.) :y '(1. 0.) :z '(0. 0.) :w '(0. 0.)))


(quat-x (from-axis e1a '(3.1416 0)))
(quat-y (from-axis e1a '(3.1416 0)))
(quat-z (from-axis e1a '(3.1416 0)))
(quat-w (from-axis e1a '(3.1416 0)))

(defun pos-quat (q1)
    (.* (from-axis e1a q1) e1a))

(type-of q1a)

(conj q1a)

(.* q1a q1a)

(.* q1a e1a)

(type-of q1a)
(type-of (from-axis e1a '(1 2)))

(.* (from-axis e1a '(1 2)) e1a)

(vector3-x (d-first pos-quat ((q1 1.507))))
(vector3-y (d-first pos-quat ((q1 1.507))))
(vector3-z (d-first pos-quat ((q1 1.507))))
