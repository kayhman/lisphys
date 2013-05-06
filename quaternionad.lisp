(in-package #:lisphys)

(defclass quaternionad (quaternion math-ad)
  ((symb :initform 'quaternionad :allocation :class))
  )

(setf q1 (make-instance 'quaternionad :x '(0. 0.) :y '(1. 0.) :z '(0. 0.) :w '(0. 0.)))
