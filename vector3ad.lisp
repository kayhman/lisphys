(defclass vector3ad (vector3)
  ((add :initform #'ad-+ :allocation :class)
   (sub :initform #'ad-- :allocation :class)
   (mult :initform #'ad-* :allocation :class))
)

(setq e1a (make-instance 'vector3ad :x '(1 0) :y '(0 0) :z '(0 0)) )
(setq e2a (make-instance 'vector3ad :x '(0 0) :y '(1 0) :z '(0 0)) )
(setq e3a (make-instance 'vector3ad :x '(0 0) :y '(0 0) :z '(1 0)) )

(vector3-x (.+ e1a e2a))
(.- e1a e2a)
(vector3-x (.- e1a e2a))
(vector3-x (cross e1a e2a))
