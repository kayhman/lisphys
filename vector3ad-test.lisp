(in-package #:lisphys)

(defsuite Vector3adSuite (LisPhysSuite))

(setq e1a (make-instance 'vector3ad :x '(1 0) :y '(0 0) :z '(0 0)) )
(print e1a)
(setq e2a (make-instance 'vector3ad :x '(0 0) :y '(1 0) :z '(0 0)) )
(print e1a)
(setq e3a (make-instance 'vector3ad :x '(0 0) :y '(0 0) :z '(1 0)) )

(print e1a)

(defun test-scal-der (q)
  (.* (.* e1a 0.666) q))

;(vector3-x (.+ e1a e2a))
;(.- e1a e2a)
;(vector3-x (.- e1a e2a))
;(vector3-x (cross e1a e2a))

;(vector3-x (.- e2a e1a))
;(cross e1a e2a)

;(class-of (cross e1a e2a))

;(vector3-x (normalize e2a))



(deftest test-vector3ad (Vector3adSuite)
  (assert-equal 0.666 (der (vector3-x (d-var q test-scal-der ((q 3.1))))))
  (assert-equal 1.0 (val (norm (cross e1a e2a))))
  (assert-equal 0.0 (val (norm (.- e1a e1a))))
  )

(run-suite 'Vector3adSuite)
