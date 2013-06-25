(in-package #:lisphys)

(defsuite Vector3Suite (LisPhysSuite))

(setq e1 (make-instance 'vector3 :x 1.0 :y 0. :z 0. ))
(setq e2 (make-instance 'vector3 :x 0.0 :y 1. :z 0. ))
(setq e3 (make-instance 'vector3 :x 0.0 :y 0. :z 1. ))

(deftest test-vector3 (Vector3Suite)
  (assert-equal 0.0 (norm (.- e3 (cross e1 e2))))
  (assert-equal t (is-null (.- e3 e3))))

(run-suite 'Vector3Suite)


