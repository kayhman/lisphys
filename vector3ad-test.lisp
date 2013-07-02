(in-package #:lisphys)

(defsuite Vector3adSuite (LisPhysSuite))

(deftest test-vector3ad (Vector3adSuite)
  (let ((e1a (make-instance 'vector3ad :x 1.0)))
   (labels ((test-scal-der (q)
	      (.* (.* e1a 0.666) q)))
    (assert-equal 0.666 (der (vector3-x (d-var q test-scal-der ((q 3.1))))))))
  (let ((e1a (make-instance 'vector3ad :x 1.0))
	(e2a (make-instance 'vector3ad :y 1.0)))
    (assert-equal 1.0 (val (norm (cross e1a e2a)))))
  (let ((e1a (make-instance 'vector3ad :x 1.0)))
    (assert-true (is-null (.- e1a e1a))))
  )
  

;(setf clunit:*clunit-report-format* :tap)
(run-suite 'Vector3adSuite)


(setq e1a (make-instance 'vector3ad :x '(1 0) :y '(0 0) :z '(0 0)) )
(setq e2a (make-instance 'vector3ad :x '(0 0) :y '(1 0) :z '(0 0)) )
(setq e3a (make-instance 'vector3ad :x '(0 0) :y '(0 0) :z '(1 0)) )
