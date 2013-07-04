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

(deftest test-vector3ad-setf (Vector3adSuite)
    (let ((v1 (make-instance 'vector3ad :x 1.0 :y 2.0 :z 3.0))) 
    (progn 
      (assert-equal '(1.0 0.0) (vector3-x v1))
      (assert-equal '(2.0 0.0) (vector3-y v1))
      (assert-equal '(3.0 0.0) (vector3-z v1))
      (setf (vector3-x v1) 11.0)
      (setf (vector3-y v1) 12.0)
      (setf (vector3-z v1) 13.0)
      (assert-equal '(11.0 0.0) (vector3-x v1))
      (assert-equal '(12.0 0.0) (vector3-y v1))
      (assert-equal '(13.0 0.0) (vector3-z v1))
      (setf (vector3-x v1) '(11.0 1.0))
      (setf (vector3-y v1) '(12.0 1.0))
      (setf (vector3-z v1) '(13.0 1.0))
      (assert-equal '(11.0 1.0) (vector3-x v1))
      (assert-equal '(12.0 1.0) (vector3-y v1))
      (assert-equal '(13.0 1.0) (vector3-z v1))
      )
    ))

  

;(setf clunit:*clunit-report-format* :tap)
(run-suite 'Vector3adSuite)


(setq e1a (make-instance 'vector3ad :x '(1 0) :y '(0 0) :z '(0 0)) )
(setq e2a (make-instance 'vector3ad :x '(0 0) :y '(1 0) :z '(0 0)) )
(setq e3a (make-instance 'vector3ad :x '(0 0) :y '(0 0) :z '(1 0)) )
