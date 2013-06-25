(in-package #:lisphys)

(defsuite Vector3adSuite (LisPhysSuite))

(defun test-scal-der (q)
  (.* (.* e1a 0.666) q))


(deftest test-vector3ad (Vector3adSuite)
  (assert-equal 0.666 (der (vector3-x (d-var q test-scal-der ((q 3.1)))))))

(run-suite 'Vector3adSuite)
