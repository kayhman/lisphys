(in-package #:lisphys)


(defsuite MatrixSuite (LisPhysSuite))

(deftest test-matrix (MatrixSuite)
  (let ((m1 #m((1 2) (3 4) (5 6)))
	(m2 #m((1 2) (3 4) (5 6)))
	(m3 #m((2 4) (6 8) (10 12)))
	(m4 #m ((0 0) (0 0) (0 0))))
    (assert-equal t (equalp  (matrix-val m3) (matrix-val (.+ m1 m2))))
    (assert-equal t (equalp  (matrix-val m4) (matrix-val (.- m1 m2))))
    ))

(run-suite 'MatrixSuite)
