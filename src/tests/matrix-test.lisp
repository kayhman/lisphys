(in-package #:lisphys)


(defsuite MatrixSuite (LisPhysSuite))

(deftest test-matrix-arithemtic (MatrixSuite)
  (let ((m1 #m((1 2) (3 4) (5 6)))
	(m2 #m((1 2) (3 4) (5 6)))
	(m3 #m((2 4) (6 8) (10 12)))
	(m4 #m ((0 0) (0 0) (0 0)))
	(m5 #m((1 2 3) (4 5 6)))
	(m6 #m((1 2) (3 4) (5 6)))
	(m7 #m((22 28) (49 64)))
	)
    (assert-equal t (equalp  (matrix-val m3) (matrix-val (.+ m1 m2))))
    (assert-equal t (equalp  (matrix-val m4) (matrix-val (.- m1 m2))))
    (assert-equal t (equalp  (matrix-val m7) (matrix-val (.* m5 m6))))
    ))

(deftest test-lu (MatrixSuite)
  (let ((m1 #m((1 3 5) (2 4 7) (1 1 0))))
    (multiple-value-bind  (L U P) (lu m1)
      (progn
	(assert-equal t (equalp  (matrix-val #m ((1.0 0 0) 
				      (0.5 1.0 0)
				      (0.5 -1.0 1.0)
				      )) (matrix-val L)))
	(assert-equal t (equalp  (matrix-val #m ((2.0 4.0 7.0) 
				      (0 1.0 1.5)
				      (0 0 -2.0)
				      )) (matrix-val U)))
	(assert-equal t (equalp  (matrix-val #m ((0 1.0 0) 
				   (1. 0 0)
				   (0 -0 1.0)
				   )) (matrix-val P)))))))
  

(run-suite 'MatrixSuite)
