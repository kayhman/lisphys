(in-package #:lisphys)

(defsuite TwistadSuite (LisPhysSuite))

(deftest test-twist (TwistadSuite)
  (let* ((pos #v(1 0 0))
	 (axis #v(0 0 1))
	 (H_1_0 (make-instance 'displacement
			       :pos #v(1 0 0)
			       :rot #q(0 0 0 1)))
	 (H_2_1 (make-instance 'displacement
			       :pos pos
			       :rot #q(0 0 0 1)))
	 (tw (make-instance 'twist 
			    :ang axis
			    :lin (cross pos axis)))
	 (a (* 2.0 (/ pi 2.0)))
	 (H_2_0 (.* (.* (.exp (.* tw a) 1e-6) H_1_0) H_2_1))
	 ) 
    (assert-true
	(< (norm (pos H_2_0)) 1e-6))))

(deftest test-twist-ad (TwistadSuite)
  (let* ((pos #v(1 0 0 ad))
	 (axis #v(0 0 1 ad))
	 (H_1_0 (make-instance 'displacement
			       :pos #v(1 0 0 ad)
			       :rot #q(0 0 0 1 ad)))
	 (H_2_1 (make-instance 'displacement
			       :pos pos
			       :rot #q(0 0 0 1 ad)))
	 (tw (make-instance 'twist 
			    :ang axis
			    :lin (cross pos axis)))
	 (a (* 2.0 (/ pi 2.0)))
	 (H_2_0 (.* (.* (.exp (.* tw a) 1e-6) H_1_0) H_2_1))
	 ) 
    (assert-true
	(< (val (norm (pos H_2_0))) 1e-6))))

(run-suite 'TwistadSuite)
