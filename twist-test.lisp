(in-package #:lisphys)

(defsuite TwistadSuite (LisPhysSuite))

(deftest test-twist (TwistadSuite)
  (let* ((pos #v(1 0 0))
	(axis #v(0 0 1))
	(h1_0 (make-instance 'displacement
			     :pos pos
			     :rot (make_instance 'quaternion)))
	(h2_1 (make-instance 'displacement
			     :pos pos
			     :rot (make_instance 'quaternion)))
	(tw (make-instance 'twist 
			   :ang axis
			   :lin (cross pos axis)))) 
    (progn 
      (print (cross pos axis))
      ; (assert-equal '(1.0 0.0) (quat-x v1))
      )))


(defun test-twist ()
  (let* ((pos #v(1 0 0))
	(axis #v(0 0 1))
	(H_1_0 (make-instance 'displacement
			     :pos pos
			     :rot #q(0 0 0 1)))
	(H_2_1 (make-instance 'displacement
			     :pos pos
			     :rot #q(0 0 0 1)))
	(tw (make-instance 'twist 
			   :ang axis
			   :lin (cross pos axis)))
	 (a (* 1.0 (/ pi 2.0)))) 
    (progn 
      (print tw)
      ;      (print (.* (rot H_1_0) (pos H_2_1)))
      ;      (print (.* H_1_0 H_2_1) )
      ;      (print (.exp tw 1e-6))
      ;      (print (.* (.exp tw 1e-6) H_2_1) )
      ;      (print (.* (.exp (.* tw a) 1e-6) H_1_0))
      (print a)
            (print (.* (.* (.exp (.* tw a) 1e-6) H_1_0) H_2_1))
      
      )))

(run-suite 'TwistadSuite)
