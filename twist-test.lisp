(in-package #:lisphys)

(defsuite TwistadSuite (LisPhysSuite))

(deftest test-twist (TwistadSuite)
  (let* ((pos #v(1 0 0))
	(axis #v(0 0 1))
	(h1_0 (make-instance 'displacement
			     :pos pos
			     :rot (make-instance 'quaternion)))
	(h2_1 (make-instance 'displacement
			     :pos pos
			     :rot (make-instance 'quaternion)))
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
			     :pos #v(1 0 0)
			     :rot #q(0 0 0 1)))
	(H_2_1 (make-instance 'displacement
			     :pos pos
			     :rot #q(0 0 0 1)))
	(tw (make-instance 'twist 
		 :ang axis
		 :lin (cross pos axis)))
	 (a (* 2.0 (/ pi 2.0)))
  ) 
    (progn 
      (print (list "H_1_0 " H_1_0))
      (print (list "tw " (.exp tw 1e-6)))
      (print (.* (.exp tw 1e-6) H_1_0))
      (print (list "H_2_0" (.* (.* (.exp (.* tw a) 1e-6) H_1_0) H_2_1)))
      
      )))


(defun test-twist-ad ()
  (let* ((pos #v(1 0 0 ad))
	(axis #v(0 0 1 ad))
	(H_1_0 (make-instance 'displacement
			     :pos pos
			     :rot #q(0 0 0 1 ad)))
	(H_2_1 (make-instance 'displacement
			     :pos pos
			     :rot #q(0 0 0 1 ad)))
	(tw (make-instance 'twist 
		 :ang axis
		 :lin (cross pos axis)))
	 (a (* 2.0 (/ pi 2.0)))
  ) 
    (progn 
      (print (list "H_1_0 " H_1_0))
      (print (list "tw " (.exp tw 1e-6)))
      (print (.* (.exp tw 1e-6) H_1_0))
      (print (list "H_2_0" (.* (.* (.exp (.* tw a) 1e-6) H_1_0) H_2_1)))
      
      )))

(run-suite 'TwistadSuite)
