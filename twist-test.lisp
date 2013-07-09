(in-package #:lisphys)

(defsuite TwistadSuite (LisPhysSuite))

(deftest test-quaternion3ad-setf (TwistadSuite)
  (let ((h1_0 (make-instance 'displacement
			     :pos (make-instance 'vector3 :x 1.0)
			     :rot (make_instance 'quaternion)))
	(h2_1 (make-instance 'displacement
			     :pos (make-instance 'vector3 :x 1.0)
			     :rot (make_instance 'quaternion)))
	(tw (make_instance 'twist 
			   :ang (make-instance 'vector3 :z 1.0)
			   :lin (make-instance 'vector3 :y 1.0)))) 
    (progn 
      (assert-equal '(1.0 0.0) (quat-x v1))
      )
    ))

(run-suite 'TwistadSuite)
