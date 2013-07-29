(in-package #:lisphys)


(defsuite LisPhysSuite ())
(defsuite AutoDiffSuite (LisPhysSuite))

(deftest test-der (AutoDiffSuite)
  (progn
    (adify p (x y z) (* x (* z y)))
    (assert-equal '(6 2.0) (d-var x p ((x 3) (y 2) (z 1)))))
  )


(run-suite 'LisPhysSuite)
