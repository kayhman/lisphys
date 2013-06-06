(in-package #:lisphys)


(defsuite LisPhysSuite ())
(defsuite AutoDiffSuite (LisPhysSuite))

(adify p (x y z) (* x (* z y)))

  ;;

(deftest test-der (AutoDiffSuite)
  (assert-equal 1 (+ 1 0))
  (assert-equal '(6 2) (d-var x p ((x 3) (y 2) (z 1))))
  )


(run-suite 'LisPhysSuite)
