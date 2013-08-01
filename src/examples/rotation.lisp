(in-package #:lisphys)

(defun rot (q1)
  (from-axis #v (0 0 1 ad) q1)
) 

(setq vel #q (0 0 1 0)) ;;pure quaternion

(setq q1_0 (/ pi 4.0))

(setq drot# (.* (.* (rot q1_0) vel) 0.5)) ;; dot(q) = q * v

(setq drot (der (d-var q1 rot ((q1 q1_0))))) ;; compute dot(q) using add

(d-var q1 rot ((q1 0.7853981)))

(.* (.* (conj (rot q1_0)) drot) 2.0);; check that q-1 * dot(q) = vrl
