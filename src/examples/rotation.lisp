(in-package #:lisphys)

(setq axis (normalize #v ((random 100) (random 100) (random 100)
		ad)))

(defun rotate (q1)
  (from-axis axis q1))


(setq vel #q ((vector3-x axis)	 ;;pure quaternion
	      (vector3-y axis) 
	      (vector3-z axis)
	      0))

(setq q1_0 (* (/ (random 180) 180) pi ))

(setq drot# (.* (.* (rotate q1_0) vel) 0.5)) ;; dot(q) = q * v

(setq drot (der (d-var q1 rotate ((q1 q1_0))))) ;; compute dot(q) using add

(d-var q1 rotate ((q1 0.7853981)))

(.* (.* (conj (rotate q1_0)) drot) 2.0);; check that q-1 * dot(q) = vrl
