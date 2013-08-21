(in-package #:lisphys)

;;Murray Ly Sastry p.172

(setq l0 1.5)
(setq l1 1.5)
(setq l2 1.5)

(setq r0 (/ l0 2))
(setq r1 (/ l1 2))
(setq r2 (/ l2 2))


(setq tw1 #t((0 0 0 ad) 
	     (0 0 1 ad)))

(setq tw2 #t((0 (- l0) 0 ad) 
	     ((- 1) 0 0 ad)))

(setq tw3 #t(( 0 (- l0) l1 ad) 
	     ((- 1) 0 0 ad)))

(setq g_l1_s_0 #d ((0 0 r0 ad)
		(0 0 0 1 ad)))

(setq g_l2_s_0 #d ((0 r1 l0 ad)
		(0 0 0 1 ad)))

(setq g_l3_s_0 #d ((0 (+ l1 r2) l0 ad)
		(0 0 0 1 ad)))

(defun g_l1_s (q1 q2 q3)
  (.* (.exp (.* tw1 q1)) g_l1_s_0 ))

(defun g_l2_s (q1 q2 q3)
  (.* (.exp (.* tw1 q1)) (.* (.exp (.* tw2 q2)) g_l2_s_0)))

(defun g_l3_s (q1 q2 q3)
  (.* (.exp (.* tw1 q1)) (.* (.* (.exp (.* tw2 q2)) (.exp (.* tw3 q3))) g_l3_s_0)))


(jacobian-transpose g_l1_s ((q1 0) (q2 0) (q3 0)))
(jacobian-transpose g_l2_s ((q1 0) (q2 0) (q3 0)))
(jacobian-transpose g_l3_s ((q1 0) (q2 0) (q3 0)))
