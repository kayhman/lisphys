(in-package #:lisphys)

(setq l0 1.5)
(setq l1 1.5)
(setq l2 1.5)

(setq tw1 #t((0 0 0 ad) 
	     (0 0 1 ad)))

(setq tw2 #t((0 (- l0) 0 ad) 
	     ((- 1) 0 0 ad)))

(setq tw3 #t(( 0 (- l0) l1 ad) 
	     ((- 1) 0 0 ad)))

(setq tw4 #t(((+ l1 l2) 0 0 ad) 
	     (0 0 1 ad)))

(setq tw5 #t((0 (- l0) (+ l1 l2) ad) 
	     ((- 1) 0 0 ad)))

(setq tw6 #t(((- l0) 0 0 ad) 
	     (0 1 0 ad)))

(setq H_T_0 #d ((0 (+ l1 l2) l0 ad)
		(0 0 0 1 ad)))


(defun g_T_0# (q1 q2 q3 q4 q5 q6)
  (.* (.exp (.* tw6 q6)) H_T_0))


() (defun g_T_0 (q1 q2 q3 q4 q5 q6)
  (.* (.exp (.* tw1 q1))
   (.* (.exp (.* tw2 q2))
       (.* (.exp (.* tw3 q3))
	   (.* (.exp (.* tw4 q4))
	       (.* (.exp (.* tw5 q5)) 
		   (.* (.exp  (.* tw6 q6)) H_T_0))))))
  )
