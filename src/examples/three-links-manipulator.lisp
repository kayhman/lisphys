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

(defvar **gravity** #m((0) (0) (1) (0) (0) (0)) )

(setf m1 (ball-inertia 1.0 0.3))
(setq m2 (ball-inertia 1.0 0.3))
(setq m3 (ball-inertia 1.0 0.3))

(let ((x1 0.)
      (x2 0.)
      (x3 0.)
      (v1 0.)
      (v2 0.)
      (v3 0.)
      (ti 0.0)
      Jt1 Jt2 Jt3 mg1 mg2 mg3 mg rhs)
  (defun integrate (dt)
    (progn
      (setf Jt1 (jacobian-transpose g_l1_s ((q1 x1) (q2 x2) (q3 x3))))
      (setf Jt2 (jacobian-transpose g_l2_s ((q1 x1) (q2 x2) (q3 x3))))
      (setf Jt3 (jacobian-transpose g_l3_s ((q1 x1) (q2 x2) (q3 x3))))
    
      (setf mg1 (.* (val Jt1) (.* m1 (val (transpose Jt1)))))
      (setf mg2 (.* (val Jt2) (.* m1 (val (transpose Jt2)))))
      (setf mg3 (.* (val Jt3) (.* m1 (val (transpose Jt3)))))
    
      (setf mg (.+ (.+ mg1 mg2) mg3))
      (setf rhs (.+ (.+ (.* (val Jt1) **gravity**) 
			(.* (val Jt2) **gravity**))
		    (.* (val Jt3) **gravity**)))
      (multiple-value-bind  (L U P) (lu (val mg))
	(let ((x #m((0) (0) (0))))
	  (progn
	    (with-open-file (stream "traj.txt" :direction :output :if-exists :append)
	      (format stream "gravity : ~a ~a ~a ~%" (mref rhs 0 0 ) (mref rhs 1 0 ) (mref rhs 2 0 )))
	    (solve P L U x rhs)
	    (setf v1 (+ v1 (* dt (mref x 0 0)))) 
	    (setf v2 (+ v2 (* dt (mref x 1 0))))
	    (setf v3 (+ v3 (* dt (mref x 2 0))))
	    (setf x1 (+ x1 (* dt v1))) 
	    (setf x2 (+ x2 (* dt v2))) 
	    (setf x3 (+ x3 (* dt v3)))
	    (setf ti (+ ti dt))
	    )))
      (with-open-file (stream "traj.txt" :direction :output :if-exists :append)
	(format stream "~a ~a ~a ~a ~%" ti x1 x2 x3)
	(values x1 x2 x3)))))

(defun draw-cube (x y z)
  (gl:push-matrix)
  (gl:translate x y r)
  (draw-figure +cube-vertices+ +cube-faces+)
  (gl:pop-matrix))

(defun draw-frame (a b c)
	    (multiple-value-bind (x1 x2 x3) (integrate 1e-3) 
	      (gl:matrix-mode :modelview)
	      
	      (print (list "xs -> " (pos (g_l1_s x1 x2 x3)) (pos (g_l2_s x1 x2 x3)) (pos (g_l3_s x1 x2 x3))))))

(loop for i from 0 to 1000 do (integrate 1e-3))

;; Time Integration 

