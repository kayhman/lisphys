(in-package #:lisphys)

;;See http://en.wikipedia.org/wiki/List_of_moment_of_inertia_tensors

(defun ball-inertia (m r)
  "Compute the inertia matrix of a ball of mass m and radius r"
  (let ((i (!! m * r * r * 2.0 / 5.0)))
    #m ((m 0 0 0 0 0)
	(0 m 0 0 0 0)
	(0 0 m 0 0 0)
	(0 0 0 i 0 0)
	(0 0 0 0 i 0)
	(0 0 0 0 0 i))))

(defun cuboid-inertia (m w h d)
  "Compute the inertia matrix of a cuboid of mass m, weight w, height h, and depth d"
  (let ((ix (!! m * ( h * h + d * d) / 12.0))
	(iy (!! m * ( w * w + d * d) / 12.0))
	(iz (!! m * ( w * w + h * h) / 12.0)))
    #m ((m 0 0 0 0 0)
	(0 m 0 0 0 0)
	(0 0 m 0 0 0)
	(0 0 0 ix 0 0)
	(0 0 0 0 iy 0)
	(0 0 0 0 0 iz))))

(defun cylinder-inertia (m r h)
  "Compute the inertia matrix of a cylinder of mass m, height h (along z), and radius r (along x or y)"
  (let ((ixy (!! m * ( 3 * r * r + h * h) / 12.0))
	(iz (!! m * r * r / 2.0)))
    #m ((m 0 0 0 0 0)
	(0 m 0 0 0 0)
	(0 0 m 0 0 0)
	(0 0 0 ixy 0 0)
	(0 0 0 0 ixy 0)
	(0 0 0 0 0 iz))))

