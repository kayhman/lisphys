(in-package #:lisphys)

(setq axis (normalize #v ((random 100) (random 100) (random 100)
		ad)))

(setq axis (normalize #v (1 0 0 ad)))

(setq tw (make-instance 'twist :lin axis :ang #v (0 0 1 ad)))

(defun trans (q1)
  (.exp (.* tw q1) ))


(setq q1_0 (* (/ (random 180) 180) pi ))
(setq q1_0 0.0)

(setq dtrans (der (d-var q1 trans ((q1 q1_0)))))

(.* (rot (.* (inv (trans q1_0)) dtrans)) 2.0)
(pos (.* (inv (trans q1_0)) dtrans))
