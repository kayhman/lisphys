(in-package #:lisphys)

(defmacro make-hinge (hinge-name axis center)
  `(defun ,hinge-name (q)
     (let ((tw (make-instance 'twist :lin (cross ,center ,axis) :ang ,axis)))
      (.exp (.* tw q)))))

(defun root-position ()
  #d((0 0 0 ad)
     (0 0 0 1 ad)))


(defmacro add-rigid-body (prev-rb joint H_j_i mass name)
   `(progn
      (defun ,(symbolicate (string-upcase name) "-POSITION") (&rest qs)
	(.* (.* (apply #',(symbolicate (string-upcase prev-rb) "-POSITION") (butlast qs)) (apply #',joint (last qs))) ,H_j_i))
      (defun ,(symbolicate (string-upcase name) "-MASS") ()
      (ball-inertia ,mass 0.1))))

;; Time Integration 

