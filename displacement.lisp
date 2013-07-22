(in-package #:lisphys)

(defclass displacement (math)
  ((pos :accessor pos :initarg :pos 
	:initform (make-instance 'vector3))
   (rot :accessor rot :initarg :rot 
	:initform (make-instance 'quaternion))))

(defmethod .* ((h1 displacement) (h2 displacement))
  (with-slots ((pos1 pos) (rot1 rot)) h1
    (with-slots ((pos2 pos) (rot2 rot)) h2
      (make-instance 'displacement
		     :pos (.+ (.* rot1 pos2) pos1)
		     :rot (.* rot1 rot2)))))


(defmethod print-object ((h displacement) stream)
  (with-slots (pos rot) h
       (format t "~a : ~a ~a ~%" (type-of h) pos rot)))
