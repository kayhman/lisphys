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

(defmethod inv ((d displacement))
  (with-slots (pos rot) d
   (make-instance 'displacement
		  :pos (.* (conj rot) (.* pos -1.0))
		  :rot (conj rot))))

;; MLS P.54
(defmethod .*-1 ((d-der displacement) (d displacement))
"Multiply displacement derivative d-der by displacement d. Used to compute unit twist."
  (with-slots ((drot rot) pos) d
    (with-slots ((der-rot rot) (der-pos pos)) d-der
	(let* ((quat-pure #q ((der (quat-x der-rot))
			      (der (quat-y der-rot))
			      (der (quat-z der-rot))
			      '(0. 0.)
			      ad))
	       (ang-vel (.* (.* quat-pure (conj drot)) 2.0))
	       (ang-vel-vec #v ((quat-x ang-vel)
				(quat-y ang-vel)
				(quat-z ang-vel)
				ad
				)))
	  (make-instance 'twist 
			 :lin (der der-pos)
			 :ang ang-vel-vec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Helper                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-dispatch-macro-character #\# #\d
			      #'(lambda (stream subchar arg)
				  (let* ((sexp (read stream t))
					 (pos (first sexp))
					 (rot (second sexp))
					 (typ (case (fourth pos)
					       ('ad 'vector3ad)
					       (otherwise 'vector3 )))
					 (tyr (case (fifth rot)
					       ('ad 'quaternionad)
					       (otherwise 'quaternion ))))
				    `(make-instance 'displacement
						    :pos (make-instance ',typ :x ,(first pos) :y ,(second pos) :z ,(third pos))
						    :rot (make-instance ',tyr :x ,(first rot) :y ,(second rot) :z ,(third rot) :w ,(fourth rot)
									)))))


(defmethod print-object ((h displacement) stream)
  (with-slots (pos rot) h
       (format t "~a : ~a ~a ~%" (type-of h) pos rot)))
