(defclass matrix (math)
  ((val :accessor matrix-val :initarg :val)
   (nrows :accessor matrix-nrows :initarg :nrows :initform 0)
   (ncols :accessor matrix-ncols :initarg :ncols :initform 0))
  )

(defmethod initialize-instance :after ((m matrix) &key)
  (if (not (matrix-val m))
	   (setf (matrix-val m) 
		 (make-array (* (matrix-nrows m) (matrix-ncols m)))
		 )))

(defmethod melt ((m matrix) (i integer) (j integer))
  (with-slots (nrows ncols val) m
    (if (and (< i nrows) (< j ncols))
	(elt val
	     (+ j (* i (matrix-ncols m)))
	    ))))

;(reduce #'+ (map 'vector #'* (vector 1 0 0) (vector 1 0 0))) <- dot product code

(defmethod .+ ((m1 matrix) (m2 matrix))
  (with-slots ((nr1 nrows) (nc1 ncols) (val1 val)) m1
    (with-slots ((nr2 nrows) (nc2 ncols) (val2 val)) m2
      (if (and (= nr1 nr2) (= nr1 nr2))
	  (let ((val (map 'vector #'+ val1 val2)))
	    (make-instance 'matrix :nrows nr1 :ncols nc1 :val val))))))

(setq m1 (make-instance 'matrix :nrows 2 :ncols 3))
(setq m2 (make-instance 'matrix :nrows 2 :ncols 3))

;; Add reader macro

(defun matrix-map-transformer (stream subchar arg)  
  (let* ((sexp (read stream t))
	 (dim (car sexp))
	 (val (cdr sexp)))
    `(make-instance 'matrix 
		    :nrows (first ',dim)
		    :ncols (second ',dim)
		    :val (vector ,@val)
		    )))

(set-dispatch-macro-character #\# #\m
			      #'matrix-map-transformer)

;test read-macro
;(matrix-ncols #m((1 2) 1. 2.))
