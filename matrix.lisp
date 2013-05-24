(defclass matrix (math)
  ((val :accessor matrix-val :initarg :val)
   (nrows :accessor matrix-nrows :initarg :nrows :initform 0)
   (ncols :accessor matrix-ncols :initarg :ncols :initform 0))
  )

(defmethod initialize-instance :after ((m matrix) &key)
  (setf (matrix-val m) 
	(make-array (* (matrix-nrows m) (matrix-ncols m)))
	 ))

(defmethod melt ((m matrix) (i integer) (j integer))
  (with-slots (nrows ncols val) m
    (if (and (< i nrows) (< j ncols))
	(elt val
	     (+ j (* i (matrix-ncols m)))
	    ))))
