(in-package #:lisphys)

(defclass matrix (math)
  ((val :accessor matrix-val :initarg :val)
   (nrows :accessor matrix-nrows :initarg :nrows :initform 0)
   (ncols :accessor matrix-ncols :initarg :ncols :initform 0)
   (row-major :accessor matrix-row-major)))

(defmethod initialize-instance :after ((m matrix) &key nrows ncols val)
  (if (not val)
      (progn
	(setf (matrix-val m) 
	      (make-array `(,nrows ,ncols)))
	(setf (matrix-row-major m) 
	      (make-array (array-total-size (matrix-val m))
			  :displaced-to (matrix-val m) 
			  :element-type (array-element-type (matrix-val m))))
	)
      (progn 
	(setf (matrix-val m) val)
	(setf (matrix-row-major m) 
	       (make-array (array-total-size val)
			   :displaced-to val 
			   :element-type (array-element-type val))))))

(defmethod mref ((m matrix) (i integer) (j integer))
  (with-slots (nrows ncols val) m
    (if (and (< i nrows) (< j ncols))
	(aref val i j ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Arithmetic operation                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod .+ ((m1 matrix) (m2 matrix))
  (with-slots ((nr1 nrows) (nc1 ncols) (val1 row-major)) m1
    (with-slots ((nr2 nrows) (nc2 ncols) (val2 row-major)) m2
      (if (and (= nr1 nr2) (= nr1 nr2))
	  (let ((val (map 'vector #'+ val1 val2)))
	    (make-instance 'matrix :nrows nr1 :ncols nc1 
			   :val (make-array `(,nr1 ,nc1) 
					    :displaced-to val 
					    :element-type (array-element-type val1 ))))))))

(defmethod .- ((m1 matrix) (m2 matrix))
  (with-slots ((nr1 nrows) (nc1 ncols) (val1 row-major)) m1
    (with-slots ((nr2 nrows) (nc2 ncols) (val2 row-major)) m2
      (if (and (= nr1 nr2) (= nr1 nr2))
	  (let ((val (map 'vector #'- val1 val2)))
	    (make-instance 'matrix :nrows nr1 :ncols nc1 
			   :val (make-array `(,nr1 ,nc1) 
					    :displaced-to val 
					    :element-type (array-element-type val1 ))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Accessors                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod row ((m matrix)  (i integer))
  (with-slots (nrows ncols val) m
    (make-array ncols 
		:displaced-to val
		:displaced-index-offset (* i ncols)
		:element-type (array-element-type val))))

;See http://rosettacode.org/wiki/LU_decomposition for initial lu code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Linear Algebra functions                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod eye ((n integer))
  (let ((I (make-instance 'matrix :nrows n :ncols n)))
    (with-slots (val) I
	(loop for j from 0 to (- n 1) do
	     (setf (aref val j j ) 1.0)))
    I))

(setq m1 (make-instance 'matrix :nrows 2 :ncols 3))
(setq m2 (make-instance 'matrix :nrows 2 :ncols 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Helper                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matrix-map-transformer (stream subchar arg)  
  (let* ((sexp (read stream t))
	 (dim (car sexp))
	 (val (cdr sexp)))
    `(make-instance 'matrix 
		    :nrows (first ',dim)
		    :ncols (second ',dim)
		    :val (make-array 
			  (list ,(first dim) , (second dim))  
			  :initial-contents (quote ,@val)
			  )
		    )))

(set-dispatch-macro-character #\# #\m
			      #'matrix-map-transformer)


(defmethod print-object ((m matrix) stream)
  (with-slots (nrows ncols val) m
       (format t "~a : ~a ~a ~% ~% ~a" (type-of m) nrows ncols val)))
;test read-macro
;(matrix-ncols #m((1 2) 1. 2.))
