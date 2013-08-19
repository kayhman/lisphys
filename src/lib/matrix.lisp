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
	      (make-array `(,nrows ,ncols) :initial-element 0))
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

(defmethod (setf mref) (x  (m matrix) (i integer) (j integer))
  (setf (aref (matrix-val m) i j) x))

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


(defmethod .* ((m1 matrix) (m2 matrix))
  (with-slots ((nr1 nrows) (nc1 ncols) (val1 row-major)) m1
    (with-slots ((nr2 nrows) (nc2 ncols) (val2 row-major)) m2
      (if (= nc1 nr2)
	  (let ((res (make-instance 'matrix :nrows nr1 :ncols nc2)))
	    (loop for i from 0 to (- (matrix-nrows res) 1) do
		 (loop for j from 0 to (- (matrix-ncols res) 1) do
		      (setf (mref res i j ) 
			    (reduce #'+ (map 'vector #'* (row m1 i) (col m2 j))))))
	    res)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Accessors                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod row ((m matrix)  (i integer))
  (with-slots (nrows ncols val) m
    (make-array ncols 
		:displaced-to val
		:displaced-index-offset (* i ncols)
		:element-type (array-element-type val))))

(defmethod (setf row) ((tw twist)  (m matrix) (i integer))
  "Set the value of row i element with twist value, starting with linear"
  (with-slots (angular linear) tw
    (progn
      (setf (mref m i 0) (vector3-x linear))
      (setf (mref m i 1) (vector3-y linear))
      (setf (mref m i 2) (vector3-z linear))
      (setf (mref m i 3) (vector3-x angular))
      (setf (mref m i 4) (vector3-y angular))
      (setf (mref m i 5) (vector3-z angular)))))

(defmethod col ((m matrix)  (j integer))
  (with-slots (nrows ncols val) m
    (let ((res (make-array nrows 
			   :element-type (array-element-type val))))
      (progn
	(loop for i from 0 to (- nrows 1) do
	     (setf (aref res i ) (mref m i j )))
	res
	))))

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

;; Swap two rows l and k of a mxn matrix A, which is a 2D array.
(defun swap-rows (A l k)
  (let* ((n (matrix-ncols A))
         (row (make-array n :initial-element 0)))
    (loop for j from 0 to (- n 1) do
          (setf (aref row j) (mref A l j))
          (setf (mref A l j) (mref A k j))
          (setf (mref A k j) (aref row j)))))
 
;; Creates the pivoting matrix for A.
(defun pivotize (A)
  (let* ((n (matrix-ncols A))
         (P (eye n)))
    (loop for j from 0 to (- n 1) do
          (let ((max (mref A j j))
                (row j))
            (loop for i from j to (- n 1) do
                  (if (> (mref A i j) max)
                      (setq max (mref A i j)
                            row i)))
            (if (not (= j row))
                (swap-rows P j row))))
 
  ;; Return P.
  P))
 
;; Decomposes a square matrix A by PA=LU and returns L, U and P.
(defun lu (A)
  (let* ((n (matrix-ncols A))
         (L (make-instance 'matrix :nrows n :ncols n))
         (U (make-instance 'matrix :nrows n :ncols n))
         (P (pivotize A))
         (A (.* P A)))
 
    (loop for j from 0 to (- n 1) do
	 (setf (mref L j j) 1)
          (loop for i from 0 to j do
                (setf (mref U i j)
                      (- (mref A i j)
                         (loop for k from 0 to (- i 1)
                               sum (* (mref U k j)
                                      (mref L i k))))))
          (loop for i from j to (- n 1) do
                (setf (mref L i j)
                      (/ (- (mref A i j)
                            (loop for k from 0 to (- j 1)
                                  sum (* (mref U k j)
                                         (mref L i k))))
                         (mref U j j)))))
    ;; Return L, U and P.
    (values L U P)))

(defun forward-substitution (L y b)
  (progn 
    (fill y 0)
    (let* ((n (matrix-ncols L)))
      (loop for i from 0 to (- n 1) do
	   (let ((sub-r (make-array i :displaced-to (row L i) :element-type (array-element-type y)))
		 (sub-y (make-array i :displaced-to y :element-type (array-element-type y) )))
	     (setf (aref y i) (- (aref b i) (reduce #'+ (map 'vector #'* sub-r sub-y) ))))))))

(defun backward-substitution (U x y)
  (progn
    (fill x 0)
    (let* ((n (matrix-ncols U)))
      (loop for i from (- n 1) downto 0 do
	   (let ((sub-r (make-array (- n (+ i 1)) :displaced-index-offset (+ i 1) 
				    :displaced-to (row U i) :element-type (array-element-type x)))
		 (sub-x (make-array (- n (+ i 1)) :displaced-index-offset (+ i 1)
				    :displaced-to x :element-type (array-element-type x) )))
	     (setf (aref x i) (/ (- (aref y i) (reduce #'+ (map 'vector #'* sub-r sub-x))) (mref U i i))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Helper                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matrix-map-transformer (stream subchar arg)  
  (let* ((val (read stream t))
	 (nrows (length val))
	 (ncols (length (first val))))
    `(make-instance 'matrix 
		    :nrows ,nrows
		    :ncols ,ncols
		    :val (make-array 
			   (quote (,nrows ,ncols))  
			  :initial-contents '(,@val)
			  )
		    )))

(set-dispatch-macro-character #\# #\m
			      #'matrix-map-transformer)


(defmethod print-object ((m matrix) stream)
  (with-slots (nrows ncols val) m
       (format t "~a : ~a ~a ~% ~% ~a" (type-of m) nrows ncols val)))
;test read-macro
;(matrix-ncols #m((1 2) 1. 2.))


(multiple-value-bind (L U P) (lu #m((1 3 5) (2 4 7) (1 1 0))) (setq Ll L) (setq Uu U) (setq Pp P))
(setq y (make-array 3 :initial-element 0. ))
(setq x (make-array 3 :initial-element 0. ))
(setq b #(1 2 3))

(forward-substitution Ll y b)
(backward-substitution Uu x y)

;;check : (.* P (.* m1 Xm)) == b

(loop for i from 12 to 0 do (print i))

(defun solve (P L U x b)
  nil)
