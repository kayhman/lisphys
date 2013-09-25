(in-package #:lisphys)

(defclass matrix (math)
  ((val :accessor matrix-val :initarg :val)
   (nrows :accessor matrix-nrows :initarg :nrows :initform 0)
   (ncols :accessor matrix-ncols :initarg :ncols :initform 0)
   (row-major :accessor matrix-row-major :initarg :row-major )))

(defmethod initialize-instance :after ((m matrix) &key nrows ncols val row-major)
  (with-ad m 
    (cond 
      ((and (not row-major) (not val)) 
       (progn
	 (setf (matrix-val m) 
	       (make-array `(,nrows ,ncols) :initial-element !zero))
	 (setf (matrix-row-major m) 
	       (make-array (array-total-size (matrix-val m))
			   :displaced-to (matrix-val m) 
			   :element-type (array-element-type (matrix-val m))))
	 ))
      (val 
       (progn 
	 (setf (matrix-val m) val)
	 (setf (matrix-row-major m) 
	       (make-array (array-total-size val)
			   :displaced-to val 
			   :element-type (array-element-type val)))))
      (row-major 
       (progn 
	 (setf (matrix-val m) 
	       (make-array `(,nrows ,ncols)
			   :displaced-to row-major 
			   :element-type (array-element-type row-major)))
	 (setf (matrix-row-major m) row-major))))))



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
	  (with-ad m1 
	    (let ((val (map 'vector #'!+ val1 val2)))
	      (make-instance 'matrix :nrows nr1 :ncols nc1 
			     :val (make-array `(,nr1 ,nc1) 
					      :displaced-to val 
					      :element-type (array-element-type val1 )))))))))

(defmethod .- ((m1 matrix) (m2 matrix))
  (with-slots ((nr1 nrows) (nc1 ncols) (val1 row-major)) m1
    (with-slots ((nr2 nrows) (nc2 ncols) (val2 row-major)) m2
      (if (and (= nr1 nr2) (= nr1 nr2))
	  (with-ad m1 
	   (let ((val (map 'vector #'!- val1 val2)))
	     (make-instance 'matrix :nrows nr1 :ncols nc1 
			    :val (make-array `(,nr1 ,nc1) 
					     :displaced-to val 
					     :element-type (array-element-type val1 )))))))))


(defmethod .* ((m1 matrix) (m2 matrix))
  (with-slots ((nr1 nrows) (nc1 ncols) (val1 row-major)) m1
    (with-slots ((nr2 nrows) (nc2 ncols) (val2 row-major)) m2
      (if (= nc1 nr2)
	  (with-ad m1
	    (let ((res (make-instance (type-of m1) :nrows nr1 :ncols nc2)))
	      (loop for i from 0 to (- (matrix-nrows res) 1) do
		   (loop for j from 0 to (- (matrix-ncols res) 1) do
			(setf (mref res i j )
			      (reduce #'!+ (map 'vector #'!* (row m1 i) (col m2 j))))))
	      res))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Accessors                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod row ((m matrix)  (i integer))
  (with-slots (nrows ncols val) m
    (make-instance 'matrix 
		   :nrows 1
		   :ncols ncols
		   :row-major (make-array ncols 
				    :displaced-to val
				    :displaced-index-offset (* i ncols)
				    :element-type (array-element-type val)))))

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
      (loop for i from 0 to (- nrows 1) do
	   (setf (aref res i ) (mref m i j )))
      (make-instance 'matrix :nrows nrows :ncols 1 :row-major res))))


;See http://rosettacode.org/wiki/LU_decomposition for initial lu code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Linear Algebra functions                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod eye ((n integer) &optional ad)
  (let* ((ty (if ad 
		 'matrixad
		 'matrix))
	 (I (make-instance `,ty :nrows n :ncols n)))
    (with-ad I
      (with-slots (val) I
	(loop for j from 0 to (- n 1) do
	     (setf (aref val j j ) !unit))))
    I))

(defmethod transpose ((m matrix))
  (with-slots (nrows ncols) m
    (let ((mt (make-instance 'matrix :nrows ncols :ncols nrows)))
      (progn
	(loop for i from 0 to (- nrows 1) do
	     (loop for j from 0 to (- ncols 1) do
		  (setf (mref mt j i) (mref m i j))))
	mt))))




;; Swap two rows l and k of a mxn matrix A, which is a 2D array.
(defun swap-rows (A l k)
  (with-ad A
    (let* ((n (matrix-ncols A))
	   (row (make-array n :initial-element !zero)))
      (loop for j from 0 to (- n 1) do
	   (setf (aref row j) (mref A l j))
	   (setf (mref A l j) (mref A k j))
	   (setf (mref A k j) (aref row j))))))

;; Creates the pivoting matrix for A.
(defun pivotize (A)
  (let* ((n (matrix-ncols A))
         (P (eye n (is-ad A))))
    (loop for j from 0 to (- n 1) do
          (let ((max (val (mref A j j)))
                (row j))
            (loop for i from j to (- n 1) do
                  (if (> (val (mref A i j)) max)
                      (setq max (val (mref A i j))
                            row i)))
            (if (not (= j row))
                (swap-rows P j row))))
 
  ;; Return P.
  P))
 
;; Decomposes a square matrix A by PA=LU and returns L, U and P.
(defun lu (A)
  (with-ad A 
    (let* ((n (matrix-ncols A))
	   (L (make-instance (type-of A) :nrows n :ncols n))
	   (U (make-instance (type-of A) :nrows n :ncols n))
	   (P (pivotize A))
	   (A (.* P A)))
      
      (loop for j from 0 to (- n 1) do
	   (setf (mref L j j) 1)
	   (loop for i from 0 to j do
		(setf (mref U i j)
		      (!- (mref A i j)
			  (reduce #'!+ (loop for k from 0 to (- i 1)
					  collect (!* (mref U k j)
						      (mref L i k)))))))
	   (loop for i from j to (- n 1) do
    		  (setf (mref L i j)
		       (!/ (!- (mref A i j)
			       (reduce #'!+ (loop for k from 0 to (- j 1)
					       collect (!* (mref U k j)
							   (mref L i k)))))
			   (mref U j j)))))
      ;; Return L, U and P.
      (values L U P))))

(defun forward-substitution (L y b)
  (with-ad L 
    (progn 
      (fill (matrix-row-major y) !zero)
      (let* ((n (matrix-ncols L)))
	(loop for i from 0 to (- n 1) do
	     (let ((sub-r (make-array i :displaced-to (row L i) :element-type (array-element-type (matrix-val y))))
		   (sub-y (make-array i :displaced-to (matrix-val y) :element-type (array-element-type (matrix-val y)) )))
	       (setf (mref y i 0) (!- (mref b i 0) (reduce #'!+ (map 'vector #'!* sub-r sub-y) )))))))))

(defun backward-substitution (U x y)
  (with-ad U 
    (progn
      (fill (matrix-row-major x) !zero)
      (let* ((n (matrix-ncols U)))
	(loop for i from (- n 1) downto 0 do
	     (let ((sub-r (make-array (- n (+ i 1)) :displaced-index-offset (+ i 1) 
				      :displaced-to (row U i) :element-type (array-element-type (matrix-val x))))
		   (sub-x (make-array (- n (+ i 1)) :displaced-index-offset (+ i 1)
				      :displaced-to (matrix-val x) :element-type (array-element-type (matrix-val x)) )))
	       (setf (mref x i 0) (!/ (!- (mref y i 0) (reduce #'!+ (map 'vector #'!* sub-r sub-x))) (mref U i i)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Autodiff                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(macrolet ((def-get (op)
	     `(defmethod ,op ((m matrix)) (with-slots (nrows ncols) m
		 (make-instance 'matrix :nrows nrows :ncols ncols 
				:row-major (map (type-of (matrix-row-major m)) #'(lambda (x) (,op x)) (matrix-row-major m))))))) 
 (progn
    (def-get val)
    (def-get der)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Helper                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matrix-map-transformer (stream subchar arg)  
  (let* ((val (read stream t))
	 (ty (case (car (last val))
	       ('ad 'matrixad)
	       (otherwise 'matrix )))
	 (nrows (length (delete 'ad val)))
	 (ncols (length (first val))))
    `(make-instance ',ty	  ;TODO : finir reading macro pour ad 
		    :nrows ,nrows
		    :ncols ,ncols
		    :val (make-array 
			  (quote (,nrows ,ncols))  
			  :initial-contents (list ,@(mapcar #'(lambda (x)
								`(list ,@x))
							    val))))))


(set-dispatch-macro-character #\# #\m
			      #'matrix-map-transformer)


(defmethod print-object ((m matrix) stream)
  (with-slots (nrows ncols val) m
       (format t "~a : ~a ~a ~% ~a ~%-- ~%" (type-of m) nrows ncols val)))

(defun solve (P L U x b)
  (let ((y (make-instance 'matrix :nrows (matrix-nrows x) :ncols (matrix-ncols x)))) 
  (progn
    (forward-substitution L y b)
    (backward-substitution U x y)
    )))
