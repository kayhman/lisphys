(in-package #:lisphys)

(defun val (x)
(cond
  ((numberp x) x)
  (t (first x))))

(defun der (x)
(cond 
  ((numberp x) 0.)
  (t (second x))))

(defmacro adify-function (name deriv)
  `(defun ,(symbolicate "AD-" name) (x)
     (list (,name (first x)) ,deriv)
     )
  )

(adify-function cos (* (der x) (- (sin (val x)) )))

(adify-function sin (* (der x) (cos (val x)) ))

(adify-function sqrt (/ (der x) (* 2.0 (sqrt (val x))) ))

(defun ad-+ (&rest x)
  (list 
   (reduce #'+ (mapcar #'val x) )
   (reduce #'+ (mapcar #'der x) )
   )
  )

(defun ad-- (&rest x)
  (list 
   (reduce #'- (append '(0) (mapcar #'val x) ))
   (reduce #'- (append '(0) (mapcar #'der x) ))
   )
  )

(defun ad-* (x y)
(list 
(* (val x) (val y))  (+ (* (val x) (der y)) (* (val y) (der x)))
)
)

(defun ad-/ (x y)
  (let ((y2 (* (first y) (first y))))
    (list 
     (/ (val x) (val y))
     (/ (- (* (der x) (val y)) (* (val x) (der y))) y2) )))

;;Add atom to *list* to avoid to have to add atom to the list below :
(defun adify-atom (x)
  (case x
    (+ 'ad-+)
    (* 'ad-*)
    (cos 'ad-cos)
    (sin 'ad-sin)
    (sqrt 'ad-sqrt)
    (otherwise `,x)))

(defun adify-exp (expr)
(labels ((rec (x) 
	   (if (atom x)
	       (adify-atom x)
	       (if (null (cdr x))
		   (adify-atom (car x))
		   (mapcar #'rec x)
		   )
	       )
	   ))
  (mapcar #'rec expr)))

(defmacro adify (name var-list (&body exp))
  `(defun ,name ,var-list
      ,(adify-exp exp)))

(defun create-phase (x)
  (list (first x) `'(,(second x) 0.)))

(defmacro d-var (var fn bindings)
  `(let ,(mapcar #'(lambda (x) 
		  (if (string= var (first x))
		      (list (first x) `'(,(second x) 1.0))
		      (create-phase x)))
		  bindings
		  )
    (,fn ,@(mapcar #'first bindings))))
