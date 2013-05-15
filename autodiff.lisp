(in-package #:lisphys)

(defun val (x)
(first x)
)

(defun der (x)
(second x)
)

(defmacro getname (x)
`(values (intern (concatenate 'string "AD-" (string-upcase ,x))))
)


(defmacro adify-function (name deriv)
  `(defun ,(getname name) (x)
     (list (,name (first x)) ,deriv)
     )
  )

(adify-function cos (* (der x) (- (sin (val x)) )))

(adify-function sin (* (der x) (cos (val x)) ))

(adify-function sqrt (/ (der x) (* 2.0 (sqrt (val x))) ))

(macroexpand-1 '(adify-function cos (* (second x) (sin (first x)) )))

(defun ad-+ (&rest x)
  (list 
   (reduce #'+ (mapcar #'val x) )
   (reduce #'+ (mapcar #'der x) )
   )
  )

(defun ad-- (&rest x)
  (list 
   (reduce #'- (mapcar #'val x) )
   (reduce #'- (mapcar #'der x) )
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

;;(defun ad-sin (x)
;;(list 
;;(sin (first x))  ( * (cos (first x))  (second x))
;;)
;;)

;;(defun ad-cos (x)
;;(list 
;;(cos (first x))  (- ( * (sin (first x))  (second x)))
;;)
;;)

(defun adify-atom (x)
(case x
  (+ 'ad-+)
  (* 'ad-*)
  (cos 'ad-cos)
  (sin 'ad-sin)
  (sqrt 'ad-sqrt)
  (otherwise `,x)
  )
)

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
  (mapcar #'rec expr))
)

(defmacro adify (name var-list (&body exp))
  `(defun ,name ,var-list
      ,(adify-exp exp)
     )
  )


(defun create-phase (x)
  (list (first x) `'(,(second x) 0)))

(defmacro d-first (fn bindings)
  `(let ,(append (list `(,(caar bindings) '(,(second (car bindings)) 1))) (mapcar #'create-phase (cdr bindings)))
    (,fn ,@(mapcar #'first bindings))))

(adify p (x y z) (* x (* z y)))

(p '(3 1) '(2 0) '(1 0)) ;; d p / dx | x == 3

;;(dx p ((x 3) (y 2) (z 1)))


(macroexpand-1 '(d-first p ((x 3) (y 2) (z 1))))

(macroexpand-1 '(dx p ((y 3) (x 2) (z 1) (w 3))))

(d-first p ((x 3) (y 2) (z 1)))

(d-first p ((y 2) (x 3) (z 1)))
