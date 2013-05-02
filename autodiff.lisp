(defun val (x)
(first x)
)


(defun dot (x)
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

(adify-function cos (* (dot x) (- (sin (val x)) )))

(adify-function sin (* (dot x) (cos (val x)) ))

(macroexpand-1 '(adify-function cos (* (second x) (sin (first x)) )))

(defun ad-+ (x y)
(list 
(+ (val x) (val y))  (+ (dot x) (dot y))
)
)

(defun ad-- (x y)
(list 
(- (val x) (val y))  (- (dot x) (dot y))
)
)

(defun ad-* (x y)
(list 
(* (val x) (val y))  (+ (* (val x) (dot y)) (* (val y) (dot x)))
)
)


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
  (otherwise `(list ,x 0))
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
