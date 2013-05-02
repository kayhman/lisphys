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
(+ (first x) (first y))  (+ (second x) (second y))
)
)

(defun ad-- (x y)
(list 
(- (first x) (first y))  (- (second x) (second y))
)
)

(defun ad-* (x y)
(list 
(* (first x) (first y))  (+ (* (first x) (second y)) (* (first y) (second x)))
)
)


(defun ad-sin (x)
(list 
(sin (first x))  ( * (cos (first x))  (second x))
)
)

(defun ad-cos (x)
(list 
(cos (first x))  (- ( * (sin (first x))  (second x)))
)
)

(defun adify-atom (x)
(case x
  (+ 'ad-+)
  (* 'ad-*)
  (cos 'ad-cos)
  (sin 'ad-sin)
  (otherwise `(list ,x 0))
  )
)

(adify-atom '+)
(adify-atom 1)
(adify-atom 'cos)

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

(adify-exp '(* 1 (+ 2 3)))

(defmacro adify (name var-list (&body exp))
  `(defun ,name ,var-list
      ,(adify-exp exp)
     )
  )


(adify p (x y z) (+ x y))

(macroexpand-1 '(adify p (x y z) (+ x y)))


(macroexpand-1 '(adify p (x y z) (+ x y)) )

(macroexpand-1 '(adify p (x y z) (+ x (+ y z)) ))

(adify p (x y z) (* x (+ y (+ z (cos x)))) )


(macroexpand-1 '(adify p (x y z) (* x (+ y (+ z (cos x)))) ))

(macroexpand-1 '(adify p (x y z) (defun (a b ) (+ a ) ) ) )

(p 1 2 0)

(ad-+ '(1 2) '(3 4))
(ad-* '(1 2) '(3 4))
