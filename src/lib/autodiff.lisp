(in-package #:lisphys)

(defmethod val (x)
  (cond
    ((numberp x) x)
    (t (first x))))

(defmethod der (x)
  (cond 
    ((numberp x) 0.)
    (t (second x))))

(defmacro adify-function (name deriv)
  `(defun ,(symbolicate "AD-" name) (x)
     (list (,name (first x)) ,deriv)
     )
  )

;TODO : add domain definition.
(adify-function cos (* (der x) (- (sin (val x)) )))

(adify-function sin (* (der x) (cos (val x)) ))

(adify-function sqrt (/ (der x) (* 2.0 (sqrt (val x))) ))

(defun ad-+ (&rest x)
  (list 
   (reduce #'+ (mapcar #'val x) )
   (reduce #'+ (mapcar #'der x) )
   )
  )

(defun ad-- (first &rest x)
  (if x
    (list 
     (reduce #'- (cons (val first) (mapcar #'val x) ))
     (reduce #'- (cons (der first) (mapcar #'der x) )))
    (list 
     (- (val first))
     (- (der first)))))

(defun ad-* (x y)
(list 
(* (val x) (val y))  (+ (* (val x) (der y)) (* (val y) (der x)))
)
)

(defun ad-/ (x y)
  (let ((y2 (* (val y) (val y))))
    (list 
     (/ (val x) (val y))
     (/ (- (* (der x) (val y)) (* (val x) (der y))) y2) )))

;;Add atom to *list* to avoid to have to add atom to the list below :
(defun adify-atom (x)
  (case x
    (+ 'ad-+)
    (* 'ad-*)
    (- 'ad--)
    (/ 'ad-/)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Helper                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro pick-class (test-class instance name)
  `(if (subtypep (type-of ,instance) ',test-class)
      (symbolicate ,(string-upcase name) "AD")
      (symbolicate ,(string-upcase name))))


(defmacro !!ad (&body body)
  "Converts infix to prefix adn adify the results"
  (adify-exp (infix->prefix body (list '+ '- '* '/))))
