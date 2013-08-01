(in-package #:lisphys)

(defclass math ()
  ((add :accessor math-add :initarg :add :initform #'+ :allocation :class)
   (sub :accessor math-sub :initarg :sub :initform #'- :allocation :class)
   (mult :accessor math-mult :initarg :mult :initform #'* :allocation :class)
   (div :accessor math-div :initarg :div :initform #'/ :allocation :class)
   (sqrt :accessor math-sqrt :initarg :sqrt :initform #'sqrt :allocation :class)
   (sin :accessor math-sin :initarg :sqrt :initform #'sin :allocation :class)
   (cos :accessor math-cos :initarg :sqrt :initform #'cos :allocation :class))
)

(defclass math-ad (math)
  ((add :initform #'ad-+ :allocation :class)
   (sub :initform #'ad-- :allocation :class)
   (mult :initform #'ad-* :allocation :class)
   (div :initform #'ad-/ :allocation :class)
   (sqrt :initform #'ad-sqrt :allocation :class)
   (sin :accessor math-sin :initarg :sqrt :initform #'ad-sin :allocation :class)
   (cos :accessor math-cos :initarg :sqrt :initform #'ad-cos :allocation :class))
)

(defmethod is-ad (obj)
  nil)

(defmethod is-ad ((obj math))
  nil)

(defmethod is-ad ((obj math-ad))
  t)

(defmacro with-ad# (obj &body body)
  `(with-slots ((!+ add) (!- sub) (!* mult) (!/ div) (!sqrt sqrt) (!sin sin) (!cos cos) (!sqrt sqrt)) ,obj
		,@body))

(defmacro with-ad (obj &body body)
  `(if (is-ad ,obj)
       (macrolet ((!!! (&body expr) `(!!ad ,@expr)))
	 (with-ad# ,obj ,@body))
       (macrolet ((!!! (&body expr) `(!! ,@expr)))
	 (with-ad# ,obj ,@body))
       ))
