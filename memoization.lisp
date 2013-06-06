(defmacro equal-args (args)
  `(and  
    ,@(mapcar #'(lambda (x) (= (car x) (cadr x))) args)))

(defmacro defun-mem (name args body)
`(let ((a nil) (res nil))
  (defun ,name (,@args)
     (if (equal (list ,@args) a)
	 res
	 (progn (print ',name) (setq a (list ,@args))
	   (setq res ,body) res)))))


(DEFUN Lievre (X Y)
  (IF (EQUAL `(,X ,Y) '(1 2.0))
      (PROGN (PRINT 'LO) 2.0)
      (PROGN (SETQ A '(X Y) (+ X Y))))

(let ((a nil) (res nil))
(DEFUN licorne (X Y)
  (IF (EQUAL (LIST X Y) A)
      (PROGN (PRINT 'LO) RES)
      (PROGN (SETQ A (list X Y)) (print a) (setq res (+ X Y)) res))))
