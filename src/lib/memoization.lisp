(in-package #:lisphys)

(defmacro defun-mem (name args body)
`(let ((a nil) (res nil))
  (defun ,name (,@args)
     (if (equal (list ,@args) a)
	 res
	 (progn (print ',name) (setq a (list ,@args))
	   (setq res ,body) res)))))
