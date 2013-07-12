(in-package #:lisphys)


(defun is-operator (op)
  (case op
    ('+ t)
    ('- t)
    ('* t)
    ('/ t)
    (otherwise nil)))


(defun operator-priority (op)
  (case op
    (+ 1)
    (- 1)
    (* 2)
    (/ 2)
    (otherwise nil)))


(defun add-parenthesis (exprs)
  (let ((buffer nil)
	(res nil)
	(priority 1))
    (progn 
      (dolist (exp exprs)
	(if (is-operator exp)
	    (cond
	      ((> (operator-priority exp) priority)
	       (progn
		 (setq priority (operator-priority exp))
		 (let ((last (pop buffer)))
		   (progn 
		     (push buffer res)
		     (setq buffer (list last))
		     (push exp buffer)
		     )
		   )))
	      
		((< (operator-priority exp) priority)
		 (progn
		   (setq priority (operator-priority exp))
		   (push buffer res)
		   (setq buffer exp)
		   ))
		(t (push exp buffer))
		)
	    (push exp buffer)
	    ))
      (push buffer res)
      (print res))))

