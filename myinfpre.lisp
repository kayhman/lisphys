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
    (- 2)
    (* 3)
    (/ 3)
    (otherwise -1)))


(defun add-parenthesis# (exprs)
  (let ((buffer nil)
	(res nil)
	(priority 1))
    (progn 
      (dolist (exp (reverse exprs))
	(cond 
	  ((is-operator exp)
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
	     ))
	  ((listp exp) 
	   (push (add-parenthesis exp) buffer ))
	  (t (push exp buffer))))
    
      (push buffer res)
      (print res))))



(defun add-parenthesis## (exprs)
  (let ((op-stack nil)
	(n-stack nil)
	(stack nil)
	(exp (pop exprs)))
    (loop do 
	 (print exp)
	 (cond
	   ((is-operator exp) (push exp op-stack))
	   ((numberp exp) 
	    (if (> (length op-stack ) 1 )
		(cond 
		  ((< (operator-priority (first op-stack))
		      (operator-priority (second op-stack)))
		   (progn 
		     (push (list (pop op-stack) (reverse (list (pop n-stack) (pop n-stack) (pop op-stack )))) stack)
		     (push exp n-stack)
		     (print exprs))
		   
		   
		   )))
	    	     (push exp n-stack)))
	 
	 (setq exp (pop exprs))
	 while exp
	 )
    (values n-stack op-stack stack)))



(defun add-parenthesis (exprs oop on priority)
  (cond 
    ((numberp (first exprs))
     (let ((nb (pop exprs))
	   (op (pop exprs)))
       (progn
	 (print "call")
	 (print exprs)
	 (print nb)
	 (print op)
	(cond 
	  ((< (operator-priority op) priority)
	   (progn
	     (print "inf branch")
	     (list nb)))
	  ((> (operator-priority op) priority)
	   (progn
	     (print "sup branch")
	     (let ((nn (list op nb (add-parenthesis exprs oop on (operator-priority op)))))
	       (progn
		 (print nn)
		 (add-parenthesis exprs op nn (operator-priority op)))
	       )))
	 
	  ))))))







(add-parenthesis '(1 + 2 * 3 + 2 * ( 5 *7 * 8 + 3 * 2 - 7 + 4 - 12) * 12.0 -12))
