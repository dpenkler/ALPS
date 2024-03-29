;;;-*- mode: emacs-lisp -*-

;;; From WINSTON

(defun water (A B C) 
	(prog (W1 W2)
	      (setq #PP 0)
	      (setq W1 (w-c A B C))
	      (setq W2 (w-c B A C))
	      (cond ((equal W1 W2) W1)
		    ((le (len W1) (len W2)) W1)
		    (t W2))))
    
(defun w-c (A B C)
       (cond ((and (gt C A) (gt C B))   (list C "TOO LARGE"))
             ((not (zerop (| (GCD A B) C))) (list C "NOT POSSIBLE"))
             (t (w-m 0 0))))
      
(defun w-m (X Y)  
  (cond ((eq X C)       (cons (list "correct amount in" A) nil))
        ((eq Y C)       (cons (list "correct amount in" B) nil))
        ((eq X A)       (cons (list "empty" A) (w-m 0 Y)))
        ((eq Y 0)       (cons (list "fill"  B) (w-m X B)))
        ((gt (- A X) Y) (cons (list "empty" B "into" A)
                              (w-m (+ X Y) 0)))
        (t              (cons (list "fill"  A "from" B)
                              (w-m A (- Y (- A X)))))))

(defun Simplify (W)  
  (prog (UR ST SR CPOS)
      (setq UR (UNIQ W))
      (setq ST (mapcar '(lambda (X) (list  X (gensym))) UR))
      (setq SR (mapcar '(lambda (X) (cadar (ASSOC X ST))) W))
      (defun CPOS (X Y) ; how many times does X occur in Y
	     (prog (N) (setq N  0) 
		   (while Y (cond ((PR X Y) (setq N (+ N  1)))) 
			  (setq Y (cdr Y))) N))
  )
)

