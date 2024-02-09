;;;-*- mode: emacs-lisp -*-
;;; gc bound due to atm alloc
(defun Queens (N) ; fully functional version
  ((label FB lambda 
	  ( BR    ; list of row    coords for current board solution
	    BC    ; list of column coords for current board solution
	    MR    ; row    coord of queen being placed
	    MC    ; column coord of queen being placed
	    Res ) ; list of solutions
	  
	  (cond ((zerop MC)                      ; No more columns
		 (FB BR BC N N                   ; start again
		     (cons (mapcar 'list BC BR) Res))) ; adding solution
		((zerop MR)                      ; No more rows
		 (cond ((null BR) Res)           ; no queen was placed
		       (t (FB (cdr BR) (cdr BC)  ; backtrack and start next row
			      (- (car BR) 1) (car BC) Res))))
		((ngenc '(lambda (TR TC) (or (eq MR TR) (eq MC TC) 
						  (eq (- MR MC) (- TR TC))
						  (eq (+ MR MC) (+ TR TC))))
			BR BC)                   ; current spot under threat
		 (FB BR BC (- MR 1) MC Res))     ; try next row
		(t (FB (cons MR BR) (cons MC BC) ; else add spot to board
		       N (- MC 1) Res))))        ; and try next column

	 nil nil N N nil)) ; starting values


(defun Queens1 (N) ; functional classic 
  (let (FB TA TB)
    (defun TA (TR TC) ; Threat analysis
      (cond ((null TR) nil)
	    ((or (eq MR (car TR)) (eq MC (car TC)) 
	      (eq (- MR MC) (- (car TR) (car TC)))
	      (eq (+ MR MC) (+ (car TR) (car TC)))) t)
	    (t (TA (cdr TR) (cdr TC)))))

    (defun TB (TR TC) (ngenc '(lambda (TR TC) (or (eq MR TR) (eq MC TC) 
						  (eq (- MR MC) (- TR TC))
						  (eq (+ MR MC) (+ TR TC))))
			     TR TC))
    (defun FB (BR BC MR MC Res )
      (cond ((zerop MC)                      ; No more columns
	     (FB BR BC N N                   ; start again
		 (cons (mapcar 'list BC BR) Res))) ; adding solution
	    ((zerop MR)                      ; No more rows
	     (cond ((null BR) Res)           ; no queen was placed
		   (t (FB (cdr BR) (cdr BC)  ; backtrack and start next row
			  (- (car BR) 1) (car BC) Res))))
	    ((TB BR BC) (FB BR BC (- MR 1) MC Res)) ;under threat ->try next row
	    (t (FB (cons MR BR) (cons MC BC) ; else add spot to board
		   N (- MC 1) Res))))        ; and try next column
    
    (FB nil nil N N nil)))

(defun Queens2 (N) ; block interative
  (prog (BR BC (MR N) (MC N) Res)
    
    (while t
      (cond ((zerop MC) (a MR N MC N Res (cons (mapcar 'list BC BR) Res)))
	    ((zerop MR) (cond ((null BR) (return Res))
			      (t (a MR (- (car BR) 1) MC (car BC) 
				    BR (cdr BR) BC (cdr BC)))))
	    ((ngenc '(lambda (TR TC) (or (eq MR TR) (eq MC TC) 
					 (eq (- MR MC) (- TR TC))
					 (eq (+ MR MC) (+ TR TC))))
		    BR BC) (a MR (- MR 1)))
	    (t  (a BR (cons MR BR) BC (cons MC BC) MR N MC (- MC 1)))))))
    
(defun Queens3 (N) ; goto iterative
  (prog (TA BR BC (MR N) (MC N) Res)
    (defun TA (TR TC) 
      (ngenc '(lambda (TR TC) (or (eq MR TR) (eq MC TC) 
				  (eq (- MR MC) (- TR TC))
				  (eq (+ MR MC) (+ TR TC)))) TR TC))
     (a MR N)
LR   (a MC N)
LC   (if (TA BR BC) (go 'UC))
     (a BR (cons MR BR) BC (cons MC BC))
     (if (zerop (a MR (- MR 1)))  (a Res (cons (mapcar 'list BC BR) Res)))
     (go 'LR)
UN   (if (null BR) (return Res)
       (a MR (car BR) MC (car BC) BR (cdr BR) BC (cdr BC)))
UC     (if (zerop (a MC (- MC 1))) (go 'UN)
            (go 'LC))
       ))
     

(defun Queens4 (N) ; mutually recursive functional
  (prog ((QA '(lambda (BR BC MR) 
		(cond ((zerop MR) (a  Res (cons (mapcar 'list BC BR) Res)))
		      (t (QB BR BC MR N)))))
	 (QB '(lambda (BR BC MR MC) 
		(cond ((zerop MC))
		      (t (cond ((TA))
			       (t (QA (cons MR BR) (cons MC BC) (- MR 1))))
			 (QB BR BC MR (- MC 1))))))
	 (TA '(lambda ()
		(ngenc '(lambda (TR TC) (or (eq MR TR) (eq MC TC) 
					    (eq (- MR MC)  (- TR TC))
					    (eq (+ MR MC) (+ TR TC))))
		       BR BC)))
	 Res)
	(QA nil nil N)
	Res))

(defun Queens5 (N) ; block interative
  (prog (BR BC (TA (p {N N} 0)) (MR N) (MC N) Res)
    
    (while t
      (cond ((zerop MC) (a MR N MC N Res (cons (mapcar 'list BC BR) Res)))
	    ((zerop MR) (cond ((null BR) (return Res))
			      (t (a MR (- (car BR) 1) MC (car BC) 
				    BR (cdr BR) BC (cdr BC)))))
	    ((ngenc '(lambda (TR TC) (or (eq MR TR) (eq MC TC) 
					 (eq (- MR MC) (- TR TC))
					 (eq (+ MR MC) (+ TR TC))))
		    BR BC) (a MR (- MR 1)))
	    (t  (a BR (cons MR BR) BC (cons MC BC) MR N MC (- MC 1)))))))

(defun TQueens (N) 
  (mapc '(lambda (Q) (print Q) (gc) (print (TIME (len (Q N)))))
	'(Queens Queens1 Queens2 Queens3 Queens4 Queens5)))
