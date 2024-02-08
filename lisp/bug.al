(defun bug ()
  (let ((Name 'name)
	(Parameters 'parms)
	(Body 'body)
	(Args 'args)
	(mlet 'mlet)
	)
    (Defun ListB (X Y Z)
      (print (list Name Parameters Body Args mlet))
      (list X Y Z))
    (ListB 1 (+ 1 2) ((lambda (X) (print (list X Args))) "!"))))



;;       (load 'bug)
;; loading bug.al
;; t
;;       (bug)
;; ("!" args)
;; (name parms body args mlet)
;; (name parms body args mlet)
;;       (load 'qq)
;; loading qq.al
;; t
;;       (load 'cl)
;; loading cl.al
;; t
;;       (bug)
;; ("!" args)
;; (name parms body args mlet)
;; (1 3 ("!" args))
      

(df LB1 (Args) ;; generated with built-in 
       (append '(let)
	       (append (list ((label mlet lambda (X Y)
				     (cond ((null X) nil)
					   ((null Y) (cons (car X)
							   (mlet (cdr X) Y)))
					   (t (cons (list (car X) (car Y))
						    (mlet (cdr X) (cdr Y))))))
			      '(X Y Z) Args))
		       (append (list (quote (print (list Name Parameters Body Args mlet)))
					    '(list X Y Z)))
			       'nil))))
;; issue is the quote at the end of LB1 has 2 arguments

(df LB2 (Args) ;; generated with qq.al
       (list* 'let ((label mlet lambda (X Y)
			   (cond ((null X) nil)
				 ((null Y) (cons (car X)
						 (mlet (cdr X) Y)))
				 (t (cons (list (car X) (car Y))
					  (mlet (cdr X) (cdr Y))))))
		    '(X Y Z) Args)
	      '((print (list Name Parameters Body Args mlet)) (list X Y Z))))
