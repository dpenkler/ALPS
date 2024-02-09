;;;-*- mode: emacs-lisp -*-
;;
;; Testing parallel evaluation
;;

(defun TPEVAL (N) 
   (prog ((TTQ '(lambda (X) (TQ X) (fmt "(TQ " X ") Round number "(incr TQC))))
	  (TQC 0) TTASK Terminator R Z T TP)
	 (defun R (X Y) 
	   (prog (Z (BOO '(lambda () (print (list X I)))))
		 (FOR I 0 Y
		      (FOR J 1 20000 (setq Z (+ I J)))
		      (if (zerop (| 20 I)) (BOO))))
	   (list 'Teamaker X "number" TCnt "done."))
	 
	 (defun Z (S) (peval '(R S 100)) )
	 
	 (defun T (N) 
	   (print (fmt "Launched (T " N ")"))
	   (setq Ticker t)
	   (let ((TCnt 0) ML T)
	     (peval 
	      '(while Ticker 
		 (a ML (list (TP))) ; schedule tea makers
		 (princ (fmt "Ticker " (incr TCnt))) (terpri)
		 (while ML 
		   (a T (wait N ML)) ; wait for one to finish or
		   (cond ((nump T) (print "Waiting for tea!")) ; shout
			 ((null T) (print "Done")) ; nothing going on anymore
			 (t                        ; something is finished
			  (cond ((eq (cadr T) 'Teamaker)
				 (princl (cdr T))))   ; a tea maker is finished 
			  (a ML (delete (car T) ML)))))))))
	 
	 (defun TP ()
	   (peval '(let ((A (Z "polly")) (B (Z "sueky")))
		     (print "peval test started")
		     (a ML (append ML (list A B))))))

	 (setq TPEVAL_COMPLIST nil)
	 (push (peval (list 'TTQ N)) 'TPEVAL_COMPLIST)
	 (push (peval (list 'TTQ N)) 'TPEVAL_COMPLIST)
	 (print "launching T")
	 (a TTASK (T 10))
	 (while (a Terminator (wait -1 TPEVAL_COMPLIST))
	   (princl (fmt (car Terminator) " res: " (cdr Terminator) "; Done"))
	   (a TPEVAL_COMPLIST (delete (car Terminator) TPEVAL_COMPLIST)))
	 (setq Ticker nil)
	 (print "Waiting for all done")
	 (wait -1 (list TTASK))
	 "TPEVAL done"))
