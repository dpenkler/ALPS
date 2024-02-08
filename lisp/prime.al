; -*- mode: emacs-lisp; -*-
(require 'idioms) ; for PRIMES
;; lisp and apl style functions for generating primes and factorising
;; into prime factors

(defun factorise (X) ;; Returns a list of the prime factors of X
  (let ((factaux
	 '(lambda (D X L) 
	    (let ((G (v X D)))
;;	      (print (list G D X L))
	      (cond ((onep X) L)
		    ((onep D) (cons X L))
		    ((onep G) (factaux (- D 1) X L))
		    (t (append (factorise G) (factaux (- D 1) (/ X G) L))))))))
    (factaux (f  (sqrt X)) X nil)))

(defun fact1 (X) ;; Returns a list of the prime factors of X
  (let ((factaux
	 '(lambda (D X L) 
	    (prog (G)
		  Again
		  (a G (v X D))
		  (cond ((onep X) L)
			((onep D) (cons X L))
			((onep G) (a D (- D 1)) (go 'Again))
			(t (append (fact1 G) (factaux (- D 1) (/ X G) L))))))))
    (factaux (f (/ X 2)) X nil)))

(defun factm (X) ;; returns a vector of the powers of the primes dividing X
  (let* ((factaux '(lambda () (k (= 0 (| P X)) P)))
	 (P (CPr X)) (T (factaux)) (S 0))
    (a P (tk (ind P (tk -1 T)) P)) ; use only as many primes as needed
    (while (not (onep X)) (a T (factaux) X (/ X (r '* T)) S (+ S (elt P T))))))

(defun TFACTM (N) ;; test factm
  (let* ((EXP '(lambda (X) (r '* (exp (tk (p X) (CPr N)) X)))))
    (FOR I 2 N (if (ne I (EXP (fact I)))
		   (error (fmt "no match for " I))
		 (print I)))))

(defun fact (X) ;; Returns a vector of the prime factors of X
  (let ((M (factm X))) (k M (tk (p M) (CPr X)))))

(defun TFACT (N) ;; test fact
  (FOR I 2 N (if (or (neql (fact I) (implod (sort 'le (factorise I))))
		     (neql (fact I) (implod (sort 'le (fact1 I)))))
		   (break (fmt "no match for " I))
		 (print I))))

(defun DPrimes (R) ;; Dumb apl style prime number generator
  (let* ((#IO 1) (T (dp 1 (i R))))
    (k (~ (elt T (o '* T T))) T)))

(defun OPrimes (R) ;; slightly Optimised apl style PNG
  (let* ((#IO 1) (J (cat 2(dp 1 (i {1 2 R})))))
    (k (~ (elt J ((lambda (X) (o '* X X)) (+ (* 2 (i (f (/ R 2)))) 1)))) J)))

(defun Primes (R) ;; Optimised mixed apl lisp PNG
 (prog ((#IO 1)(P [2]) (A 0) (S 1) (I [])) 
   (cond ((lt R 2) (return []))
	 ((eq R 2) (return P)))
   (a S (c (sqrt R)) I (+ 1 (* 2 (i (f (/ (- R 1) 2))))))
   (repeat (ge A S) (a A (tk 1 I) P (cat P A) I (k (<> 0 (| A I)) I)))
   (cat P I)))
				
(setq CPr ; Cacheing prime
  (let ((Pr Primes) (ON 0) (PrA []))
    (fun (lambda (N) 
	   (cond ((lt ON N) (a ON N PrA (Pr N)))
		 (t (k (<= PrA N) PrA)))))))

(defun GPr (R) ;;  sieve of Eratosthenes
    (prog* ((P '(2)) (Q  P) (X 3) L)
	(while (lt X R)
	  (a L (sqrt X))
	  (cond ((catch 'Prime
		       (mapc '(lambda (K)
				(cond ((gt K L) (throw 'Prime t))
				      ((onep (v X K))) 
				      ((throw 'Prime nil))))
			     P))
		(rplacd Q (list X)) (a Q (cdr Q))))
	  (a X (+ X 2)))
	P))

(defun GPLr (R) ;; Prime list this time with genc and nconc
  (prog ((P (list 2)))
	((label F lambda (X)
		(cond ((gt X R) P)
		      ((prog ((L (sqrt X)))
			     (genc '(lambda (Y) 
				      (cond ((gt Y L) (return t)) 
					    ((onep (v X Y)))))
				   P))
		       (nconc P (list X)) (F (+ X 2)))
		      (t (F (+ X 2))))) 3)))

(defun RanP (R) ;; random prime pair in R
  ((lambda (X) (rav (aref X (? 2 (p X))))) (dp 4 (CPr R)))) ; drop 4

(defun TPRIME (N) ;; test prime number generation (TPRIME 1000)
  (let ((Ref (CPr N)) T1 T2 R)  ; Cache will always be hot
    (mapc '(lambda (F) (a T1 (time) R (F N) T2 (- (time) T1))
	     (if (listp R) (a R (implod R)))
	     (princl (fmt F:8 (aref T2 1):8:5 " check "
			  (if (eql Ref R) "ok" "failed"))))
	  '(DPrimes OPrimes PRIMES Primes CPr GPr GPLr))))

(defun isPrime (X) (elt X (CPr (r 'c X))))


(defun PDist (N) "Show distribution of distances between primes"
       (let* ((D (r '- -1 -2 (CPr N)))
	      (M (r 'c D))) ;; greatest difference
	 (r '+ (o '= (i M) D))))

;; (FOR I 1 100 (prat 1 1 (chr [27 91 74])) (princ (BarGraph (l (+ 1 (PDist (* I 1000)))))) (wait .1))
