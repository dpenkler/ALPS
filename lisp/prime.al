; -*- mode: emacs-lisp; -*-
(require 'idioms) ; for PRIMES and BARGRAPH
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
  (let* ((#IO 1)(factaux '(lambda () (k (= 0 (| P X)) P)))
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

;; PRIMES as defined in idioms.al
;;(defun PRIMES  (N) (k (= 2 (r '+ 1 (= 0 (o '| (i N) (i N))))) (i N)))

(defun OPRIMES (R) ;; lightly optimised PRIMES
  (prog* ((#IO 1) (C (c (sqrt R))) (S (i {3 2 C})) (P [2]) (Q (i {3 2 R})))
    (cond ((lt R 2) (return []))
	  ((eq R 2) (return P)))
    (cat P (k (= (tk (p Q) (p (p S) 1)) (r '+ 1 (= 0 (o '| S Q)))) Q))))

(defun DPrimes (R) ;; Dumb apl style prime number generator
  (let* ((#IO 1) (T (dp 1 (i R))))
    (k (~ (elt T (o '* T T))) T)))

(defun OPrimes (R) ;; slightly Optimised apl style PNG
  (let* ((#IO 1) (J (cat 2(dp 1 (i {1 2 R})))))
    (k (~ (elt J ((lambda (X) (o '* X X)) (+ (* 2 (i (f (/ R 2)))) 1)))) J)))

(defun Primes (R) "Sieve of Eratosthenes with arrays - Iterative"
       (prog* ((#IO 1) (C 3) (S (f (sqrt R))) (P [2]) (Q (i {3 2 R})))
	      (cond ((lt R 2) (return []))
		    ((eq R 2) (return P)))
	      (while (le C S) (a Q (k (<> 0 (| C Q)) Q) P (cat P C) C (tk 1 Q)))
	      (cat P Q)))

(setq CPr ; Cacheing prime
      (let ((#IO 1) (ON 3) (PrA [2 3])
	    ;; PrC re-uses the previous primes in PrA but if the delta between
	    ;; N and ON is greater that 400 the you might as well use Primes
	    (PrC '(lambda () ;; N ON PrA Free
		    (prog* ((S (f (sqrt N))) (Q (i {(+ ON 1 (| 2 ON)) 2 N}))
			    (N1 (p PrA)) C (P []))
			   (FOR I 2 N1 (a Q (k (<> 0 (| (aref PrA I) Q)) Q)))
			   ;; (a Q (k (= N1 (r '+ 1 (<> 0 (o '| PrA Q)))) Q))
			   (if (zerop (a C (tk 1 Q))) (return PrA))
			   (while (le C S)
			     (a Q (k (<> 0 (| C Q)) Q) P (cat P C) C (tk 1 Q)))
			   {PrA P Q})))
	    (Pr '(lambda () (PrC))))
	(fun (lambda (N) 
	       (cond ((lt ON N) (a PrA (Pr) ON N) PrA)
		     ((eq ON N) PrA)
		     (t (k (<= PrA N) PrA)))))))

(defun CPrInit () (se '(a ON 3 PrA [2 3]) CPr)) ;; reset prime cache

(defun GPr (R) ;;  sieve of Eratosthenes
    (prog* ((P '(2)) (Q  P) (X 3) L)
	(while (le X R)
	  (a L (f (sqrt X)))
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
  (let* ((P '(2)) (Q P))
	((label F lambda (X)
		(cond ((gt X R) P)
		      ((prog ((L (sqrt X)))
			     (genc '(lambda (Y) 
				      (cond ((gt Y L) (return t)) 
					    ((not (zerop (| Y X))))))
				   P))
		       (rplacd Q (list X)) (a Q (cdr Q))  (F (+ X 2)))
		      (t (F (+ X 2))))) 3)))

(defun ErastL (R) "Sieve of Eratosthenes with lists - Recursive"
  (let ((Q (explod (i {3 2 R}))) (A (c (sqrt R)))
	(RF '(lambda (L N) ;; remove from (sieve L for non multiples of N)
	       (mapcan '(lambda (X) (unless (zerop (| N X)) (list X))) L))))
    ((label SV lambda (L M)
	    (cond ((or (gt (car M) A) (null L)) (append (revl M)  L))
		  (t (SV (RF (cdr L) (car L)) (cons (car L) M)))))
     Q '(2))))

(defun RanP (R) ;; random prime pair in R
  ((lambda (X) (rav (aref X (? 2 (p X))))) (dp 4 (CPr R)))) ; drop 4

(defun TPRIME (N) ;; test prime number generation (TPRIME 1000)
  (let ((Ref (Primes N)) T1 T2 R R1)
    (a R1 (mapcar '(lambda (F) (a T1 (time) R (F N) T2 (- (time) T1))
		     (if (listp R) (a R (implod R)))
		     (list F (aref T2 1) " check "
			   (if (eql Ref R) "ok" "failed")))
		  ;; DPrimes, OPrimes and PRIMES are not in the race
		  '(OPRIMES CPr GPr GPLr ErastL Primes)))
    (mapc '(lambda (X) (princl (fmt (car X):8 (cadr X):8:5 (caddr X)
				    (car (cdddr X))))) ;; print sorted list
	  (sort '(lambda (X Y) (lt (cadr X) (cadr Y))) R1))))

(defun isPrime (X) (elt X (CPr (r 'c X))))

(defun PDist (N) "Show distribution of distances between primes"
       (let* ((D (r '- -1 -2 (CPr N)))
	      (M (r 'c D))) ;; greatest difference
	 (r '+ (o '= (i M) D))))

(defun ShowPD () "Show distribution log growth for distances between 100000 successive primes by 1000"
       (prat 1 1(chr [27 91 74]))
       (CPr 100000) ;; warm up the cache
  (FOR I 1 100
       (let ((T  (BarGraph (l (+ 1 (PDist (* I 1000)))))))
	 (prat (- 20 (tally T)) 1 (chr [27 74]))
	 (princ T)
	 (wait .1))))

(defun PrimeBase (x)
  "Convert x to a vector of powers of prime numbers in increasing order"
  (cond ((zerop x) '-Inf)
	((onep x) [0])
	(t  (let ((F (fact x))) (r '+ (o '= (CPr (r 'c F)) F)))))))
;; (PrimeBase 2025) ;=> [0 4 2]
;; (r '* (exp [2 3 5] [0 4 2])) ;=> 2025
