;; -*- mode: emacs-lisp; -*-
(require 'prime)
(require 'graf)

;; Perfect, abundant, deficient and amicable numbers and related things


;; Simple APL version that tests every number up to Nth index
(defun PN (N) "Generate perfect numbers up to Nth index"
  (let ((IN (i (tk -1 (sn1 (i N)))))) ;; so that is compatible with PN1-3
    (k (=    ; select integers which are equal to the sum of their factors
	(* 2 IN) ; times 2 cause numbers divide themselves 
	(sf0 IN)) ; sum of factors 
       IN) ;; integers up to nth even perfect number
    ))

(de sf0 (IN) (r '+ 1 (* (~ (o '| IN IN)) (vtr IN)))) ;; sum of factors for elts in IN
(de nf0 (IN) (r '+ 1 (~ (o '| IN IN))))              ;; number of factors for elts in IN

(de FindFactors (X) (let ((IN (i X))) (k  (~ (o '| IN X)) IN)))

(de GFFactors (X)  (let ((V (i X))) (k (= V (v V X)) V))) ; use gcd

(de SumFactors (X) (let ((IN (i X))) (r '+ (k  (~ (o '| IN X)) IN))))

(de NumFactors (X) (let ((IN (i X))) (r '+ (~ (o '| IN X)))))

;; graf perfect, abundant and deficient nos 1-N with moving avg M
;; positive points are abundant, negatives are deficient and zeros perfect
(de GF (N M)
    (let* (
	(I (i N))
	(R (- (/ (sf0 (i N)) 2) I))
	(K (>= R 0))
	(S (cat .5 I R))
	(P (explod (k (= 0 R) I))) (#WN) (#PN 3) (#DM EZ_POINTS))
      (plot (list (k 2 K S)              ;; Number - sum of factors
		  (k 2 (~ K) S)   
		  (cat .5 I (/ (s '+ R) I)) ;; cumulative avg of sum of factors
		  (cat .5 (i {M N})         ;; moving avg of sum of factors
		       (/ (r '+ 1 M R) M))))
      (a #DM EZ_LINES #PN 1)
      (mapc '(lambda (X) (MARK X 0)) P)      ;; mark the perfect numbers
      (axes) ;; refresh
      P))

;; every even perfect number p must be of this form
;; http://mathworld.wolfram.com/PerfectNumber.html
;; Prop  IX.36 of Euclid's Elements (
(de sn (p) (* (exp 2 (- p 1)) (- (exp 2 p) 1))) 

(de sn1 (p) (let* ((pc (- (exp 2 p) 1)) (S (isPrime pc)))
	      (* (exp 2 (- (k S p) 1)) (k S pc))))

(de sigd (x) ;; scalable version of (Sigma (divisors x))
             ;; no significant perf advantage making L > 1e5
    (let (I (J 1) V (L 1e5) (R (c (/ x 2))) (S 0))
      (while (gt R 0)
	(cond ((gt R L) (a I (i {J (+ J L -1)}) R (- R L) J (+ J L)))
	      (t        (a I (i {J (+ J R -1)}) R 0)))
	(a S (+ S (r '+ (k (~ (o '| I x)) I)))))))

(de isPerfRuiz (n)
    (let ((n2 (i (- n 2))) (n1 (i (- n 1))))
      (= (r '+ (* n2 (f (/ n n2)))) (+ 1 (r '+ (* n1 (f (/ (- n 1) n1))))))))

(de PN1 (N) ;; faster version checking only even perfect numbers with form (sn)
    (let* ((S (sn (i {2 N}))) (R (implod (mapcar sigd (explod S)))))
      (k (= R S) S)))

(de PN2 (N) ;; Slower version based on Ruiz test
    (let* ((S (sn (i N))) (R (implod (mapcar isPerfRuiz (explod S)))))
      (k R S)))

(de PN3 (N) ;; Fastest  selecting from (sn) ones with Mersenne prime factor
    (sn1 (i N)))

(de TPN (N) ;; Test the perfect number generators to N
    (mapc '(lambda (F) (a T1 (time) R (F N) T2 (- (time) T1))
	     (princl (fmt F:8 (aref T2 1):8:5 R:6)))
	  '(PN PN1 PN2 PN3)))

(de FM (N) ;; The most deficient/defective numbers are primes
    (let* ((I (i N)) (R (dp 1 (- (sf0 I) I))) (V (r 'f R)))
      ;; Assert (eq V 1) 
      (k (=  V R) (dp 1 I)))) ;; most deficient numbers
;; not surprising since 1 is a factor of all numbers and
;; is the only factor (excluding the number itself) the
;; number must be prime
;; See also (LNF 2 N) below

(de FA (N) "Find Amicable numbers in (i N)"
;; Two numbers are amicable if the sum of their factors are equal
;; and the sum of the two numbers is equal to the sum of their factors
    (let* ((SF (sf0 (i N)))                          ;; Sum of Factors
	   (DD (k (<> 1 (r '+ (o '= SF SF)))         ;; remove unique sums with their product
		  (cat .5 (i N) SF)))                ;; tack on products to keep track of them 
	   (DS (aref DD () (gup (aref DD 2 ()))))    ;; sort by increasing sums
	   (R (aref DS 2 ()))                        ;; handle on sums
	   (S (aref DS 1 ()))                        ;; handle on corresponding products
	   (RD (k (= (ind R R) (i (p R))) R))        ;; remove duplicate sums
	   (T (* (o '= RD R) (p {(p RD) (p R)} S)))) ;; split equal sum products onto different rows
      ;; leaving APL generate list of candidate friends with equal factor sum moving to LISP
      (a U (mapcar '(lambda (X Y) (list Y (k (> X 0) X)))  (explod 1 T) (explod RD)))
      ;; list of friends with their sum
      (a V (mapcan '(lambda (X) (let ((SF (car X)) (C (explod (cadr X))))
				  (mapcan '(lambda (X) (if (eq SF (apply '+ X))
							   (list (list SF X))))
					  (comb 2 C))))
		   U))))

(de CKFA (F) ;; Check that they are really friends
    (let ((R 0))
       (mapc '(lambda (X)
		(let ((SF (car X)) (F1 (caadr X)) (F2 (car (cdadr X))))
		  (cond ((ne SF (SumFactors F1)) (incr R)
			 (princl (fmt "sum of factors failed for " F1 " expected " SF)))
			((ne SF (SumFactors F2)) (incr R)
			 (princl (fmt "sum of factors failed for " F2 " expected " SF)))
			((ne SF (+ F1 F2)) (incr R)
			 (princl (fmt F1 " and " F2 " do not sum to " SF))))))
	     F)
       (princl (fmt "Done: " R (if (onep R) " failure." " failures.")))
       'R))

(de LNF (N M) "Make list of numbers with N factors"
       (let ((LM (explod (i M))))
	 (mapcan '(lambda (X) (if (eql (car X) N) (list (cdr X))))
		 (pairlis (mapcar NumFactors LM) LM))))

(de ANF (N M) "Make vector of numbers with N factors"
    (let* ((IM (i M)) (R (cat 0.5 (nf0 IM) IM)))
      (aref (k (= N (aref R 1 ())) R) 2 ())))

(de ckxnf (N M) (onep (r '^ (= (implod (LNF N M)) (ANF N M))))) ;; check Array with List
      
;; (LNF 2 M) generates prime numbers up to M
;; (LNF 3 M) generates perfect squares up to M (squares of prime numbers)
;; (LNF 5 M) generates squares of the squares of prime numbers
;; (LNF 7 M) generates squares of the cubes of prime numbers

;; (PN3 19) ;=> [6  28  496  8128  33550336  8589869056  1.374386913e11]

;; (TPN 10)
;; PN       5.01508     6    28   496  8128
;; PN1       .01411     6    28   496  8128
;; PN2       .03546     6    28   496  8128
;; PN3       .00004     6    28   496  8128
;; Sourcefile alps.c Time-stamp: <2019-07-08 15:53:35 dave>
;; gccVersion 8.3.0
;; gccFlags -O4 -m64 -march=native -fomit-frame-pointer -fno-caller-saves -std=gnu99 
;; gccHost    Linux elgonzo 5.3.0+ #1 SMP PREEMPT Thu Sep 19 11:55:35 CEST 2019
;; gccProc    x86_64 Intel(R) Core(TM) i5-4300U CPU @ 1.90GHz GenuineIntel
;; Executable ./alpsf Linux 64bit Interpreter V7.83 cpt 100
;; Memory allocation Summary Total Workspace size: 6144MB

; End of file: perfnum.al
