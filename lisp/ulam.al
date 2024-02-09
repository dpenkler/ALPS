;;;-*- mode: emacs-lisp -*-
;;
;; Explore algorithms to generate Ulam's spiral
;;

;; Ulam and Ulam2 are defined in idioms.al


(require 'prime)
(defun uli (N)  ;; A fresh attempt
  (let* ((#IO 0) (NN (sqr N)) (G (f (/ N 2))) (F (vtr {G (- G (~ (| 2 N)))}))
	 (L (p [2 8]
	       [-1  0  1  1  0  0 -1 -1    ;; Y coordinate moves (dim 1)
	         0  1  0  0 -1 -1  0  0])) ;; X coordinate moves (dim 2)
	 (SI   [ 0  2  1  1  1  1  1  1])  ;; move multiplier increments
	 (K (rav (+ 1 (o '* (i G) SI))))   ;; expanded move compress selector
	 (SPIND (+ F  (s '+ 2 (k 2 K (aref L () (| 8 (i (* 8 G)))))))) ;; spiral
 	 (A (p {N N} 1))   ;; Array with ones along the sprial of primes
	 (P (p NN 0)))     ;; Vector of zeros to receive one at prime indexes
    (iset P 1 (Primes NN)) ;; (aset P 1 I) for I in primes less or equal to NN
    (iset A (cat (dp 1 P) 0) (tk {2 NN} (cat F SPIND))) 
    (aref "-*" A)))

(defun uli2 (N) ;; Modification of the APL version from idioms.al
  (let* ((#IO 1) (A (i (* N 2))) (NN (sqr N)) 
	 (C (| 4 (k (| (s '- 1 -1 A)) A))) ; note apl scan semantics
	 (G (f (+ .5 (/ N 2)))) (F (vtr {(+ G (~ (| 2 N))) G}))
	 (D (+ C (* 4 (= 0 C))))
	 (L (p [2 4] [-1 0 1 0 0 1 0 -1]))
	 (E (aref L () (tk  (- NN 1) D)))
	 (SPIND (+ F (s '+ (cat 0 E)))) ;; Spiral indexes
	 (A (p {N N} 0))   ;; Array with ones along the sprial of primes
	 (P (p NN 1)))     ;; Vector of 1s to receive 2s at prime indexes
    (iset P 2 (Primes NN)) ;; (aset P I 1) for I in primes less or equal to NN
    (iset A P SPIND)       ;; Primes from 1 wrap, 1 and NN not prime
    (aref "-*" A)))

(defun Tuli (N) ;; Test the implementations
  (let ((#IO 1) (L (explod (i 20)))
	(U '(Ulam2 uli uli2))
	(V $[Ulam Ulam2 uli uli2])
	(PRIMES 'CPr) ;; Force everybody to use the cached prime function
	(Primes 'CPr)
	(T '(lambda (F)
	      (apply 'and (mapcar '(lambda (X) (eql (Ulam X) (F X))) L)))))
    (mapc '(lambda (X) (unless (T X) (error {X " failed"}))) U)
    (FOR F 1 4 (gc)
	 (princl (fmt (aref V F):6 "  " (aref (Time ((aref V F) N)) 1):7:4)))
    'ok))
