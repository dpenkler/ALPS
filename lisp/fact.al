;;;-*- mode: emacs-lisp -*-
;; Fun with factorials

;; The intuitive implementation of the factorial function for whole numbers
(de fact (n) (if (zerop n) 1 (* n (fact (- n 1)))))

;; Some APL style implementations
(de FACT (n) (r '* (i n)))

;; The double factorial
(de DFACT (n)
    (cond ((lt n -1) nil)
	  ((or (onep (- n)) (zerop n)) 1)
	  ((evenp n) (r '* (* 2 (i (/ n 2)))))
	  ((oddp n) (r '* (- (* 2 (i (/ n 2))) 1)))))

;; Binomial coefficients
(de bin (x)
    (let ((z [1])) (while (ge x (p z)) (a z (+ (cat 0 z) (cat z 0)))) z))

(de bin! (x) (! (- (i (+ x 1)) #IO) x))

;; In how many ways can m things be taken out of a set of n things
(de Comb (m n) (/ (! n) (* (! m) (! (- n m)))))
;; (Comb [0 1 2 3 4 5 6 7 8] 7) ;=> [1 7 21 35 35 21 7 1 0]
;; (!    [0 1 2 3 4 5 6 7 8] 7) ;=> [1 7 21 35 35 21 7 1 0]

;; Fractions work as well
;; (Comb [2.5 2.4] [4 6.7])  ;=> [5.432488724 24.39985592]
;; (!    [2.5 2.4] [4 6.7])  ;=> [5.432488724 24.39985592]

;; Negative values can also be used
;; (o '! (i [-3 3]) (i [-3 3])) ;=>
;;[  1 -2  1 0 0 0 0
;;   0  1 -1 0 0 0 0
;;   0  0  1 0 0 0 0
;;   1  1  1 1 1 1 1
;;  -3 -2 -1 0 1 2 3
;;   6  3  1 0 0 1 3
;; -10 -4 -1 0 0 0 1]

;; Negative non-integers too
;; (Comb [-2.1 -.6] [2 6]) ;=> [7.370511731e-3 .1426763772]
;; (!    [-2.1 -.6] [2 6]) ;=> [7.370511731e-3 .1426763772]

;; Sum of first N natural numbers
;; LISP recursive
(de SigR (N) (if (onep N) 1 (+ N (SigR (- N 1)))))
;; LISP Poly-adic
(de SigL (N) (apply '+ (explod (i N))))
;; APL Classic + reduction
(de SigAPL (N) (r '+ (i N)))
;; Gauss at 9
;; (eql (+ (i 100) (rev (i 100))) (p 100 101)) ;=> t
;; (/ (* 100 101) 2) ;=> 5050
(de SigG (N) (* (+ N 1) N .5))
;; Using Comb
(de SigC (N) (! 2 (+ N 1)))

(de CKSig (N)
    (let ((Sigs '(SigR SigL SigAPL SigG SigC)) (A (SigAPL N)))
      (mapc '(lambda (S) (if (ne (S N) A) (error (fmt S " Failed")))) Sigs)
      '(CKSig passed)))

;; The beta function
(de Beta (x y) (/ (! (- x 1) (+ x y -1)) y))
