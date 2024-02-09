;;;-*- mode: emacs-lisp -*-
;;
;; fun with Leonardo Fibonacci 1170-1250 and friends
;;

;; For a comprehensive discussion of all things Fibonacci see
;; https://mathworld.wolfram.com/FibonacciNumber.html

;;
;; Here we explore various ways of coding a function to generate
;; the fibonacci numbers

(defun ffib (N) ; doubly recursive (super slow)
  (cond ((lt N 2) N) (t (+ (ffib (- N 1)) (ffib (- N 2))))))

(defun Y (F) ; paradoxical Y combinator (Steel & Sussman AIM-453)
  ((lambda (G) 
     (fun (lambda (X) ((F (G G)) X)))) 
   (fun (lambda (G) 
	  (fun (lambda (X) ((F (G G)) X)))))))

(defun FIB (K) ; obscure lisp version (also super slow)
  ((Y '(lambda (F)
	 (fun (lambda (N) 
		(cond ((zerop N) 0)
		      ((onep N) 1)
		      (t (+ (F (- N 1)) (F (- N 2))))))))) K))

(defun fib (N) ; simple recursive 
  ((label F lambda (X Y) (cond ((zerop N) X) ((decr N) (F Y (+ Y X))))) 0 1))

(defun ifib (N) (let ((F 0) (G 1) (C 1) T) ; scalar fully iterative
		  (while (lt C N) (a T G G (+ F G) F T) (incr C)) G))

(defun vifib (N) (let ((F [0 1])) ; vector fully iterative
		  (if (zerop N) 0
		    (FOR I 1 N (a F (dp 1 (cat F (r '+ F))))) (p [] F))))

(defun bfib (N)  ; Binet's formula
  (/ (- (exp (+ 1 (sqrt 5)) N) (exp (- 1 (sqrt 5)) N)) (* (exp 2 N) (sqrt 5))))

;; Peter-Michael Hager from http://dfns.dyalog.com/n_fibonacci.htm
(defun pfib (n)
  (let ((p (* .5 (+ 1 (sqrt 5))))  ; GR
	(q (* .5 (- 1 (sqrt 5))))) ; (/ GR)
    (/ (- (exp p n) (exp q n)) (- p q))))

;; +∘÷/40⍴1 example of reduction with Beside +∘÷ Bob Smith dyalog
(de CalcGR (N)  (Reduce (be '+ '/) 1 -1 (p N 1)))
;;  (FOR I 1 40 (print (CalcGR I)))

(defun gfib (N) ; generate fibonacci numbers using the golden ratio
  (f (+ (/ (exp GR N) (sqrt 5)) .5)))

(defun ggrwf (N) ; generate GR with ratios of successive fibonacci numbers
  (let ((F (afibl N))) (- GR (/ (dp 1 F) (dp -1 F)))))

(de TGGR () (let ((#SD 3)) (print (ggrwf 42)) t)) ; show convergence of ggrwf

(defun Fib (X) ; somewhat obscure apl version (see PasFib)
  (let ((W (- (i X) #IO))) (r '+ (! (rev W) W)))) 

(defun PasFib (n) ; sum shallow diagonals of pascal's triangle
    (let* ((#IO 0) (q (i n))) (r '+ (shft 1 (- q)  (tr (o '! q q))))))

(defun pasfib (X)  (p [] (tk -1 (PasFib X)))) ; wrapper for PasFib

(defun afibl (X) ; clearer version from Iverson's Notation as a Tool for thought
  (if (onep X) 1 ((lambda (Z) (cat Z (r '+ (tk -2 Z)))) (afibl (- X 1)))))

(defun afib (X)  (p [] (tk -1 (afibl X)))) ; wrapper for afibl

(require 'dfns)
(a afib2 (dfmd (a a [0 1])  ;; short and sweet recursive apl version
	       (if (zerop w) (p [] a) (d (dp 1 (cat a (r '+ a))) (- w 1)))))

;; extension to real and complex numbers
(defun cfib (x) (/ (- (exp GR x) (* (% 2 (% x)) (exp GR (- x)))) (sqrt 5)))

;; (plot (list ((dfd (cat .5 w ((arg a) w))) cfib (i [-10 .1 10]))))

(defun TFIB ()
    (mapc '(lambda (F) (prin0 F) (tab 7)
	     (print (mapcar '(lambda (X) (F X)) (explod (i 20)))))
	  '(fib ffib FIB ifib vifib bfib pfib gfib Fib pasfib afib afib2)))

(df stime (x) (let ((t1 (time))) (eval (car x)) (aref (- (time) t1) 1)))

(defun RFIB () ;A fib race; note ffib and FIB can run really slowly
  (mapc '(lambda (X) (princl (fmt (car X):7 (cadr X):10:7 " secs")))
	(sort '(lambda (X Y) (lt (cadr X) (cadr Y)))
	      (mapcar '(lambda (fn) (list fn (stime (fn 32))))
		      '(ffib FIB fib ifib vifib bfib pfib gfib Fib
			     pasfib afib afib2)))))
	


;; (aref " *" (+ 1 (enc (p 48 2) (PasFib 70))))
;;                                                                      *
;;                                                                    ** 
;;                                                                   * **
;;                                                                  *    
;;                                                                **** **
;;                                                               * *    *
;;                                                             **  *  ** 
;;                                                            * *****  **
;;                                                          **  **** *   
;;                                                         * *** *  **   
;;                                                       **   * ***  ** *
;;                                                      * ** *   * *     
;;                                                     *    *** *  *    *
;;                                                   **** ** * *****  ** 
;;                                                  * *      * **** *    
;;                                                **  *    **  **  **   *
;;                                               * ****   * **  **  ** * 
;;                                             **  **  * *   **  **   ** 
;;                                            * **  **** *  * **  ** * **
;;                                           *   **  *  ****    *    * **
;;                                         **** * ****** *  * ***  **    
;;                                        * *   * ***** ****  ** *   * **
;;                                      *** *  *  ***    *  *   **  *    
;;                                     * * ******* * * ******* * ****    
;;                                   **  * *****  ** * *****  ** **  * **
;;                                  * **** **** *    * **** *     ***  **
;;                                **  **    *  **  **  **  **    * * *   
;;                               * **  ** ****  **  **  **   * *** * *   
;;                              *    *    *** *  **  **  ** *   * ** *   
;;                            **** ***  ** * ***  **  **   **  *    **  *
;;                           * *    * *    * ** *  **  ** * ****  ** ****
;;                         **  *  *** *  **    **** **    * *** *    **  
;;                        * ****** * ***   * ** *     * **   * **  ** ***
;;                      **  ****   * ** * *     *    *    * *    *    ***
;;                     * *** *  * *     * *   **** **** *** *  **** ** * 
;;                   **   * ***** *   *** *  * *    *    * **** *     ***
;;                  * ** *  **** **  * * ***** *  **** **  **  **    * * 
;;                 *    **** *    **** * **** **** *    **  **   * **  * 
;;               **** ** *  **  ** *   * **    *  **   * **  ** *   **** 
;;              * *     **** **   **  *    * ****  ** *    *   **  * *   
;;            **  *    * *     * * ****  **  *** *   **  **** * **** *  *
;;           * ****  *** *   **  * *** *  **  * *** * *** *   * **  **** 
;;         **  *** *  * *** * ***  ** **** ***   * ** ** *** *   **  *   
;;        * *** * ****   * ** ** *    **    * * *         * *** * ****   
;;      **   * ** *** * *       **   * ** *** * *       **   * ** *** * *
;;     * ** *      * ** *      * ** *      * ** *      * ** *      * ** *
;;   **    **    **    **    **    **    **    **    **    **    **    **
;; ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *"


;;
;; Bonus Pell sequence
;; From https://en.wikipedia.org/wiki/Pell_number
;;
(de PellR (N) (cond ((zerop N) 0) ;; recurrence relationship
		    ((onep N) 1)
		    ((+ (* 2 (PellR (- N 1))) (PellR (- N 2))))))


(de PellB (N) ;; Binet like formula
    (let ((SR2 (sqrt 2))) (/ (- (exp (+ 1 SR2) N) (exp (- 1 SR2) N)) (* 2 SR2))))

(de PellM (N) ;; Matrix formula
    (let ((#IO 1) (M  (p [2 2] [2 1 1 0])))
      (if (zerop N) 0
	(aref ((label PM lambda (N) (cond ((onep N) M)
					  (t (. '+ '*  (PM (- N 1)) M))))
	       N)
	      1 2))))

(defun TPell ()
  (let* ((#IO 0) (R (explod (i 16))))
    (mapc '(lambda (F) (prin0 F) (tab 7)
	     (print (mapcar '(lambda (X) (F X)) R)))
	  '(PellR PellB PellM))))


(de PellSR2 (N) ;; approximation to square root of 2
    (if (lt N 1) (error "N must be at least 1")
	(let ((PN (PellR N)) (PN1 (PellR (- N 1))))
	  (/ (+ PN PN1) PN))))

;; (- (sqrt 2)(PellSR2 20)) ;=> -1.33226763e-15

(de Poct (N) ;; approximate octagon with Pell numbers
    (let* ((P (PellR N)) (Q (PellR (- N 1))) (R (+ P Q))
	   (RS (implod (mapcar vtr (list {R 0} {(- R) 0} {0 R} {0 (- R)}
					 {P P} {(- P) P} {P (- P)} {(- P) (- P)})))))
      (/ RS (r 'c (rav RS)))))

(de Ploct (N) ;; plot octagon with Pell numbers N, N-1
    (require 'graf)
    (let ((#DM EZ_LINE_LOOP))
      (Ginit2D)
      (graf (ORP (Poct N)))
    ))
;;
;; Bonus+: The Collatz problem https://mathworld.wolfram.com/CollatzProblem.html
;; This is a sequence of integers but whether the sequence goes to 1
;; for every starting number is not known (July 2020)

;; The sequence of integers generated by the problem is sometimes
;; called the hailstone sequence
;; https://mathworld.wolfram.com/HailstoneNumber.html

(defun coll (n) ; lisp style
  (cond ((onep n) [1])
	((oddp n) (cat n (coll (+ 1 (* 3 n)))))
	((cat n (coll (/ n 2))))))

(defun coll1 (n) ; old apl style
  (prog ()
	(go (k {(= 1 n) (= [1 0] (| 2 n))} $[one odd even]))
	one: (return [1])
	odd: (return (cat n (coll1 (+ 1 (* 3 n)))))
	even: (return (cat n (coll1 (/ n 2))))))
 
;; Hailstones being whipped up in a cloud and finally dropping,
;; pictured sideways

(defun shwhs (N)
  (let* ((A (coll N)) (C (rev (tr (BarGraph A)))))
    (princ {(fe2 blzs (MkStr A)) (p {(p A) 1} " ") C}) []))

;; (shwhs 18) =>
;; 18 ##################                                  
;;  9 #########                                           
;; 28 ############################                        
;; 14 ##############                                      
;;  7 #######                                             
;; 22 ######################                              
;; 11 ###########                                         
;; 34 ##################################                  
;; 17 #################                                   
;; 52 ####################################################
;; 26 ##########################                          
;; 13 #############                                       
;; 40 ########################################            
;; 20 ####################                                
;; 10 ##########                                          
;;  5 #####                                               
;; 16 ################                                    
;;  8 ########                                            
;;  4 ####                                                
;;  2 ##                                                  
;;  1 # 
;; []

;;
;; Bonus++
;; Markov numbers
;; They have a relationship with the Fibonacci sequence - see TMEF
;;
(de MEQ (x y z) (+ (sqr x) (sqr z)  (* -3 x y z) (sqr y)))
(de MEaux (x y z) (list x (- (* 3 x y) z) y (- (* 3 z y) x) z))
(de ME (l) (let ((x (car l)) (y (cadr l)) (z (caddr l)))
	     (cond ((null z) nil)
		   (t (append (MEaux x y z) (cdr (ME (cddr l))))))))

(de MET (l n) ;; Generate Markov tree
    (if (eq n 0) nil (cons l (MET (ME l) (- n 1)))))

(de MTrip (l n) ;; Generate list of triples from tree
    (let (M)
      (mapc '(lambda (L) 
	       (while (cdr L) (a M (cons (list (car L) (cadr L) (caddr L))
					 (cons (list (cadr L) (car L) (caddr L)) M)))
		      (a L (cddr L))))
	    (MET l n))
      M))

(de CkMTrip (N) "Check that all triples are a solution"
    (genc '(lambda (X) (print X) (zerop (apply MEQ X)))
	  (MTrip '(1 5 2) N)))
;; Because the numbers grow so fast by level 6 the number of digits exceed double precision ~17 digits
;; so (CkMTrip 6) fails
    
;; Frobenius conjecture: the largest number in a triple uniquely determines the pair of the two lesser numbers
(de MFrob (N) ;; In our case a bit of a joke
    (let* ((L  ;; sort triples from lowest greatest number to highest greatest number
	    (sort '(lambda (X Y) (lt (max (implod X)) (max (implod Y))))(MTrip '(1 5 2) N)))
	   (M (mapcar '(lambda (X) (sort 'lt X)) L))) ;; sort each triple low to high
      (mapc 'print M)
      (while M (unless (equal (car M) (cadr M)) (error (fmt "Mismatch " M)))
	     (a M (cddr M)))
      t))
      
(de TMEF (N) "test that the left branch of the markov tree is equal to every other Fibonacci number"
    (equal (mapcar 'cadr (MET '(1 5 2) N)) (mapcar fib (explod (+ 3 (* 2 (i N)))))))
      
(de TMEP (N) "test that the right branch of the markov tree is equal to every other Pell number"
    (equal (mapcar '(lambda (X) (cadr (revl X))) (MET '(1 5 2) N)) (mapcar PellR (explod (+ 1 (* 2 (i N)))))))
