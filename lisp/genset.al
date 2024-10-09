; -*- mode: emacs-lisp; -*-
; variations on combinations, permutation and powerset generation

; commbination of each el of X with each el of Y
(defun COMB (X Y) (mapcan '(lambda (X) (mapcar '(lambda (Y) (list X Y))Y))X))

; commbinations of pairs of elements from Vector Z as array
(defun COMBV (Z)
  (let ((Z1 (explod Z)))
    (implod (mapcan '(lambda (X)
		       (mapcar '(lambda (Y) (tr (cat X Y)))Z1))
		    Z1))))

; Unique commbination of each el of X with each el of Y
(defun UCOMB (X Y)
  (mapcan '(lambda (X)
	     (mapcan '(lambda (Y) 
			(cond ((eq X Y) nil) (t (list (list X Y)))))
		     Y))
	  X))

(defun Pairs (X Y) ; same as COMB
  (cond ((null X) nil)
	((atom X) (cond ((null Y) nil)
			(t (cons (list X (car Y)) (Pairs X (cdr Y))))))
	(t (append (Pairs (car X) Y) (Pairs (cdr X) Y)))))

(defun UNIQ (X Y) ; construct list of unique first level members of X on Y
    (cond (X (cond ((member (car X) Y) (UNIQ (cdr X) Y))
		   (t (UNIQ (cdr X) (cons (car X) Y)))))
	  (t Y)))

(defun comb (N L)
  "Ordered subsets of length N from L"
  (cond ((zerop N) (list nil))
	((null L)  nil) 
	(t (nconc (mapcar '(lambda (X) (cons (car L) X)) (comb (- N 1) (cdr L)))
		   (comb N (cdr L))))))

	

(defun SOSS (X) ; generate list of ordered subsets of X
  (mapn '(lambda (X)
	   (mapn '(lambda (X) (list X)) (revl X)))
	(revl X)))

(defun GOS (X) ; generate list of ordered non-singleton subsets of X 
  (mapn '(lambda (X) 
	   (mapn '(lambda (X) (cond ((null (cdr X)) nil) (t (list X)))) 
		 (revl X)))
	(revl X)))
			
;; (equal (remove-if '(lambda (X) (onep (len X))) (SOSS '(a b c d e)))
;;        (GOS '(a b c d e)))

(defun SoaS (X)  "Generate set of all subsets of X"
       (let (T
	     (GG '(lambda (X Y) "Prefix element X to all subsets of Y"
		    (if (null Y) nil
		      (nconc (FF X Y) 
			      (if (null (cdr Y)) nil
				(a T (GG (car Y) (cdr Y)))
				(if (null X) T
				  (nconc T (FF X T))))))))
	     (FF '(lambda (X Z) "prefix X to each element of Z"
		    (mapcar '(lambda (Q) (cond ((null X) (list Q))
					       ((atom Q) (list X Q))
					       (t (cons X Q))))
			    Z))))
	 
	 (nconc (revl (GG nil X)) (list nil))))


(defun Scomp (S)  "print set of all subsets of S with their complements "
       (mapc '(lambda (X) (print (append (car X) '(-)  (cadr X))))
	     (mapcar '(lambda (X) (list X (scomp S X))) (SoaS S))))

;  (Scomp '(Man Cabbage Goat Wilf))
	
(defun psg (s) ; Powerset generation, nice and compact but slow
  (if (null s) (list nil)
    (nconc (mapcar '(lambda (x) (cons (car s) x)) (psg (cdr s)))
	   (psg (cdr s)))))

(defun psg1 (s)  ; Powerset generation, quicker
  (if (null s) (list nil)
    (let ((x (psg1 (cdr s))))
      (nconc (mapcar '(lambda (x) (cons (car s) x)) x) x))))

(defun psg2 (s) ; Powerset generation, variant on psg1
  (if (null s) (list nil)
    ((lambda (x) (nconc (mapcar '(lambda (x) (cons (car s) x)) x) x))
     (psg2 (cdr s)))))

(defun PSUB (X) ; List of all permutations of the subsets of X
  (prog* ((L (mapcar 'list X)) M (R (append L (list nil))))
	 (mapc '(lambda (Y)
		  (setq M L)
		  (a L (mapcan '(lambda (U)
				  (mapcan '(lambda (V)
					     (cond ((memq U V) nil)
						   (t (list (cons U V)))))
					  M)) 
			       X))
		  (setq R (append L R)))
	       X)
	 R))

(defun TPSG (FL S)  "Test if powerset generation fns in FL on set S are equal"
  ;; (TPSG  '(SoaS psg psg1 psg2)'(a b c d e f g h i j))
  (let ((Q ((car FL) S)) ;; First one is reference result
	(R (mapcar '(lambda (F) (F S)) (cdr FL))))
	(apply 'and
	   (mapcar '(lambda (X)
		      (not (null (genc '(lambda (Z) (member Z Q)) X))))
		   R))))

(defun HB (X) ; Distribution of size of subsets of X gives binomial coeffs
   (let ((#IO 0) (A (p (+ 1 (len X)) 0)) N)
     (mapc '(lambda (X) (setq N (len X)) (aset A (+ 1 (aref A N)) N))
	   (psg1 X))
     A))

;; (eql (HB (explod (i 20))) (! (cat 0 (i 20)) 20))

(defun HB1 (X) ; Distribution of size of subsets of X gives binomial coeffs
  (r '+ (o '= (i {0 (len X)}) (implod (mapcar 'len (psg1 X))))))

(defun part (s) ; Generate the partitions of set s
  (cond (s 
	  (let ((a (car s)) (b (part (cdr s))))
	    (append (mapcar '(lambda (x) (cons (list a) x)) b)
		    (mapcan '(lambda (x) (amplify a x)) b))))
	 (t '(()))))

(defun amplify (x s)
   (cond (s 
	  (let ((a (car s)) (b (amplify x (cdr s))))
	    (cons (cons (cons x a) (cdr s))
		  (mapcar '(lambda (y) (cons a y)) b))))
	  (t '(()))))


; Variations on generation of permutations

(defun GTUP (S N) 
 (cond ((or (not (nump N)) (not (listp S))) "GTUP: Bad args")
       ((le N 0) nil)
       ((let* ((#GS "N0000001")
	       (FP  ((label A lambda (X) 
			    (cond ((zerop X) nil) 
				  (t (cons (gensym) (A (- X 1))))))
		     N))
	       (Perm '(lambda  (F)
			(cond ((null F) (list 'list (append '(list) FP)))
			      (t (list 'mapcan 
				       (list 'quote (list 'lambda
							  (list (car F)) 
							  (Perm (cdr F))))
				       'S))))))
	  (eval (Perm FP))))))

(defun UTUP (S N) 
  ((label UTaux lambda (P)
	  (cond ((null P) nil)
		(t (cons (car P)
			 (UTaux (mapcan '(lambda (X)
					   (if (nequal (car P) X) (list X)))
					(cdr P)))))))
   (GTUP S N)))

(defun GENTUPLES (X) 
  ((label GTP 
	  lambda (X)
	    (cond ((null (cdr X))  (list X))
		  (t (mapcan 
		      '(lambda (Y) 
			 ((label R 
				 lambda (A X) 
				   (cond ((null X) nil)
					 ((cons (append X A)
						(R (append A (list (car X)))
						   (cdr X))))))
			  () (cons (car X) Y))) 
		      (GTP (cdr X))))))
   X))

; adapted  for sets from winston Lisp ed.2 

(defun PERMS (S SIZE) 
  (prog (PERMX PERMY (K 0))

	(defun PERMX (PERM)
	  (cond ((eq (len PERM) SIZE) (list (revl PERM)))
		(t (PERMY PERM S))))
	
	(defun PERMY (PERM M)
	  (cond 
	   ((null M) nil)
	   (t (cond ((memq (car M) PERM) (PERMY PERM (cdr M)))
;	   (t (cond (nil (PERMY PERM (cdr M)))
		    (t (nconc (PERMX (cons (car M) PERM))
			      (PERMY PERM (cdr M))))))))
	  (PERMX nil)))

(defun CNTEQ (A B)
  (prog ((N 0)) (mapc '(lambda (X) (cond ((equal A X) (incr N)))) B) N))

(defun CKGNTUP (X) "check number and uniqueness of ntuples of X"
  (let ((B (GENTUPLES X))) 
    (and (eq (! (len X)) (len B)) ; number of ntuples = !n
	 (genc '(lambda (Y) (onep (CNTEQ Y B))) B)))) ; genc t if all t

(defun TSRT (X) ; test sort using nutples 
  (let*  ((N (sort 'lt X)) 
	  (A (GENTUPLES N)) 
	  (B (mapcar '(lambda (X) (sort 'lt X)) A)))
    (eq (! (len X)) (CNTEQ N B))))

(defun PV (N) ; make permutation vector
  (let* ((#IO 0) (S (p N 0)) (I 0) (J 0) (K nil) L Carry
	(GEN '(lambda () 
		(if (null K) (a K t)
		  (cond ((lt (a I (+ I 1)) N) (aset S I 0))
			(t (a Carry t I 0 J 1) (aset S 0 0)
			   (while Carry 
			     (cond ((a Carry (eq (a L (+ (aref S J) 1)) N))
				    (aset S 0 J) (a J (+ J 1))
				    (cond ((eq J N) (a Carry nil))))
				   (t (aset S L J))))))) S)))
    (fun GEN)))
		       
(defun SymPV (V) (eql (i (p V)) (aref V (aref V $[]))))

(defun SPV (N)
  (let* ((V (PV N)) T (#IO 0) (R (i N))
	 (GEN '(lambda () (repeat (eql R (aref (a T (V)) (aref T $[])))) T)))
    (fun GEN)))

(defun TSPV (N) (let* ((S (SPV N)) (Z 1) (T (cat (S))) U)
		  (princl (fmt T Z:4))
		  (while (neql T (a U (S))) (princl (fmt U (incr Z):4))) Z))

(defun FS (V N) (let* ((R (i N)) (M (p N N)))
		  (repeat (eql R (aref V (aref V $[])))
			  (a V (enc M (+ 1 (dec N V))))) V))

(defun TSPV2 (N) (let* ((#IO 0) (R (i N)) (S R) (M (p N N)) (Z 1) 
			(FS '(lambda () 
			       (repeat (eql R (aref S (aref S $[])))
				       (a S (enc M (+ 1 (dec N S))))))S))
		   (a T (FS))
		   (princl (fmt S Z:4))
		   (while (neql T (FS)) 
		    (princl (fmt S (incr Z):4))) Z))


; Knuth TAOCP 7.2.1.2
(defun Ehrlich (N) ; Returns a permuation generator for perms of length N
  (prog* 
   ((#IO 0) (V (i N)) (B (i N)) (C (p N 0)) K T (Y 0) (Z 0)
    (SWAP '(lambda (V I J) 
	     (let ((T (aref V I))) (aset V (aref V J) I) (aset V T J))))
    (GEN 
     '(lambda () 
	(prog ()
	      (cond ((zerop Z) (incr Z) (return V))) ; first one is sym
	      (a K 1)
	      (repeat (lt (aref C K) K)
		      (cond ((eq K (aref C K)) 
			     (aset C 0 K) 
			     (if (eq N (a K (+ 1 K))) (return nil)))))
	      (aset C (+ 1 (aref C K)) K)
	      (SWAP V 0 (aref B K))
	      (a J 1 K (- K 1))
	      (repeat (ge J K)  
		      (cond ((lt J K) (SWAP B J K) (a J (+ J 1) K (- K 1)))))
	      V))))
	 (fun GEN)))


(defun P101 (N) 
  (let* ((#IO 0) (V (i N)) (L (cat (i N)  0)) (K 0) (U (p N 0)))
    (repeat (ge K N)
	    (a P (aref L 0)) (aset U P K) (aset L (aref L P) 0) (a K (+ K 1)))
    (print U) ))
    
 (defun SN (N) ; number of symmetric permutations of a vector of N elements
   (cond ((le N 1) 1) 
	 ((eq N 2) 2)
	 (t (+ (SN (- N 1)) (* (- N 1) (SN (- N 2)))))))

(defun SSN (N) 
  (if (le N 1) 1
    ((label F lambda (X Y Z) 
	    (cond ((eq Z N) X) (t (F (+ X (* Z Y)) X (+ Z 1))))) 2 1 2)))

(defun SSC (N) ; closed form from A Lehnen Madison college
  (let ((K (cat 0 (i (f (/ N 2))))))
    (r '+ (/ (! N) (* (! K) (! (- N (* 2 K))) (exp 2 K))))))

; Make list of permutations (MLPerm (Ehrlich 4))
(defun MLPerm (F)
  (let ((T (F))) (cond ((null T) nil) (t (cons (+ T) (MLPerm F))))))

;; Pairs from (i X) and (i Y)
(defun Apairs (X Y) (+ #IO (enc {X Y} (- (i (* X Y)) #IO))))

;; Unique pairs from (i X) and (i Y) APL style
(defun UApairs (X Y)
  (let ((A (+ #IO (enc {X Y} (- (i (* X Y)) #IO)))))
    (k (r '<> 1 A) A)))

;; Unique pairs ingoring order
(defun UUApairs (X Y)
  (let* (B C (A (+ #IO (enc {X Y} (- (a C (i (a B (* X Y)))) #IO)))))
    (k (r '+ (^ (o '< C C) (o '= (dec B A) (dec B (rev 1 A))))) A)))

;; Hack for unique pairs ingoring order from (i N) and (i N)
(defun HUUApairs (N)
  (let ((A (+ #IO (enc {N N} (- (i (* N N)) #IO)))))
    (k (k (rav (cat 1.5 (rev (i (- N 1))) (- (i (- N 1))))) 1)
       (k (r '<> 1 A) A))))

;; Constructive approach
(defun CUApairs (N)
  (cat .5 (k (- N 1) (i N))
       (k (p (* N N) (cat 0 (p N 1))) (p (* N N) (i N)))))

(defun CUUApairs (N)
  (cat .5 (k (i {(- N 1) 1}) (i (- N 1)))
       (k (rav (o '< (i N) (i N))) (p (* N N) (i N)))))

(defun TUUpairs (N)
  (FOR I 2 N (if (not (and (eql (UUApairs I I) (HUUApairs I))
			   (eql (UUApairs I I) (CUUApairs I))))
		 (error (fmt "TUUpairs: failed at " I)))))

;APL way of generating combinations
(defun AC (N) (let ((#IO 0)) (enc (p N N) (i (exp N N)))))

(defun TCOMB (N) ;; check APL vs LISP
  (let ((A (explod (i N))))
    (eql (Apairs N N)
	 (implod 1.5 (mapcar 'implod  (COMB A A))))))

(defun TUCOMB (N) ;; check APL vs LISP
  (let ((A (explod (i N))))
    (eql (UApairs N N)
	 (implod 1.5 (mapcar 'implod  (UCOMB A A))))))
		   
(defun APN (N J) ;  Jth lexical perm of (i  N) based on D. N. Lehmer [1906]
  (let ((Z (cat 1)) (C (+ 1 ( enc (rev (+ 1 (i (- N 1)))) J))) (I N))
  (while (not (zerop (a I (- I 1))))
    (a Z (cat (aref C I) (+ Z (>= Z (aref C I))))))))

(defun API (P) ; index of P in lexicographical perms of (i (p P))
  (prog ((N (p (a P (cat P)))) (Z []))
	(while (lt 1 (p P))
	  (a Z (cat Z (tk 1 P))
	     P (dp 1 (- P (> P (tk 1 P))))))
	(dec (rev (+ 1 (i (- N 1)))) (- Z 1))))

(defun APNA (N J) ; Returns the J-th lexical permutations of (i N)
;; N must be a scalar, but J may have any shape.  (p Z) <=> (cat (p J) N)
;; From an algorithm by D. N. Lehmer [1906])
(let ((Z (p (cat 1 (r '* (p J))) 1)) 
      (C (+ 1 (enc (rev (+ 1 (i (- N 1)))) (cat (tr J)))))
      (I N))
  (while (not (zerop (a I (- I 1))))
    (a Z (cat (aref C I ()) (+ Z (>= Z (p (p Z) (aref C I ())))) 1)))
  (tr (p (cat (tally  Z) (rev (p J))) Z))))

(defun APIA (P)
;; Returns the lexical indices of permutations P
;;  P may be a vector containing a single permutation, or an array
;;  having a permutation in each row.  (p result) <=> (dp -1 (p P))
  (let* ((S (p P)) 
	 (P (tr (p (cat (r '* (dp -1 S)) (tk -1 (cat 1 S))) P)))
	(Z (p (* [0 1] (p P)) 0)))
    (while (lt 1 (tally  P))
      (a Z (cat Z (aref P 1 ()) 1)
	 P (dp [1 0] (- P (> P (p (p P) (aref P 1 ())))))))
    (p (dp -1 S)  (dec (rev (+ 1 (i (+ -1 (tk -1 (cat 1 S)))))) (- Z 1)))))

;; More permutation stuff from Iverson's Notation as a tool of thought sec 3.3
;; Is P a permutation of its indeces ?
(defun IsaPerm (P) (onep (r '^ (r '+ (o '= P (i (p P)))))))

;; Binary representation from direct representation of a permutation P
(defun BfDPerm (P) (o '= P (i (p P))))

;; Direct representation from binary representation of a permutation P
(defun DfBPerm (P) (. '+ '* P (i (tally  P))))

;; Direct from cycle
(defun DfCPerm (P) (let ((X (= P (s 'f P))))
		     (aref (aref P (gup (+ X (s '+ X)))) (InvPerm P))))
;(DfCPerm (a C [7 3 6 5 2 1 4]) => [4 2 6 1 3 5 7]

;; produce matrix where each column is  subscripted by previous column
(defun PowerCycle (D) (cond ((onep (r '<= (p D))) D) 
			    (t (PowerCycle (cat (aref D nil 1)
					      (aref (aref D nil 1) D))))))

;; Cycle from direct (if you are still following...)
(defun CfDPerm (D) 
  (prog (M
	 (ssr '(lambda (w) (k 1 (r '^ (= (a M (s 'f w)) (rot 1 M))) w)))
	 (dol '(lambda (w) (aref w (gdn (aref w nil 1)) nil)))
	 (cir '(lambda (w) (k (rav (cat 1 (s '^ (dp [0 1] (<> w (s 'f w))))))
			      (rav w)))))
	(cir (dol (ssr (PowerCycle (tr D)))))))
;(CfDPerm (DfCPerm [7 3 6 5 2 1 4])) => [7 3 6 5 2 1 4]

;; Radix representation
(defun RRPerm (N) (+ 1 (enc (rev (i N)) (+ -1 (i (! N))))))


(defun Nub (V) (k (r 'v (rank V) (as '< (o '= V V))) V))

(defun as (F A)
  (let ((S (dp -1 (p A))) (R (tk -1 (p A))))
    (implod (if (gt (rank A) 1) 1.5)
	    (mapcar '(lambda (X) (ar F X))
		    (mapcar '(lambda (Y) (tk {S Y} A))
			    (explod (i R)))))))

(defun ar (F X) 
  (prog ((R (F)) T)
	(while (not (zerop (r '^ (p X)))) ; Check this
	  (a R (F (tk -1 X) R))
	  (a X (dp -1 X))) R)) 

(defun PC (N F) ; Generate the set of involutions of length N
  (let* ((#IO 0) (P (i N)) (C 0) R
	 (swap '(lambda (A B) 
		  (let ((T (aref P A))) (aset P (aref P B) A) (aset P T B))))
	 (SSCSS '(lambda (A B)
		   (swap A n) (swap B (+ n 1)) 
		   (cycle (+ n 2)) 
		   (swap B (+ n 1)) (swap A n)))
	 (cycle '(lambda (n) 
		   (if (ge n N) (let ((P (+ 1 P))) (incr C) (F P))
		     ;; Do the 1 cycle part of the recurrence
		     (cycle (+ n 1))
		     ;; See if we can process 2-cycles
		     (when (ge (- N n) 2) 
		       ;; Make a 2 cycle...
		       (swap n (+ n 1))
		       (cycle (+ n 2))
		       ;; Loop through and replace ....
		       (FOR i  (- n 1) DOWNTO 0
			    (if (eq (aref P i) i) (SSCSS i n) 
			      (SSCSS (aref P i) i)))
		       (swap n (+ n 1)))))))
		
    (if (null F) (a F showit))
    (cycle 0)
    C ))


(defun So1 (P) ; Prints cycle notation of P given in direct form
  (prog* ((#IO 1) (R) (Cyc (CfDPerm P))
	  (K (= Cyc (s 'f Cyc)))
	  (L (~ (^ (cat (dp 1 K) 1) K))) ; Eliminate single cycles
	  (C (explod (k L K)))
	  (D (explod (k L Cyc))))
	 
	 (mapc '(lambda (C D)
	   (cond ((onep C) (a R (append R (list (list D)))))
		 (t (nconc (car (last R)) (list D)))))
	       C D)
	 (prin0 P) (princ "   ")  (print R) ))

(defun SSM (P) ; used with PC checks that representation changes are equivalent
  (prog ((#IO 1)) (prin0  P) (print (eql  P (DfCPerm (CfDPerm P)))) ))
