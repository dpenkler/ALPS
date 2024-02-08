; -*- mode: lisp; -*-
(require 'genset)
(defun AVL (L) ; Adel'son-Vel'skin and Landis
   (prog 
      (ROOT mkNode showNode insNode addNode (Key 'cadr) (Bal 'cddr) 
       (Left 'caar) (Right 'cdar) setBal (one 1) (zero 0) (m1 -1) N Adj mAdj) 
      (defun setBal (N B) (rplacd (cdr N) B)) ; Balanced Short Long
      (defun adjLink (N Adj) (cond ((onep Adj) (Right N)) 
				   (t          (Left N))))
      (defun setAdjLink (N Adj Sub) 
	(cond ((onep Adj) (rplacd (car N) Sub))
	      (t          (rplaca (car N) Sub))))
      (defun mkNode (K B L R) (acons L R (cons K B))) 
      (defun depth (N) 
	(cond ((null N) 0) 
	      (t (let ((DL (+ 1 (depth (Left N) )))
		       (DR (+ 1 (depth (Right N)))))
		   (if (lt DL DR) DR DL)))))
      (defun showNode (S N D) 
	(princ (fmt "Key " (Key N):6 " Bal " (Bal N):2
		    "; Depth " (depth N) " , " D 
		    " LD " (depth (Left N)) " RD " (depth (Right N)) 
		    " " S "\n")))
      (defun showTree (S N D) 
	(cond ((not (null N))
	       (showTree 'Left (Left N) (+ D 1)) 
	       (showNode S N D)
	       (showTree 'Right (Right N) (+ D 1)))))
      (defun insNode (K P S) 
         (prog ((N (S P)))
            (cond ((null N) (addNode (mkNode K 'B nil nil) P S))
		  ((equal K (Key N)) N)
		  ((lt K (Key N)) (insNode K N 'Left)) 
		  (t (insNode K N 'Right)))))
      (defun InsNode (K BNP S) 
	(prog
	 (BN        ; balance node
	  (OP BNP)  ; Old P
	  (P (S BNP)) ; Rover
	  )
	 (a BN P)
	 (while P (cond ((equal K (Key P)) (return P))
			((lt K (Key P)) (a S 'Left))
			(t              (a S 'Right)))
		(cond ((a N (S P))
		       (cond ((not (zerop (Bal N))) (a BNP P BN N)))))
		(a OP P P N))
	 (a N (addNode (mkNode K zero nil nil) OP S))
	 (cond ((null BN) (return N)))
	 (cond ((lt K (Key BN)) (a Adj m1  mAdj one P (Left  BN)))
	       (t               (a Adj one mAdj  m1 P (Right BN))))
	 (a OP P)
	 (while (ne P N) ; adjust balance indicators between BN and N
	   (cond ((lt K (Key P)) (setBal P m1)  (a P (Left P)))
		 (t              (setBal P one) (a P (Right P)))))
	 (cond ((zerop (Bal BN))   (setBal BN Adj)  (return N))
	       ((eq (Bal BN) mAdj) (setBal BN zero) (return N))
	       (t ;(eq (Bal BN) Adj)
		(cond 
		 ((eq (Bal OP) Adj) ; single rot
		  (a P OP)
		  (setAdjLink BN  Adj  (adjLink OP mAdj))
		  (setAdjLink OP mAdj BN)
		  (setBal BN zero) (setBal OP zero))
		 (t (a P (adjLink OP mAdj)); double rot
		    (setAdjLink OP mAdj (adjLink P Adj))
		    (setAdjLink P   Adj OP)
		    (setAdjLink BN  Adj  (adjLink P mAdj))
		    (setAdjLink P mAdj BN)
		    (cond ((eq (Bal P) Adj)  (setBal BN mAdj) (setBal OP   0))
			  ((eq (Bal P) mAdj) (setBal BN    0) (setBal OP Adj))
			  (t                 (setBal BN    0) (setBal OP   0)))
		    (setBal P 0)
		    ))
		(cond ((eq BN (Right BNP)) (rplacd (car BNP) P))
		      ((eq BN (Left  BNP)) (rplaca (car BNP) P)))))
	 ))
      (defun addNode (N P S) 
         (cond ((eq S 'Left) (rplaca (car P) N)) (t (rplacd (car P) N)))) 
      (a ROOT (mkNode 'Root zero nil nil))
      (mapc '(lambda (X) (InsNode X ROOT 'Right)) L)
;      (showTree 'Right (Right ROOT) 0)
      (depth (Right ROOT))))

(defun Trees (N)
  (r '+ (o '= (i N) (implod (mapcar AVL (GENTUPLES (explod (i N))))))))

(defun Strees (N)
  (r '+ (o '= (i N) (implod (mapcar AVL (mapcar '(lambda (X) (explod (? X X))) 
						(explod (p (exp 2 N) N))))))))

(defun TAVL (N) ; Test AVL
  (princl (fmt" I" "Num Elts":10:1 "Depth":6:1  "millisecs":11:1))
  (FOR I 3 N 
       (let* ((T (- (exp 2 I) 1)) 
	      (L  (explod (i T)))
	      (T1 (time))
	      (R  (AVL L))
	      (TT (* 1000 (aref (- (time) T1) 1))))
	 (princl (fmt I:2 T:10 R:6 TT:11:2)))))

(defun SAVL (N) (FOR I 1 N (princl (fmt I:3 " " (Strees I)))))
