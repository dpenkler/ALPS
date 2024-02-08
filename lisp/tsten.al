;;
;; Test for stencil using cgol
;;

;;
;; Prototype stencil implementation for conversion into 'C'
;;

(defun stencil (Fun Shape Arg)
  (if (neql (rank Arg) (p Shape)) (error "Rank of Arg must eql shape of Shape"))
  (let* ((#IO 0)
	 (RA (rank Arg))              ;; rank of Arg
	 (SA (p Arg))                 ;; shape of Arg
	 (BI (p RA 0))                ;; base index for result
	 (PR  (- SA (~ (| Shape 2)))) ;; initial shape of result
	 (Noels (r '* PR))            ;; Number of elements in result
	 (R 0)                        ;; Result element counter
	 (Res (p PR []) )             ;; Result
	 (SI (mapcar '(lambda (X) (i X)) (explod Shape)));;Stencil index vectors
	 (DeltaD  (int (* (> Shape 2) (/ (+ 1 Shape) 4)))) ;; half delta dim
	 (PMA (+ SA (* 2 DeltaD)))    ;; Shape of padded arg
	 (mArg (p (int PMA) 0))       ;; padded argument
	 (BumpBI '(lambda (Carry)     ;; increment base index by N 'C' version
		    (let ((Ind (- RA 1)) T)
		      (a R (+ R Carry))
		      (while (not (zerop Carry))
			      (aset BI (a T (+ Carry (aref BI Ind))) Ind)
			      (if (lt (aref BI Ind) (aref PR Ind)) (a Carry 0)
				(aset BI (| (aref PR Ind) T) Ind)
				(a Carry (int (/ T (aref PR Ind))))
				(decr Ind)))) R))
	 (QBumpBI '(lambda (N) (a BI (enc PR (a R (+ N R)))) R))  ;; APL version
	 (PadInd '(lambda () (+ (* (<  (- BI DeltaD) 0) (|(- BI DeltaD)))
				(* (>= (+ BI DeltaD) SA) (- SA 1 (+ BI DeltaD))))))
	 (MkStencil '(lambda () (apply 'aref mArg (mapcar '(lambda (X Y) (+ X Y))
							  SI (explod BI)))))
	 (Arity '(lambda (F) (cond ((ne (car F) 'lambda) (error "expected lambda expression"))
				   ((len (cadr F))))))
	 Exp
	 )
    ;; Copy Arg into padded array mArg
    (apply 'aset mArg Arg (mapcar '(lambda (O X) (+ O (i X)))
				  (explod (/ (- (p mArg) SA) 2))
				  (explod SA)))
    ;; Apply Fun to successive stencils in mArg storing the result in Res
    (cond ((eql (Arity Fun) 1) (a Exp '(Fun (MkStencil))))
	  ((eql (Arity Fun) 2) (a Exp '(Fun (PadInd) (MkStencil))))
	  (t (error "Function not monadic or dyadic")))
    (repeat
     (eql (QBumpBI 1) Noels)
     (iset Res (eval Exp) (vtr BI)))
    Res
    ))

(a A (p [3 20] [ 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 0 
		 0 1 0 0 1 1 0 0 1 1 1 0 0 0 1 0 1 0 1 0 
		 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 1 
	  ]))

(de box (X) (let ((P (tk 1 (p 0 X)))) (cat 1 P (cat 1 (cat P (cat X P)) P))))

(de unbox (X) (dp [1 1] (dp [-1 -1] X)))

(de om (K);;  apl neighbour count calculation using subscripting 
    (let* ((#IO 1) (PK (p K)) (R PK[1]) (C PK[2])
	   (IR (i (- R 1))) (IC (i (- C 1)))
	   (RA {R IR}) (RB {(+ 1 IR) 1}) (CA {C IC}) (CB {(+ 1 IC) 1}))
      (+ K{RA:} K{RB:} K{:CA} K{:CB} K{RA:CA} K{RA:CB} K{RB:CA} K{RB:CB})))

(de cm (K);; classic apl neighbour count calculation
    (let (K1 K2)
      (+ (shft 1 K)  (shft -1 K)  (a K1 (shft 1 1 K))  (a K2 (shft 1 -1 K))
	 (shft 1 K1) (shft -1 K1) (shft 1 K2)          (shft -1 K2))))

(de sm (K) (- (r '+ 1 3 (r '+ 2 3 (box K))) K)) ;; Using N-wise reduction N=3

(de xm (K) (- (stencil '(lambda (w) (r '+ (rav w))) [3 3] K) K)) ;; Use stencil
;;
;; equivalent to xm in dyalog APL using stencil: ({+/,⍵}⌺3 3⊢A)-A
;; 

(defconst GLDR (getf "data/gldr"))

(defun gol (F M N) (clr) (princ M) ; conways game of life for N generations
  (let* ((K (= M '*)) K1 K2 L (#IO 0))
    (FOR H 1 N   (wait .01)
	 (setq L (F K))
	 (prat 1 1) (princ (aref " *" (setq K (v (= L 3) (^ (= L 2) K))))))))

(de tgol (N)
    (let ((fns '(sm om cm xm)) K (M (getf "data/gldr")) T0 T1)
      (mapc '(lambda (F) (a K (= M '*)) (a T0 (time))
	       (FOR I 1 N (a L (F K) K (v (= L 3) (^ (= L 2) K))))
	       (a T1 (time))
	       (princl (fmt F "  " (aref (- T1 T0) 1))))
	    fns)
      'ok))

(de tst (N)
    (FOR I 1 N
	 (princl (fmt "\n" I " dimension" (if (onep I) "" "s")":\n"))
	 (print (stencil '(lambda (w) (r '+ (rav w))) (p I 3) (p (p I 3) 1)))))
