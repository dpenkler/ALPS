;;;-*- mode: emacs-lisp -*-
;;;
;;; Exponentials
;;;
(defun g (F X N) 
  (let ((#IO 1) (Y (p N X)))
    (FOR I 2 N (aset Y (F (aref Y (- I 1))) I))
    Y))

(defun E (X) 
  (let* ((#IO 0)
;	 (L (g '(lambda (X) (/ X 2)) 1 32))
;	 (M (/ (- (g 'sqrt 10 32) 1) L)))
	 (L (exp .5 (i 32)))
	 (M (exp 10 L))
	 (N (/ (- M 1) L)))
    (exp 10 (/ X (tk -1 N)))))

(defun Briggs (X) ; Calculate log x using Briggs' method
  (cond 
   ((le X 0) (error "Briggs: arg must be > 0"))
   ((lt X 1) (- (Briggs (/ X))))
   (t 
    (prog* ((#IO 0) (Lim 40)
	    (L (exp .5 (i (+ 1 Lim)))) ; successive squares of .5
	    (M (exp 10 L))       ; 10 to the power of successive squares of .5
	    (N (/ (- M 1) L))    ; proportion of fractional part of M to L
	    (P (tk -1 N))        ; last proportion element
	   ; L is log M by construction (defun log (X) (l 10 X))
	   ; The differences between the successive values of N become
	   ; smaller and smaller so we use the last entry P to obtain
	   ; an approximation to (log S)
	   ; based on observation that for small S
	   ; (exp 10 S) is roughly equal to (+ 1 (* P S))
	   ; i.e. S is roughly equal to (log (+ 1 (* P S)))
	   ; so if we assume our last factor X to be equal to (+ 1 (* P S))
	   ; S is then simply (/ (- X 1) P)
	   ; P is also close to (ln 10) 
	   ; (exp 1) ~ (exp 10 (/ P)
	    U V (R 0))
	   (while t
	     (a V (tk 1 (k (> X M) M)) ; biggest in M smaller tha X
		U (ind M V))   ; U is index of nearest entry in M to X
;	   (print (fmt "V is "  V " X " X " => " (/ X V)))
	     (if (zerop V)
		 (return (+ R (/ (- X 1) P)))) ; add approximation
	     (a R (+ R (aref L U)) ; cumulate corresponding log and
		X (/ X V)))))))    ; divide X by the M entry
	   
	     
(defun Eaprox () ; approximating e with L and M
  (prog* ((#IO 0) (Lim 40)
	  (L (exp .5 (i (+ 1 Lim)))) ; successive squares of .5
	  (M (exp 10 L))       ; 10 to the power of successive squares of .5
	  (N (/ (- M 1) L))    ; proportion of fractional part of M to L
	  (P (tk -1 N))        ; last proportion element
	  (X (/ P))            ; (eql (exp 10 X) (exp 1)) => t
	  (R 1)
	  V)
	 (while t
	   (a V (tk 1 (k (> X L) L)))    ; largest L smaller than X
	   (if (zerop V) (return R))      
	   (a R (* R (aref M (ind L V))))  ; multiply by 10 to the power of V
	   (a X (- X V)))))              ; reduce x by V

(defun TB () ; Testing Briggs
  (let ((R 0) (K 10000))
    (FOR I 1 K
	 (a R (+ R (| ((lambda (X) (- X (exp 10 (Briggs X))))
		       (/ (? K) (sqrt K)))))))
    R)) ; sum of absolute errors


(defun EXP (X N) (let* ((#IO 0) (J (i N))) (r '+ (/ (exp X J) (! J)))))

(defun SIN (X N) ; calculate sin(x) using N terms of the expansion
  (let* ((#IO 0) (M (cat (p X) N)) (J (+ (* 2 (i N)) 1)))
    (r '- -1 -1  (/ (o 'exp X J) (p M (! J))))))

(defun COS (X N) ; calculate cos(x) using N terms of the expansion
  (let* ((#IO 0) (M (cat (p X) N)) (J (* 2 (i N))))
    (r '- -1 -1 (/ (o 'exp X J) (p M (! J))))))


(defun ANG (N P) ; generate 2N+1 angles in radians from [-pi..pi]*P
         (let ((#IO 0)) (* (% P) (/ (- N (i (+ 1 (* 2 N)))) N))))

(defun msin (X) 
  (prog ((T  X) (Y (- (sqr X))) (J 3) (OX 0))
	(while (ne X OX)  
	  (print (list T J X))
	  (a T (* T (/ Y (* J (- J 1)))))
	  (a J (+ J 2))
	  (a OX X X (+ T X))) X))

(defun mcos (X) 
  (prog ((T  1) (Y (- (sqr X))) (J 2) OX)
	(a X 1 OX 0)
	(while (ne X OX)
	  (print (list T J X))
	  (a T (* T (/ Y (* J (- J 1)))))
	  (a J (+ J 2))
	  (a OX X X (+ T X)))
	X))

(defun TTEXP (M N) (r '+ (+ (sqr (SIN (ANG M 1) N))
			    (sqr (COS (ANG M 1) N)))))

(defun TTT (M) (prog (R) ;; (TTT 360)
 ;;; find number of terms for convergence on M divisions of the circle
  (FOR I 1 100 
       (if (eq (+ 1 (* 2 M)) (a R (TTEXP M I))) (return 'ok)
	 (princl (fmt "# terms" I:3 " Conv " (sqr (- R (* 2 M))):12:7))))
  'bork))

(require 'trig)
(de me (x y)
    (let* ((a (re x)) (b (im x)) (c (re y)) (d (im y))
	   (s1 (+ (sqr a) (sqr b)))
	   (a1 (% 12 x)) ;; (arg x)
	   (s2 (+ (* d .5 (l s1)) (* c a1))))
      (print (list a b c d s1 a1 s2))
      (* (exp s1 (/ c 2)) (exp (* (- d) a1)) (+ (cos s2) (* 0j1 (sin s2))))))

; interest Principal Rate Term NumberOfConversionPeriods
(defun Intf (P R T N) (- (* P (exp (+ 1 (/ R N)) (* N T))) P))

(defun Inti (P R T N) (let ((V (p N (* P (/ R N)))))))

(defun Intl (P R T N) (let ((V  0))
			(FOR I 1 (* T N) (a V (+ V (* (+ P V) (/ R N))))) V))

; TimeToFutureValue Principal FutureValue Rate NoConvPeriods
(defun TTFV (P FV R N) (/ (l (/ FV P)) (l (+ 1 (/ R N))))) 
