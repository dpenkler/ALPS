(require 'prime)

(defun genKeys(R) 
  (let* ((RP (RanP R))
	 (P (aref RP 1))
	 (Q (aref RP 2))
	 (N (* P Q))
	 (PHI (* (- P 1) (- Q 1)))
	 (E (let ((V (- PHI 2)))
	      (if (evenp V)  (setq V (- V 1))) ; E odd & smaller than PHI
	      (while (gt (gcd V PHI) 1) (setq V (- V 2))) V))
	 (D (minv PHI E)))
 ;   (print (list P Q PHI D E N))
    (list D E N PHI P Q R)))
;key selectors
(defun PVT1 (X) (car X))
(defun PVT2 (X) (caddr X))
(defun PUB1 (X) (cadr X))
(defun PUB2 (X) (caddr X))
(defun PHI (X) (car (cdddr X)))

; Extended GCD algorithms

(defun EGCDM (A B) (let ((M (p [3 2] {1 0 0 1 A B})) (Q (p [2 2] [0 1 1 0])))
		   (while (gt B 0)
		     (aset Q (- (f (/ A B))) 2 2)
	 	     (setq M (. '+ '* M Q))
		     (setq A (aref M 3 1) B (aref M 3 2))
;		     (print (list A B))
		     )
		   (aref M 2 1)))

(defun EGCDR (A B) 
  (cond ((zerop (| B A)) (cons 0 1))
	(t (let* ((T (EGCDR B (| B A))) (X (car T)) (Y (cdr T)))
	     (cons Y (- X (* Y (f (/ A B)))))))))

(defun minv (N M) 
  (let ((P 0) (P1 1) (Q (f (/ N M))) (Q1 0) (R (| M N)))
  ;  (print (list M N R P1 P Q1 Q))
    (setq Q1 (f (/ M R)) T R R (| R M) M T) 
    (while (not (zerop R))
;      (print (list M N R P1 P Q1 Q))
      (setq T (| N (- P (* P1 Q))) P P1 P1 T Q Q1)
      (setq Q1 (f (/ M R)) T R R (| R M) M T))
 ;   (print (list M N R P1 P Q1 Q))
    (if (onep M) (| N (- P (* P1 Q))) (print (list "No inverse" P)))))
     
(defun TEGCD (A B) (list (cdr (EGCDR A B)) (minv A B) (EGCDM A B)))

; Modular exponentiation

(defun C (Symb Key1 Key2) (Caux Symb Key1 1)) 
(defun Caux (P J R) ; (print (list P J R))(getc)
  (cond ((onep J)  (| Key2 (* R P)))
	((evenp J) (Caux (| Key2 (* P P)) (/ J 2)       (| Key2 R)))
	 ( t       (Caux (| Key2 (* P P)) (/ (- J 1) 2) (| Key2 (* R P))))))
	 
(defun TME () (let (X) ; test modular exponentiation
     (FOR I 1 50 (setq X (+ 1 (* 2 I))) 
	  (FOR J 2 64
	       (if (eq (C 2 J X) (| X (exp 2 J))) ()
		 (print (list "failed at" 2 J X)))))))

(defun rsc (S K1 K2) ; encode / decode
  (let ((T (p (p S) 0)))
    (FOR I 1 (p S) (aset T (C (aref S I) K1 K2) I)) T))


(defun TRSA (R) 
  (let* ((K (genKeys R))
	 (T "hello polly")
	 (S (chr (rsc (rsc (num T) (PVT1 K) (PVT2 K)) (PUB1 K) (PUB2 K)))))
    (if (equal S T) t (print (list S K T)) (break))))

 
