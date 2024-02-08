; -*- mode: emacs-lisp; -*-

; Baking PI
; Ref [1] http://mathworld.wolfram.com/PiFormulas.html

(defun GPI (N M) (let ((P (p N M))) ; Monte Carlo sim
  (* 4 (/ (r '+ (<= (dist (? P) (? P)) M))  N))))

(defun NPI (N) ; brute force grid
  (let* ((B (+ (* 2 N) 1)) (A (i {(- N) N})))
    (* 4 (/ (r '+ (rav (<= (o 'dist A A) N))) (sqr B)))))

(defun QPI (N) ; quarter grid
   (let* ((A (i  N)))
     (* 4 (/ (r '+ (rav (<= (o 'dist A A) N))) (sqr N)))))

(defun BPI (N) ; Alex's cunning interative slink around the edge method
  (let ((X N) (Y 1) (numIn 0))
    (while (lt Y N)
      (cond ((gt (dist X Y) N) (decr X)) (t  (a numIn (+ numIn X)) (incr Y))))
    (* 4 (/ numIn (sqr N)))))

(defun Arch (M)  ; Archimedes method of exhaustion approximating the
  (* 3 (exp 2 M) ; circumference of a circle by chopping up an hexagon 
     ((label SR lambda (N)
	     (cond ((zerop N) 1)
		   (t (let ((S1 (/ (SR (- N 1)) 2)))
			(dist S1  (- 1 (sqrt (- 1 (sqr S1))))))))) M)))

(defun Wallis (N) ;John Wallis Arithmetica Infinitorum 1655
  (* 8 (r '* (/ {(exp (* 2 (+ 1 (i (- N 1)))) 2) (* 2 (+ N 1))}
		(exp (+ 1 (* 2 (i N))) 2)))))  
    
(defun LWB (N) ; Lord William Brouncker apud Wallis
  (* 4 (/ (+ 1 
	     ((label C lambda (X) 
		     (cond ((gt X N) 1)
			   (t (/ (sqr (- (* 2 X) 1)) 
				 (+ 2 (C (+ X 1))))))) 1)))))

(defun LPI (N) ; Leibniz - Gregory formula
  (* 4 (r '- -1 -1 (/ (i {1 2 (* 2 N)})))))

(defun W14PI (N) (sqrt (* 6 (r '+ (/ (sqr (i N))))))) ; [1] Eqn 14 (Euler)

(defun W16PI (N) (sqrt (* 8 (r '+ (/ (sqr (i {1 2 (* 2 N)}))))))) ; [1] Eqn 16

(defun W27PI (N) ; Gosper x=1/2 [1] Eqn 27
  (* 9 (/ (* 2 (sqrt 3)))
     (r '+ (/ (sqr (! (i {0 N}))) (! (i {1 2 (+ 1 (* 2 N))}))))))

(defun W28PI (N) ;  x=sin(PI/10) [1] Eqn 28
  (* (/  (* 5 (sqrt (+ GR 2))) 2)
     (r '+ (/ (sqr (! (i {0 N}))) 
	      (* (exp GR (i {1 2 (+ 1 (* 2 N))}))
		 (! (i {1 2 (+ 1 (* 2 N))})))))))

(defun W30PI (N) ; BBP Formula [1] Eqn 30
  (let ((N8 (* 8 (i {0 N}))))
    (r '+ (/ (- (/ 4 (+ 1 N8)) (/ 2 (+ 4 N8)) (/ (+ 5 N8)) (/ (+ 6 N8)))
	     (exp 16 (i {0 N}))))))
;
; Digit generation with BB Plouffe
;
(defun ModPow (B N K)  ; (B^N) mod K
   (let ((R 1))
     (while (gt N 0)
       (if (oddp N) (a R (| K (* R B))))
       (a N (f (/ N 2)))
       (a B (| K (sqr B))))
     R))

(defun Frac (X) (| 1 X))

(defun FDD (J D) 
  (let ((R 0) T) 
    (FOR K 0 D 
	 (a T (+ J (* 8 K))) 
	 (a R (+ R (/ (modexp 16 (- D K) T) T )))) R))

(defun FDD1 (J D) ; brute force without modular exponentiation
  (r '+ (/ (exp 16 (i {D 0})) (+ J (* 8 (i {0 D}))))))

(defun LDD (J D)  (r '+ (/ (* (exp 16 (i 15)) (+ J (* 8 (+ D (i 15))))))))

(defun SJ (J D) (Frac (+ (Frac (FDD J D)) (LDD J D))))

(defun SJC (J D) (+ (FDD J D) (LDD J D))) ; Check

(defun MKD (D) (Frac (- (* 4 (SJ 1 D)) (* 2 (SJ 4 D)) (SJ 5 D) (SJ 6 D))))

(defun MKDC (D) (- (* 4 (SJC 1 D)) (* 2 (SJC 4 D)) (SJC 5 D) (SJC 6 D))) ;check
; (MKDC 0) should give a first approc to Pi

(defun PRF (V)
  (if (lt V 0) (a V (+ V 1)))
  (FOR I 1 10 (print (f (* V 16))) (a V (Frac (* V 16)))))

(defun MKPIDIG (S N) ; Make N Pi digits starting from S
  (let ((A (p N 0)) B M 
	(L 5) ; nine digits at a time with IEEE754 seems to work for first 500
	      ; five needed for 1e6+ digs
	(I 0) (#IO 1))
    (while (lt I N)
      (a B (MKD S))
      (if (lt B 0) (incr B))
;      (if (or (lt B 0) (ge B 1)) (error "Value not in [0 1)"))
      (a M  (cond ((le (+ L I) N) L) (t (- N I))))
      (FOR J 1 M (aset A (f (* B 16)) (+ I J)) ( a B (Frac (* B 16))))
      (a I (+ I M) S (+ S M)))
    A))

(defun TDGEN (N) ; test digit generator
  (let ((A (MKPIDIG 0 N))) (+ 3 (r '+ (/ A (s '* (p (p A) 16)))))))

(a HEXLIST "\
243F6A8885A308D313198A2E03707344A4093822299F31D008\
2EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC\
29B7C97C50DD3F84D5B5B54709179216D5D98979FB1BD1310B\
A698DFB5AC2FFD72DBD01ADFB7B8E1AFED6A267E96BA7C9045\
F12C7F9924A19947B3916CF70801F2E2858EFC16636920D871\
574E69A458FEA3F4933D7E0D95748F728EB658718BCD588215\
4AEE7B54A41DC25A59B59C30D5392AF26013C5D1B023286085\
F0CA417918B8DB38EF8E79DCB0603A180E6C9E0E8BB01E8A3E\
D71577C1BD314B2778AF2FDA55605C60E65525F3AA55AB9457\
48986263E8144055CA396A2AAB10B6B4CC5C341141E8CEA154")

(a HEXL1e6 "6C65E52CB459350050E4BB1")

(defun HDGCHK () ; Hex digit generator check
	(if (equal (hexdec (MKPIDIG 0 (p HEXLIST))) HEXLIST)
	    "Pi baking test passed"
	  (error "Pi Baking Failed")))

(defun TRPI () ;TotallY Ridiculous way of generating first 3 digits [1] Eqn 132
  (r '+ (/  (* [100 10 1] (| 10 (+ [1 5 9] [2 6 5]))) 100)))

(defun TPI (N) ; the great pi baking contest
  (let 
      ((Contestants 
	'(NPI QPI BPI Arch Wallis LWB LPI 
	      W14PI W16PI W27PI W28PI W30PI TDGEN)))
    (mapc '(lambda (F) (princl (fmt F:8 (sqr (- (% 1) (F N)))))) Contestants)))

(defun TPIN (N R)
  (princl "Lowest Score is best")
  (let  (T1 (Contestants 
	  '(NPI QPI BPI Arch Wallis LWB LPI 
	      W14PI W16PI W27PI W28PI W30PI TDGEN)))
    (mapc '(lambda (X) (princl (fmt (car X):8 (cadr X))))
	  (sort '(lambda (X Y) (lt (cadr X) (cadr Y)))
	   (mapcar '(lambda (F) 
		      (a T1 (time))
		      (FOR I 1 R (F N))
		      (list F (* (+ 1e-70 (sqr (- (% 1) (F N))))
				 (aref (- (time) T1) 1))))
		   Contestants)))))
