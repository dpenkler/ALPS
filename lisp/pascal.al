;;;-*- mode: emacs-lisp -*-
(require 'idioms)
(require 'dfns)

(defun Pascal (N) "Print Pascal's triangle for N+1 rows"
  (let* ((P 1) (H (/ N 2))
	 (FW (+ 1 (c (l 10 (+ 1 (! H N))))))  (T 39))
    (princ (fmt " ":T (fmt P):FW:0 "\n"))
    (FOR J 1 N
	 (a T (- T (/ FW 2)))           ;; indentation
	 (a P (+ (cat 0 P) (cat P 0)))  ;; generate row
	 (princ (fmt " ":T))
	 (FOR I 1 (p P) (princ (fmt (fmt (aref P I)):FW:0)))
	 (terpri))))

;; (Pascal 10)
;;                      1 
;;                    1   1 
;;                  1   2   1 
;;                1   3   3   1 
;;              1   4   6   4   1 
;;            1   5  10  10   5   1 
;;          1   6  15  20  15   6   1 
;;        1   7  21  35  35  21   7   1 
;;      1   8  28  56  70  56  28   8   1 
;;    1   9  36  84  126 126 84  36   9   1 
;;  1  10  45  120 210 252 210 120 45  10   1 
;;

(de Pas (n) "Generate Pascal's triangle with n+1 rows"
    (let* ((n1 (+ n 1)) (p (tk n1 1))  (r (p {1 n1} p)))
    ;; Use shift add iterative over rows
      (FOR i 1 n (a r (cat 1 r (a p (+ p (shft -1 p)))))) r))
;; (Pas 5))
;; [1 0  0  0 0 0
;;  1 1  0  0 0 0
;;  1 2  1  0 0 0
;;  1 3  3  1 0 0
;;  1 4  6  4 1 0
;;  1 5 10 10 5 1]

(de pas (n) "Make a matrix of Pascal's triangle with n+1 rows"
    ;; Using binomial expansion
    (let* ((#IO 0) (q (i (+ n 1)))) (tr (o '! q q))))
;; (pas 5)
;; [1 0  0  0 0 0
;;  1 1  0  0 0 0
;;  1 2  1  0 0 0
;;  1 3  3  1 0 0
;;  1 4  6  4 1 0
;;  1 5 10 10 5 1]

(de mkpt (P) "Center justified char matrix of numeric triangle P"
    ;; Uses NDigs RemBlankCols and CenterJust from idioms.al
    (let* ((nd (+ 1 (NDigs (max P)))))
      (if (oddp nd) (incr nd)) ;; For CenterJust to better do its job
      (RemBlankCols ;; remove blank columns for concise representation
       (CenterJust (fe1 (dfm (fe (dfm (fmt (if (zerop w) " " w):nd)) w)) P)))))
;; (mkpt (pas 10))
;; "                  1                 
;;                  1   1               
;;                1   2   1             
;;              1   3   3   1           
;;            1   4   6   4   1         
;;          1   5  10  10   5   1       
;;        1   6  15  20  15   6   1     
;;      1   7  21  35  35  21   7  1    
;;     1  8  28  56  70  56  28   8  1  
;;   1  9  36  84 126 126  84  36  9  1 
;;  1 10 45 120 210 252 210 120  45 10 1"

(de mkpt2 (P) ;; Another version not using lists at all
    (let* ((N (f (+ 2 (l 10 (max P))))) ;; one extra padding space
	   (T (tally P)) (V (p P)) (S (r '* V)) L R U)
      (if (oddp N) (incr N)) (a L (* N (tk -1 V)))
      (a R (p {S N} (tr [2 3 1] (enc (p N 10) P)))) ;; encode over N digits
      ;; convert to ASCII and change zeros to blanks
      (a U (p {T L} (blzsm (a Q (aref #d (+ #IO R))))))
      (RemBlankCols (CenterJust U))))
;; (eql (mkpt (pas 14)) (mkpt2 (pas 14))) ;=> t
	      
(de xmas ()
    (let (R)
      (FOR I 1 8
	   (a R (mkpt (pas I)))
	   (tab (/ (- #CW (tk -1 (p R))) 2))
	   (princ (mkpt2 (pas I))))))

;; Other triangles

(defun S (N) 
   (let ((V (p N 1)) (#IO 0)) 
      (FOR I 1 N (princl (fmt (ElimTB (aref " o" V)):N:0)) (a V (s '<> V)))))
;; (S 16)
;; oooooooooooooooo
;;  o o o o o o o o
;;  oo  oo  oo  oo 
;;   o   o   o   o 
;;   oooo    oooo  
;;    o o     o o  
;;    oo      oo   
;;     o       o   
;;     oooooooo    
;;      o o o o    
;;      oo  oo     
;;       o   o     
;;       oooo      
;;        o o      
;;        oo       
;;         o       

(de Mattri (N)
    (aref " o" (+ #IO (FoFM '(lambda (X) (s '<> X)) (p N 1) (- N 1)))))
"oooooooooooooooooooooooooooooooo
 o o o o o o o o o o o o o o o o 
 oo  oo  oo  oo  oo  oo  oo  oo  
 o   o   o   o   o   o   o   o   
 oooo    oooo    oooo    oooo    
 o o     o o     o o     o o     
 oo      oo      oo      oo      
 o       o       o       o       
 oooooooo        oooooooo        
 o o o o         o o o o         
 oo  oo          oo  oo          
 o   o           o   o           
 oooo            oooo            
 o o             o o             
 oo              oo              
 o               o               
 oooooooooooooooo                
 o o o o o o o o                 
 oo  oo  oo  oo                  
 o   o   o   o                   
 oooo    oooo                    
 o o     o o                     
 oo      oo                      
 o       o                       
 oooooooo                        
 o o o o                         
 oo  oo                          
 o   o                           
 oooo                            
 o o                             
 oo                              
 o             
                  "
