;;;-*- mode: emacs-lisp -*-
(require 'idioms)
(require 'dfns)

(defun Pascal (N) 
  (let* ((P 1) (H (/ N 2))
	 (FW (+ 1 (c (l 10 (+ 1 (! H N))))))  (T 39))
    (princ (fmt " ":T (fmt P):FW:0 "\n"))
    (FOR J 1 N
	 (a T (- T (/ FW 2)))
	 (a P (+ (cat 0 P) (cat P 0)))
	 (princ (fmt " ":T))
	 (FOR I 1 (p P) (princ (fmt (fmt (aref P I)):FW:0)))
	 (terpri))))
		   
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


(de Pas (n) ;;Generate Pascal's triangle
    (let* ((p 1) (n1 (+ n 1)) (r (p {1 n1} (tk n1 1))))
      (FOR i 1 n (a r (cat 1 r (tk n1 (a p (+ (cat 0 p) (cat p 0)))))))
      (rot (- (i {n 0})) (k (exp -1 (i {0 (* 2 n)})) r))))

(de pas (n) (let* ((#IO 0) (q (i n))) (tr (o '! q q)))) ;; build triangle

(de mkpt (n) "Make pascal's triangle with n rows"
    (let* ((p (pas n)) (nd (+ 1(NDigs (r 'c (rav p))))) (blanks (p nd " ")))
      (CenterJust (fe1 (dfm (fe (dfm (if (zerop w) blanks
					     (fmt w:nd))) w)) p))))
;; "          1           
;;          1   1         
;;        1   2   1       
;;      1   3   3   1     
;;    1   4   6   4   1   
;;  1   5   10  10  5   1 "


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
