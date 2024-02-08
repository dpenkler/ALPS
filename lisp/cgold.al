;-*- mode: emacs-lisp -*-
; Old version
; try with (cgol (getf "data/gldr") 150)
(defun cgol (M N) (clr) (princ M) ; conways game of life for N generations
  (prog* ((K (= M '*)) (R (aref (p M) 1)) (C (aref (p M) 2)) (G 0)
	  (RA {R (i (- R 1))}) (RB {(+ 1 (i (- R 1))) 1})
	  (CA {C (i (- C 1))}) (CB {(+ 1 (i (- C 1))) 1}))
 LOOP	 (setq L (+ (aref K RA nil)    (aref K RB nil)   
		    (aref K nil CA)    (aref K nil CB)
		    (aref K RA CA)     (aref K RA CB) 
		    (aref K RB CA)     (aref K RB CB)))
         (princ (fmt "\[[H\[[2JGeneration " (setq G (+ 1 G)) "\n"))
	 (princ (aref " *" (+ 1 (setq K (v (= L 3) (^ (= L 2) K))))))
	 (wait .05) 
	 (cond ((lt G N) (go 'LOOP)))))
