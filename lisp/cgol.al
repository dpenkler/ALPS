;-*- mode: emacs-lisp -*-
;; try with (cgol (getf "data/gldr") 150)
;; or  (cgol M 300)
(a CGOL_DELAY 0.05)
(defun cgol (M N) (clr) (princ M) ; conways game of life for N generations
  (let* ((K (= M '*)) K1 K2 L (#IO 0))
    (FOR H 1 N   (wait CGOL_DELAY)
	 (setq L (+ (rot 1 K)             (rot -1 K) 
		    (a K1 (rot 1 1 K))    (a K2 (rot 1 -1 K))
		    (rot 1 K1)            (rot -1 K1)
		    (rot 1 K2)            (rot -1 K2)))
	 (prat 1 1) (princ (aref " *" (setq K (v (= L 3) (^ (= L 2) K))))))))
		 
(a M (p [40 40] " ")) ; build a pi heptomino
(iset M "*" (+ 20 (cat .5 [0 0 0 1 1 2 2]  [0 1 2 0 2 0 2])))
