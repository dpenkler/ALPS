; -*- mode: emacs-lisp; -*-
(defun nni (R X) ;; nearest neighbour interpolation by ratios R of X
  (let ((#IO 0) (PX (p X)) RK)
    (if (zerop (rank R)) (a R (p 1 R)))
    (if (neql (rank X) (p R))
	(error "R must have as many elements as X has dimensions"))
    (apply 'aref X
	   (mapcar '(lambda (K)
		      (a RK (aref R K))
		      ((aref $[f c] (<= RK 1)) (/ (i (* RK (aref PX K))) RK)))
		   (explod (i (p R)))))))
  
(defun Tnni ()
  (let ((M (p [30 40] (i 1200))))
    (if (neql (k 2 (k 1 2 M)) ;; apl style
	      (nni [2 2] M)) (error "nni failed")
      'ok)))

(defun ticker (D)
  (let* ((A "the quick brown fox jumps over the lazy dog. ")
 	 (CHARS (LoadFont "data/font"))
	 (B (nni [2 2] (Banner A)))
 	 (V (i (tk -1 (p B))))
 	 (#PW #CW)
 	 (Lock "\[l")
 	 (Unlock "\[m")
 	 (L (i (- #PW 1))))
    (terpri (tally B))
    (prat (- #CH (tally B)) 0 Lock)
    (unwind-protect
 	(while t (princ (aref B () (aref V L))) (a V (rot 1 V)) (wait D))
      (princl Unlock)
      (princl "Good bye!"))))

