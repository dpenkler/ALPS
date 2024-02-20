;; attempt to do mandelbrot in alps

(defun mand () ;; ascii mandelbrot set
  (let* ((#PW 79) (#IO 0)
	 (X0 -2.1) (X1 1.1) (Y0 -1.6) (Y1 1.6) (XN 78)  (YN 44) (N 15)
	 (Z (. '+ '% [-9 -11] ;; complex grid [-2.1 2.1]j[-2.1 2.1]
	       (cat 0.5 (p {YN XN} (+ X0 (* (/ (i XN) XN) (- X1 X0))))
		    (tr (p {XN YN} (+ Y0 (* (/ (i YN) YN) (- Y1 Y0))))))))
	 (T Z) (I (p (p Z) 0)))
    (FOR J 1 N (a T (+ (* T T) Z) I (+ I (>= (| T) 2)))) ;;  actual computation
    (princl (aref " ._+/!\":*$()%\=-#" I)) 'ok))
  
	    
(defun mandc (Z) ;; just the complex function on Z
  (let ((N 32) (T Z) (I (p (p Z) 0)))
    (FOR J 1 N (a T (+ (* T T) Z) I (+ I (>= (| T) 2)))) (- 1 (/ I N))))
