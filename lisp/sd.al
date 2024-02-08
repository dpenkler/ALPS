; -*- mode: lisp; -*-
(defun SD (N) 
 (let ((M (p {N N} 0))) 
    (FOR I 1 N (FOR J 1 (f (/ (+ I 1) 2)) (aset M I (- I J -1) J))) M))

(de sft (D A M) (tk (- (p M)) (rot D A (cat D (p (p M) 0) M)))) 
; Sum shallow skew diagonals of pt to produce fibonacci sequence
(defun PasFib (n)  ;; https://mathworld.wolfram.com/FibonacciNumber.html (63)
  (let* ((#IO 1)  (p (pt (- n 1))))
    (fe '(lambda (u) 
	    (r '+ (implod (mapcar (dfm (aref p  (- u w -1) w))
				  (explod (i {1 (f (/ (+ u 1) 2))}))))))
	 (i {1 n}))))

(de PasFib2 (N) (r '+ (sft 1 (- (- (i N) #IO)) (pt (- N 1)))))
