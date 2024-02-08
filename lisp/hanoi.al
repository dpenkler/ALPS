(defun Hanoi (N) ;; move disks from Peg1 to Peg2
  (let* ((#IO 1) (I 1) (D (mapcar '(lambda (X) (fmt "D" X)) (explod (i N)))))
    (Hanaux D 'Peg1 'Peg2 'Peg3)))

(defun Hanaux (Disks A B C) ;; (Hanaux '(D1 D2 D3) 'Peg1 'Peg2 'Peg3)
  (cond (Disks (Hanaux (cdr Disks) A C B)
	       (princl(fmt I " Move " (car Disks) " from " A " to " B))
	       (incr I)
	       (Hanaux (cdr Disks) C B A))))

;; Calculate minimum number of moves
(de h1 (n) (if (onep n) 1 (+ (* 2 (h1 (- n 1))) 1))) ;; recursive
(de h2 (n) (- (exp 2 n) 1)) ;; closed form
;; Calculate number of moves always going via middle peg
(de hm1 (n) (if (onep n) 2 (+ (* 3 (hm1 (- n 1))) 2))) ;; recursive
(de hm2 (n) (- (exp 3 n) 1)) ;; closed form
