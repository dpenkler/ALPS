;;;-*- mode: emacs-lisp -*-
(defun PROB (P X N) (* (! X N) (* (exp P X) (exp (- 1 P) (- N X)))))
(defun POIS (P X N) (/ (* (exp (* N P) X) (exp (- (* N P)))) (! X)))
(defun HYP (X N n m) (/ (* (! X m) (! (- N m) (- n x))) (! N n)))
(defun MINSAMP (B P) (/ (l B) (l (- 1 P))))
(defun HPP (L T K) (/ (* (exp (* L T) K) (exp (- (* L T)))) (! K)))
(defun DISTANCE (P) 
  (prog* ((N (tally  P)) (D (p {N N} 0)))
	 (FOR I 1 N
	      (FOR J 1 N
		   (aset D (. '+ '* (- (aref P I ()) (aref P J ()))
				 (- (aref P I ()) (aref P J ())))
			 I J))) D))
(defun DISTA (P) 
  (prog* ((N (tally  P)) (D (p {N N} 0)) (J 1) I)
	  L0 (a I 1)
	  L1 (aset D (. '+ '* (- (aref P I ()) (aref P J ()))
		       (- (aref P I ()) (aref P J ()))) I J)
	  (go (k (>= N (a I (+ 1 I))) $[L1]))
	  (go (k (>= N (a J (+ 1 J))) $[L0]))
	  D))

(defun Histogram (V) (aref " #" (tr (rev (+ 1 (o '>= V (i (r 'c V))))))))

