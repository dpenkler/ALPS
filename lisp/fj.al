; -*- mode: emacs-lisp; -*-
;; Flavius Josehpus elimination problem
;; In a circle of n people every kth person is executed going round the circle
;; Calculate the position of the survivor(s) in the initial circle
(defun fj (n k) ;; using lists
  (let ((L (explod (i n))) (I (- k 1)))
    (while (ge (len L) k)
      (a L (delete (nth I L) L)
	 I (| (len L) (+ I (- k 1)))))
    L))

(defun fjr (n k) ;; recurrence relationship
  (if (onep n) 1 (+  1 (| n (+ (fjr (- n 1) k) k -1)))))

(defun fj2 (n) ;; using binary rep for k=2
 (dec 2 (rot 1 (enc (p (f (+ 1 (l 2 n))) 2) n)))))
; End of file: fj.al
