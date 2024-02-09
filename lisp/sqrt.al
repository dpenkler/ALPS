;;;-*- mode: emacs-lisp -*-
(defun nsqrt (X) ; Newton's iteration
  (let ((Y 1) (E 0))  (while (ne (sqr Y) X) (a Y (* .5 (+ Y (/ X Y)))))))

(defun bbsqrt (X) ; Bhaskara-Brouncker
  (let ((Y 1) (A 1) (B 1) T)
    (while (ne (sqr Y) X) (a T A A (+ A (* B X)) B (+ T B) Y (/ A B)))))
