;;;-*- mode: emacs-lisp -*-
(setq pi (% 1) -pi (% -1) pi/2 (% 0.5) -pi/2 (% -0.5) pi*2 (% 2))
(defun sin   (x) (% 1 x))
(defun cos   (x) (% 2 x))
(defun tan   (x) (% 3 x))
(defun asin  (x) (% -1 x))
(defun acos  (x) (% -2 x))
(defun atan  (x) (% -3 x))
(defun atan2 (x y) (cond ((zerop y) piby2) (t (% -3 (/ x y)))))
(defun deg   (x) (/ (* x 180) (% 1)))
(defun rad   (x) (% (/ x 180)))
;; trig function on degrees
(defun sind  (x) (% 1 (% (/ x 180))))
(defun cosd  (x) (% 2 (% (/ x 180))))
(defun tand  (x) (% 3 (% (/ x 180))))
(defun asind (x) (* 180 (/ (% -1 x) (% 1))))
(defun acosd (x) (* 180 (/ (% -2 x) (% 1))))
(defun atand (x) (* 180 (/ (% -3 x) (% 1))))

