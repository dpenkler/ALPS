;-*- mode: emacs-lisp; -*-
;;
;; gcd lcm and continued fractions
;;

(defun GCD (X Y) (cond ((zerop X) Y) (t (GCD (| X Y) X))))
(defun LCM (X Y) (* (/ X (GCD X Y)) Y))

(defun rmod (X Y) (| Y X))

(defun lgcd (X Y) (while (gt Y 0) (setq Y (rmod X (setq X Y)))) X)

;; Built-in versions
(setq gcd 'v) 
(setq lcm '^)

(defun Gcd (V) ; loop version
  (prog (R) 
    LOOP
    (go (k (< 1 (p (a V (k (* (a V (cat (| (a R (r 'f V)) V) R))) V))))
	   $[LOOP]))
    R))

(defun rgcd (x y) (if (zerop y) (| x) (gcd y (| y x)))) ; recursive version

(defun Ggcd (V) (cond ((ge 1 (p V)) V)
		    ((Ggcd ((lambda (V) (k (* V ) V)) 
			    ((lambda (R V)(cat (| R V) R)) (r 'f V) V))))))

(defun Egcd (x y) ; Euclid's original method using repeated subtraction
  (cond ((lt x y) (gcd x (- y x)))
	((gt x y) (gcd y (- x y)))
	((t x))))

(defun Rgcd (x y))

(defun deco (m) (dec 20 (r '+ (o '= "$%^&" m))))

(defun cf (n d)
  "Expression for continued fraction numerator n denominator d"
  (let ((I (f (/ n d))) (R (| d n)))
    (cond ((zerop R) I)
	  ((gt n d) (list '+ I (cf R d)))
	  (t (list '/ (cf d n))))))

(defun convg (f)
  "Extract continued fraction (convergents) from expression f"
  (cond ((null f) nil)
	((listp (car f)) (append (convg (car f)) (convg (cdr f))))
	((nump (car f)) (cons (car f) (convg (cdr f))))
	(t (convg (cdr f)))))

(defun fc (c) "Calculate fractions from convergents of c"
       (let* ((p0 (car c)) (q0 1)
	      (q1 (if (null (cdr c)) 1 (cadr c)))
	      (p1 (+ 1 (* p0 q1))) S T)
	 (append (list (list p0 q0)) (if (onep q1) nil (list (list p1 q1)))
		 (mapcar '(lambda (x)
			    (a S p1 T q1
			       p1 (+ (* x p1) p0)
			       p0 S
			       q1 (+ (* x q1) q0)
			       q0 T)
			    (list p1 q1))
			 (cddr c)))))
		  

;; (cf 77 65)             ;=> (+ 1 (/ (+ 5 (/ (+ 2 (/ (+ 2 (/ 2))))))))
;; (eval (cf 77 65))      ;=> 1.184615385
;; (/ 77 65)              ;=> 1.184615385
;; (convg (cf 77 65))     ;=> (1 5 2 2 2)
;; (fc(convg (cf 77 65))) ;=> ((1 1) (6 5) (13 11) (32 27) (77 65))
;; (eq GR (apply '/ (car (last (fc (explod (p 32 1))))))) ;=> t ;; Golden ratio
;; (eq GR (apply '/ (car (last (fc (explod (p 31 1))))))) ;=> nil
;; (apply cf (car (last (fc (explod (p 32 1)))))) ;=> (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ 2))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
;; (let ((y 0)) (genc '(lambda (x) (if (onep x) (incr y) nil)) (convg (cf (+ 1 (sqrt 5)) 2))) y) ;=> 38
;; (r '+ (s '^ 1 -1 (= 1 (implod (convg (cf (+ 1 (sqrt 5)) 2)))))) ;=> 38


