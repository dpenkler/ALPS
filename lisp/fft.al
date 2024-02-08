; -*- mode: emacs-lisp; -*-
; Having fun with uncle Joe Fourier
;

;
; 1: Completely lispish implementation
;

(defun lfft (x)
  (let ((selct '(lambda (x y)
		  (mapcan '(lambda (x) (a y (not y)) (if y (list x))) x)))
	(evens '(lambda (x)   (selct x nil)))
	(odds  '(lambda (x)   (selct x t)))
	(add   '(lambda (x y) (mapcar '(lambda (x y) (+ x y)) x y)))
	(sub   '(lambda (x y) (mapcar '(lambda (x y) (- x y)) x y)))
	(ww    '(lambda (x y) (if x (cons (* (car x) y) (ww (cdr x) (* y w))))))
	(w*    '(lambda (x)   (let ((w (% -12 (% (/ (len x)))))) (ww x 1j0))))
	(fft   '(lambda (x)
		  (let ((n (len x)))
		    (cond ((onep n) x)
			  ((oddp n) (error "fft input not a power of two"))
			  (t (let ((even-terms (fft (evens x)))
				   (odd-terms (w* (fft (odds x)))))
			       (append (add even-terms odd-terms)
				       (sub even-terms odd-terms)))))))))
    (fft x)))

;
; 2: FFT APL style
;
(defun afft (x)
  (let* ((#IO 0) (n (p x)) (y (p n 0j0)) (IN (i n)) ; result
	 (w (% -12 (/ (* (% 2) IN) n))))  ;exp(2*pi*i*[0..n-1]/n)
    (FOR k 0 (- n 1) (aset y (r '+ (* x (aref w (| n (* k IN))))) k))
    y)) ; return result

;
; Test harness
;
(defun mfft (x) ; array wrapper for lisp fft
  (implod (lfft (explod x)))) ; array 2 list 2 array

(defun mv (N) ; make real test vector
  (let* ((#IO 0) (L (/ (% (- (/ N 2) (i N))) 4))) (% 2 L)))

(defun mvc (N) ; make test vector
  (let* ((#IO 0) (L (/ (% (- (/ N 2) (i N))) 4))) (% -12 L)))

(defun tfft (n F) ; test implementation F against builtin
  (let ((v (mvc n))) (eq n (r '+ (< (| (- (w v)( F v))) 1e-10)))))

; 3: Array recursive FFT (FIXME)

(defun rfft (X N) 
  (let ((Y (p (p X) 0)) T (WN (/ (% 2) N))
	(auxfft 
	 '(lambda (N L I)
	    (if (onep N) (aset Y (aref X 1) 1)
	      (let ((N2 (/ N 2)))
		(aset Y (auxfft N2 0 (* 2 I)) (i N2))
		(aset Y (auxfft N2 I (* 2 I)) (+ N2 (i N2)))
		(FOR K 1 N2
		     (a T (aref Y K))
		     (aset Y (+ T (* (expt WN K)) (aref Y (+ K N2))) K)
		     (aset Y (- T (* (expt WN K)) (aref Y (+ K N2))) K)))))))
	(auxfft N 1 1)
	Y))

; 4: Ancient direct method adapted from G. Goertzel NDC of America 
; in Anthony Ralston: Mathematical Methods for Digital Computers 1960

(defun Goertzel (X N) ; fortran in lisp
  (prog* ((#IO 0)  U0 U1 U2 n C S Q Wp 
	  (N1 (/ 2 (+ (* 2 N) 1))) (W (% N1)))
	 (FOR p 0 N
	   (a U2 0 U1 0 n (* 2 N) Wp (* W p) C (% 2 Wp) S (% 1 Wp))
	   (while (gt n 0)
	     (a U0 (+ (aref X n) (* 2 C U1) (- U2)) U2 U1 U1 U0 n (- n 1)))
	   (princ (fmt p:4:0
		       (* N1 (+ (aref X 0) (* C U1) (- U2))):7:3
		       (* N1 S U1):7:3))
	   (terpri))
	 "done"))
