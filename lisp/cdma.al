;;;-*- mode: emacs-lisp -*-;
; Simulate uplink coding for CDMA IS-95 spread spectrum standard
;


(defun Walsh (N) ; Generate Hadamard Matrix, rows or columns are Walsh codes
	   ((label F lambda (X J)
		   (cond ((zerop J) X)
			 (t (F (cat 1 (cat X X) 
				    (cat X (- X))) 
			       (- J 1)))))
	    (p [1 1] 1) N))

(defun CDMA () ; Bitstream is modulated over radio link
  (prog* ((#PP 0) (#FW 0) (#IO 1) (Bits 8) (Base (p Bits 2)) 
	  Spread DeSpread
	  CharStream BitStream Encode Decode Q (Signal 0)
	  (Messages $["Hello" "User2" "Boris" "Last!"]))

	 (defun Spread (Mes Code) 
	   (let ((CodeLen (p Code)) (MesLen (p Mes)))
	     (* (rav (o '* Mes (p CodeLen 1))) 
		(p (* MesLen CodeLen) Code))))

	 (defun DeSpread (Signal Code)
	   (let ((SigLen (p Signal)) (CodeLen (p Code)))
	     (/ (r '+ (p (cat (/ SigLen CodeLen) CodeLen)
			 (* Signal (p SigLen Code)))) CodeLen)))
	 (defun Walsh (N)
	   ((label F lambda (X J)
		   (cond ((zerop J) X)
			 (t (F (cat 1 (cat X X) 
				    (cat X (- X))) 
			       (- J 1)))))
	    (p [1 1] 1) N))

	 (defun CharStream (V) 
	   (chr (dec Base (tr (p (cat (/ (p V) Bits) Bits) V)))))
 
	 (defun BitStream (X) (rav (tr (enc Base (num X)))))

	 (defun Encode (Mes Code) (Spread (BitStream Mes) Code))

	 (defun Decode (Signal Code) (CharStream (DeSpread Signal Code)))

	 (setq Q (Walsh 2)) ; Generate Walsh code matrix for 2^2 = 4 users
	 (FOR I 1 4 (setq Signal (+ Signal (Encode (aref Messages I) (aref Q I ())))))
	 (FOR I 1 4 (print (Decode Signal (aref Q I ()))))))

(defun TestHadamard (N)
  (prog (Hadamard TestMatrix Size Result Time)

	(defun Hadamard (N) ; Generate Hadamard Matrix size (exp 2 N)
	   ((label F lambda (X J)
		   (cond ((zerop J) X)
			 (t (F (cat 1 (cat X X) 
				    (cat X (- X))) 
			       (- J 1)))))
	    (p [1 1] 1) N))

	(defun TestMatrix (M S) 
	    (eql (/ (. '+ '* M M) S)  ; matrix mulitply 
		 (o '= (i S) (i S)))) ; identity matrix

	(defexpr Time (X) 
	  (let ((T1 (time))) 
	    (list (eval (car X)) (aref (- (time) T1) 1))))

	(princ "Testing Hadamard Matrix\n")
	(FOR I 3 N 
	     (setq Size (exp 2 I))
	     (setq Result (Time (TestMatrix (Walsh I) Size)))
	     (princ (fmt "Size " Size:4 " = " (car Result)
			 " time = " (cadr Result)::5 " secs\n")))))
