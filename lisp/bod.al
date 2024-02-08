;-*- mode: emacs-lisp -*-
(require 'trig)
(defun BOOM (N) ; Orrery try with (BOOM 1e6)
  (prog
   ((G .01) ; gravitational constant for this universe
    (T 0.001) ; cosmic tick
    (BodyList)
    (#IO 1)
    (Display '(lambda () (clr) (ms '(ShowPosition Icon) BodyList)))
    (AddBody '(lambda (P M V I) (push (Body P M V I BodyList) 'BodyList)))
    (Body
     '(lambda (POS M V Icon Friends)
	(prog*
	 (F R RM RPOS
	  (NewNeighbour '(lambda (N) (push N 'Friends)))
	  (CalcForce ; calculate the sum of the forces acting on this body
	   '(lambda ()          
	      (a F [0 0])
	      (mapc '(lambda (Remote)
		       (a RPOS (se 'POS Remote) ; remote body position
			  RM   (se 'M   Remote) ; remote body mass
			  R (- RPOS POS)        ; distance vector to remote
			  F (+ F (/ (* G M RM R)(exp (r '+ (sqr R)) 1.5)))))
		    Friends)))
	  (NewPosition
	   '(lambda ()
	      (prog (TPOS)
		    (a V (+ V (* (/ F M) T))  ; Update velocity
		       TPOS (+ POS (* V T)))  ; Update position
		    (cond ((not(zerop (r '+ (sqr (- (f POS) (f TPOS))))))
			   (ShowPosition "." ) 
			   (a POS TPOS) 
			   (ShowPosition Icon)
		;	   (wait 0.01)
			   )
			  (t                   (a POS TPOS)))
		    )))
	  (ShowPosition 
	   '(lambda (SYM) ; Show position
	      (prog ((X (aref POS 1)) (Y (aref POS 2)))
		    (cond ((gt X 80)  (setq X (- X (f X)))))
		    (cond ((lt X  0)  (setq X (+ 80 X (c X)))))
		    (cond ((gt Y 127) (setq Y (- Y (f Y)))))
		    (cond ((lt Y  0)  (setq Y (+ 127 Y (c Y)))))
		    (prat X Y SYM)))) 
	  (Self (fun Body)))
	 ; tell everybody to add this new body 
	 (mapc '(lambda (X) (se `(NewNeighbour ',Self) X)) Friends) 
	 (return Self))))
    )
   (defun rot2 (V A) ; rotate V through A degrees about origin
     (. '+ '* V (p [2 2] {(cosd A) (- (sind A)) (sind A) (cosd A)})))
   (defun tang (V) ; normalised tangent to vector V
     (/ (. '+ '* V (p [2 2] [0 1 -1 0])) (sqrt (r '+ (sqr V)))))
   (defun mktriang (L)
     (let ((V {0 L})) (mapcar '(lambda (A) (rot2 V A)) '(0 120 240))))
   (defun genbod (S D C M) ; Speed, Distance from Center, Mass
     (mapc '(lambda (P I) (AddBody (+ C P) M (* S (tang P)) I))
	     (mktriang D) '("*" "@" "&")))
   
 ;  (Trace AddBody)

   (AddBody [20 40] 30000 [ 0  0] 'S)
;   (AddBody (+ [20 32] (car (mktriang 5))) 9 (* 6 (tang (car (mktriang 5)))))
   (genbod 1 15 [20 40] 1)
   (Display)
   (FOR I 1 N
     (ms '(CalcForce)   BodyList)  ; Calculate forces between everybody
     (ms '(NewPosition) BodyList) ; Move with the forces and show
     ))(terpri) (clr))

