;;;-*- mode: emacs-lisp -*-
;;; show some graphics with newton
(require 'graf)
(require 'geom)
(require 'newton)

(a LS0 (p [2 2] [-5.5 -5.5 5.5 5.5])
   LS1 (p [2 2] [-1 10 3 1])
   LS2 (p [2 2] [-2 10 5 -6])
   LS3 (p [2 2] [10 4 5 10])
   LS4 (p [2 2] [-6 6 3 -3])
   LS5 (p [2 2] [-4 3 5 -3])
   LS6 (p [2 2] [-5 3 6 -2]))

(defun PLLS (X) (graf (tr X))) ;plot LSS
(defun PD (X) (let ((#LS 6)) (graf (tr X)))) ;plot dashed LSS
(defun MK (X) (let ((#LS 6) (#PN 5) (#IO 1))(apply MARK (explod X)))) ;Mark point
(defun PLPoly (X) (let ((#DM EZ_LINE_LOOP)) (graf (tr X)))) ; plot polygon
(defun PLObj (Obj)
  (cond ((isLine Obj) (PLLS (getGeom Obj)))
	((isPoly Obj) (PLPoly (getGeom Obj)))
	((isArc Obj) (apply PLArc (getGeom Obj)))
	(t (error "PLObj: unknown obj"))))

(defun PLArc (r R S E L)
  "plot arc with radius r, centre R, start angle S and end angle E"
  (let* ((#IO 0) (NS 1000)
	 (AV (p {2 NS} (+ S (* (Adj (- E S)) (/ (i NS) NS))))))
    (graf (+ (vtr R) (* r (% (vtr [2 1]) AV))))
    NS))

(defun mkWin (XY) (rav (tr (p [2 2] {(- XY) XY}))))

(defun PLM () ; Test intersecting line segments
  (let ((#PN 1) (L (list LS0 LS1 LS2 LS3 LS4 LS5 LS6))) (gclear)
       (mapc '(lambda (X)  (PLLS X) (a #PN (+ #PN 1))
		(mapc '(lambda (Y)
			 (a I1 (IntLSS X Y) I2 (IntLSS Y X))
			 (cond ((neql I1 I2) (ShowV (I1 I2)))
			       ((nump I1) (MK I1))
			       (t (print I1))))
		      L)) L)))

(defun DLM ()
  (let ((LS) (#PN 1)) (gclear)
       (graf (tr LS0))
       (FOR I 0 14
	    (a LS (- LS3 I))
	    (a #PN (+ #PN 1))
	    (print (graf (tr LS)))
	    (print (IntLSLS? LS0 LS))
	    (print (IntLSLS? LS LS0)))))

(defun TNorm (S)
  (let ((LS (p [2 2] {S (+ (Xc S) 4) (Yc S)})) NLS N R (#PP 2))
    (FOR I 0 16
	 (a R (% (/ (* I 2) 16)))
	 (a R (if (gt R pi) (- R pi*2) R))
	 (a NLS (RotP LS S R))
	 (PLLS NLS)
	 (a N (mkNorm NLS 2 (aref NLS 2 ())))
	 (PLLS N)
	 (print (list I (deg R) 'LS (deg (Slope NLS)) 'N (deg (Slope N)))))))

(defun TArc (S)
  (let ((LS (p [2 2] {S (+ (Xc S) 8) (Yc S)})) NLS N N1 N2 R (#PP 2) A)
    (FOR I 0 16
	 (a R (Adj (% (/ (* I 2) 16))))
	 (a NLS (RotP LS S R))
	 (PLLS NLS)
	 (a N1 (mkNorm NLS 1.5 (aref NLS 2 ()))
	    N2 (RotP N1 (aref N1 1 ()) pi)
	    N  (cat .5 (aref N2 2 ()) (aref N1 2 ())))
	 (PLLS N)
	 (a A (mkArc N 5))
	 (ShowV (A))
	 (apply PLArc A)
	 )))

(defun TTriang (O)
  (let ((#IO 0) (#PN 1) (#PP 2)
	(T (mkTriang (% (/ 4)) 3 4 O))
	Side MidPt Norm Trg RotAng)
    (FOR I 0 16
	 (gclear)
	 (a RotAng (% (/ (* I 2) 16)))
	 (print (list 'RotAng (deg RotAng)))
	 (a Trg (RotP T O RotAng))
	 (PLPoly Trg)
	 (FOR J 0 2
	      (a Side (aref Trg {J (| 3 (+ J 1))} ())
		 MidPt (midpt Side)
		 Norm (mkNorm Side 1 MidPt))
	      (tab 4)
	      (print (list 'MidPt MidPt 'Snorm (deg (Slope Norm))))
	      (MK MidPt)
	      (PD Norm)
	      )
	 (wait 1))))

(defun TLCirc (LS R r)
  (let (IP LSI #PN)
    (PLArc r R 0 pi 0)    (PLArc r R -pi 0 0)
    (FOR I -6 6
	 (a LSI (+ (p [2 2] {0 I}) LS))
	 (a #PN 1)
	 (PLLS LSI)
	 (a IP (ILCirc LSI R r))
	 (ShowV (I IP))
	 (a #PN 2)
	 (cond ((null IP) nil)
	       ((onep (rank IP)) (apply MARK (explod IP)))
	       (t (apply MARK (explod (aref IP 1 ())))
		  (apply MARK (explod (aref IP 2 ())))))
    )))

;; (TLCirc (p [2 2] [-5 -5 5.5 5.14131120271395]) [0 0] 3)
;; (TLCirc (p [2 2] [5 -5 -5.5 5.14131120271395]) [0 0] 3)
;; (TLCirc (p [2 2] [5 -5 -5.5 5.14131120271395]) [-3 2] 3)


(defun TLArc (S LS1 r)
  ;;Test intersection of lines and arcs
  ;;(TLA [0 0]  (p [2 2] [-5 -5 5.5 5.5]) 5)
  (let (IP LSI #PN (#IO 0) (#WN (mkWin [10 10]))
	   (LS  (p [2 2] {S (+ (Xc S) 6) (Yc S)}))
	   (ALS (p [2 2] {
		   (+ (Xc S) r) (- (Yc S) 1.5)
		   (+ (Xc S) r) (+ (Yc S) 1.5)}))
	   (A (p [2 17] $[])) R NLS ANLS)
    (Ginit2D)
    (gclear)
    (FOR I 0 16 ;; build array of arcs A
	 (a R (Adj (% (/ (* I 2) 16))))
	 (a  NLS (RotP LS S R)
	     ANLS (RotP ALS S R))
	 ;; (PLLS NLS) (PLLS ANLS)
	 ;; plot em
	 (apply PLArc (aset A (mkArc ANLS 5) 0 I)) 
	 (apply PLArc (aset A (mkArc (rev 1 ANLS) 5) 1 I)))
    (FOR I -6 6
	 (a LSI (+ (p [2 2] {0 I}) LS1))
	 (a #PN 1)
	 (PLLS LSI)
	 (a #PN 2)
	 (FOR J 0 16
	      (FOR K 0 1
		   (a IP (ILArc LSI (aref A K J)))
		   (ShowV (I IP))
		   (cond ((null IP) nil)
			 ((onep (len IP))
			  (if (PtOnLS? (car IP) LSI) (MK (car IP))))
			 ((PtOnLS? (car  IP) LSI) (MK (car IP)))
			 ((PtOnLS? (cadr IP) LSI) (MK (cadr IP)))))))
    ))


(defun TTRarc (T)
  (let (Ang)
    (FOR J 0 8
	 (a Ang (/ (* pi J) 4))
	 (FOR I 6 35  (TRArc [0 -10] 30 (p [2 2] [-5 7 5 7]) I Ang)
	      (wait T)))))

(defun TRArc (St L LS r Ang)
  ;;Test reflection rays and arcs
  ;; (FOR I 6 35  (TRArc [0 -10] 30 (p [2 2] [-5 7 5 7]) I pi/2) (wait .2))
  (let* ((#IO 0) (#WN (mkWin [10 10])) #PN IP oRay T
	 LS1 RS N1 Phi1 NS Done PoI NPoI Rin Rint
	 (S (RotP St [0 0] Ang))
	 (Ray (mkRay S 0))
	 (LS1 (RotP LS [0 0] Ang))
	 (A (mkArc LS1 r))
	 (C (ACtr  A))
	 (R (ARad A))
	 (N 16)
	 (LI (/ (LSLen LS1) (* 2 N)))
	 (M (midpt LS1))
	 (RBase (mkRayLS (MovP LS1 (- S M))))
	 (Rin (SlopeP S M))
	 (DoSeg '(lambda (X)
		   (addPath NS X)
		   (a N1 (SlopeP X C)
		      Phi1 (Adj (- (Flip Rint) N1))
		      Rout (Adj (- N1 Phi1))
		      oRay (mkRay X Rout)
		      RS (RProj oRay 20)
		      NS X)
;;		   (ShowV 'rav (RS NS))
;;		   (ShowV deg (Rint N1 Phi1 Rout))
		   (a Rint Rout)))
	)
    (Ginit2D)
    (gclear)
    (apply PLArc A)
    (se `(setSlope ,Rin) Ray)
    (FOR I  (* 2 (- N)) (* 2 N) ;; Rays shining on arc
	 (a Path nil)
	 (a NS (RBase (+ (* I LI) (Xc S))))  ;; Next start
	 (se `(setStart ,NS) Ray)            ;; Patch Ray
	 (a RS (RProj Ray L)                 ;; Ray segment
	    Rint Rin
	    Done nil)
	 (princ (fmt "************ " NS " *************\n"))
	 (while (not Done)
	   (a IP (ILArc RS A))
	   (ShowV ((rav RS) NS IP))
	   (ShowV deg (Rint))
	   (cond ((null IP)
		  (print 'null) (apply addPath (explod 1 RS)) (a Done t))
		 ((onep (len IP))
		  (print 'one)
		  (a PoI (car IP))
		  (cond ((lt (r '+ (| (- PoI NS))) #CT)
			 (print (list "TPOL PoI RS " (PtOnLS? PoI RS)))
			 (apply addPath (explod 1 RS)) (a Done t))
			(t (DoSeg PoI))))
		 (t (print 'more)
		    (a T (findc '(lambda (X) (eql X NS)) IP t))
		    (if (null T)
			(Error "failed intersection test 2 points returned")
		      (a NPoI (car T)
			 IP (delete NPoI IP)
			 PoI (car IP))
		      (DoSeg PoI))))
	   ) ;while
	 (mapc PLLS Path) ;(when (eq "b" (getc)) (break) (rclr))
	 ) ;FOR
    ))

(defun TNewt (A S O L T R I0)
  ;; A Apex of triangle1, S source of Ray, O Apex coord of triange 1
  ;; L x-axis shift for triangle 2, T time to wait
  ;; R Radius of curvature of ceiling mirror
  ;; I0 final ray angle
  ;; AAA =  (TNewt 45 [-9.9 1.5] [-6.5 4.5] 13 .02 11.42 -48)
  ;; $ convert -delay 7 -loop 1 *.ppm AAA.gif

  (prog* ((#WN [-10 10 -2 5])
	  (#PN 1)
	  (Frame (mkRect (tr (p [2 2] #WN))))
	  (N (p VisFR)) ;; number of visible colours
	  (Ang (rad A))
	  (C (RotP (mkTriang Ang 7 7 O) O (% 0)))
	  (A (mkArc (p [2 2] [-.5 4.5 .5 4.5]) R))
	  (R (aref (mkRect (tr (p [2 2] [-2.1 2.1 .7 1]))) [1 4 3 2] ()))
	  (L1 (p [2 2] [10 -2 -17 -2]))
	  (L2 (p [2 2] [10 4.5 -10 4.5]))
	  (M1 (p [2 2] [-3.7 -2  -3 -1.95]))
	  (M2 (p [2 2] [3 -1.95 3.7 -2]))
	  (Side (aref C (cond ((eq (Xc S) (Xc O)) [2 3])
			      ((lt (Xc S) (Xc O)) [1 2])
			      (t [3 1])) ()))
	  (MidPt  (midpt Side )) ; midpoint prism on side of source
	  (Paths (tk N $[]))
	  ;; alist of objects and their $[fn r t]	 
	  (Objs (list (list ${SMR 0 PolyTp} C) ;; triangle 1
		      (list ${SMR 0 PolyTp}    ;; triange 2
			    (MovP C ;(RotP C O (% 1))
				  {L 0}))
			    ;;(- (* (LSLen Side) (sin (/ (- pi Ang) 2))))}))
		      ;; mirror at y= -9 reflecting up
		      (list ${nil 1 LineTp} L1)
		      ;;(list ${nil 1 LineTp} L2)	      
		      (list ${nil 1 LineTp} M1)	      
		      (list ${nil 1 LineTp} M2)	      
		      (list ${nil 1 ArcTp}  A)
		      (list ${SMR 0 PolyTp} R)
		      ))
	  (NCanvas (mkwdgt EZ_WIDGET_3D_CANVAS ()
		      (list (list EZ_EXPAND 1)
			    (list EZ_WIDTH_HINT  1080)
			    (list EZ_HEIGHT_HINT (* 54 7))
			    (list EZ_BACKING_STORE 1)
			    ;; (list EZ_EVENT_HANDLER Cevent)
			    )))

	  Ray RaySlope (G 1000)
	  (ILS (cat .5 S MidPt))
	  (Sx (Xc S)) (Sy (Yc S)) TSy
	  (Fcount 0)
	  (RPrism '(lambda (I) 
		     ;; (ShowV (I #WN))
		     (a RaySlope (/ (* pi I) G)
			Ray (mkRayLS (RotP ILS MidPt RaySlope)))
		     (TPrism Frame Objs Ray)
		     (wait T)
		     (incr Fcount)
		     ;(gsave NCanvas (fmt "TripleA_" Fcount:-4 ".ppm" ))
		  )))
	 
	 (dispwdgt NCanvas)
	 (gcanvas NCanvas)
	 (glinit)
	 (gclear)
	 ;; set up for 2-D
	 (gdisable EZ_DEPTH_TEST  EZ_LIGHTING EZ_CULL_FACE )
	 (proj  nil)
	 (RPrism 100)
	 (getc)
	 (FOR I 100 DOWNTO (- I0 10) (RPrism I))
	 (FOR I (- I0 10) (+ I0 10) (RPrism I))
	 (FOR I (+ I0 10) DOWNTO I0 (RPrism I))
;	 (RPrism I0)
	 ))

(defun TPrism (Frame Objs Ray)
  (let* ((#PN 1) (#IO 1))
    ;(a N 4)
    (FOR I 1 N (aset Paths (Newton Frame Objs Ray I) I))
    (gclear)
    (PLLS (car (aref Paths 1))) ;; plot first piece of ray in white
    (mapc PLObj Objs) ;; Draw objects
    (FOR I 1 N
	 (a #PN (+ I 1))
	 (mapc '(lambda (X) (graf (tr X))) (cdr (aref Paths I)))) ; show path
    ))

(defun TIRLS (S)
  (let (Ray L (Poly (p [4 2] [-10 -10 -10 10 10 10 10 -10])))
    (FOR I 0 16
	 (a Ray (mkRay S (% (/ (* I 2) 16))))
	 (a L (findSides Ray Poly S))
	 (print (list 'Ang (deg (RSlope Ray))))
	 (print L)
	 (cond ((onep (len L))
		(gclear)
		(graf (tr (p [2 2] {S (cadar L)}))) (getc))))))

(defun TProj (S)
  (let ((Ray (mkRay S 0)) (N 720))
    (gclear)
    (FOR I 0 N
	 (se `(setSlope (rad ,I)) Ray)
	 ;;(print (deg (RSlope Ray)))
	 (PLLS (RProj Ray (/ (* 5 I) 360))))))

(defun TAngc (Aperture)
  (let (A B C Cnt S (Blip '(lambda () (incr Cnt) (push (list A B C) 'S))))
    (FOR I 0 360
	 (a A (Adj (rad I)) B (Adj (rad(+ I Aperture))))
	 (a Cnt 0 S nil)
	 (FOR J 0 359
	      (a C (Adj (rad J)))
	      (if (AngBtwxt A B C) (Blip)))
	 (when (ne Cnt (+ Aperture 1))
	   (ShowV deg (A B C))
	   (ShowV (Cnt))
	   (getc)
	   (while S (print (mapcar deg (pop 'S))))))))
     
;; Fiat Lux
(a #WN [-10 10 -10 10])
;;(Ginit2D)

(defun TGEOM (T) "Test newton with delay T seconds"
  (TNewt 45 [-9.9 1.5] [-6.5 4.5] 13 T 11.42 -48))
