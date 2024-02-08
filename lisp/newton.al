(require 'geom)
(require 'opt)
(require 'graf)
;; Env is a list of Objects
;; An Object is a pair (X G)
;;     where X is a property vector and G is the associated geometric object
;; A geometric object is a line segment, polygon, arc or composite structure
;; For polygons and composite structures it is assumed that the normal
;; constructed on a side using the earlier point as the first and the
;; following as the second will have the normal pointing outward.
;; The property vector: [n r t]
;; where n is the refractive index
;;       r is the reflectivity
;;   and t is the type 0 - line, 1 - polygon, 2 - arc, 3 - composite structure
;; An edge is a list of Side and Point of intersection (LS PoI)
(defun getProps (Obj)  (car Obj))
(defun getPropN (Prop) (aref Prop 1))
(defun getPropR (Prop) (aref Prop 2))
(defun getPropT (Prop) (aref Prop 3))
(defun getGeom  (Obj)  (cadr Obj))
(defun objType  (Obj)  (getPropT (getProps Obj)))
(defun isLine   (Obj)  (eq LineTp (objType Obj)))
(defun isPoly   (Obj)  (eq PolyTp (objType Obj)))
(defun isArc    (Obj)  (eq ArcTp  (objType Obj)))
(defun getSide  (X)    (car X))
(defun getPoI   (X)    (cadr X))

(defun DrawNorm (LS) ; point P length L normal ray R
  (let ((#LS 6) (R (r '- 1 LS))) (graf (tr LS))
       (graf (tr (RotP LS (aref LS 1 ()) pi)))))

(defun PtInEnvObjs (P Env) ;; for now only check polygons.
  (mapcan
   '(lambda (X)
      (if (not (isPoly X)) nil ; check for polygon
	(if (inside P (cadr X)) (list X))))
            Env))

(defun findPoIs (R Type Geom ExSide)
  "Find sides of Poly/line/arc pierced by R -> list of (side poi)"
  ;; ExSide is excluded.
  ;; Note: For Arc ExSide is chord ergo only works if Arc is concave towards R
  (let* ((#IO 0) Side Res Int N N1
	 (ProcLine '(lambda ()
		      (FOR I 0 N1
			   (a Side (aref Geom {I (| N (+ I 1))} ()))
			   (if (eql ExSide Side) (a Int nil)
			     (a Int (IntLSS R Side)))
			   ;;(ShowV ("findPoIs" (rav Side) Int))
			   (if Int (push (list Side Int) 'Res)))))
	 (ProcArc '(lambda ()
		     (if (neql ExSide (a Side (AChord Geom)))
			 (when (a Int (ILArc R Geom))
			   ;;(ShowV ("findPoIs" (rav Side) Int))
			   (push (list Side (car Int)) 'Res))))))
    ;;(princl (fmt "Checking out a " (aref ObjDes Type)))
    (cond ((eq Type LineTp) (a N 0 N1 0) (ProcLine))
	  ((eq Type PolyTp) (a N (aref (p Geom) 0) N1 (- N 1)) (ProcLine))
	  ((eq Type ArcTp)  (ProcArc))
	  (t (error "findPoIs: unrecognised object type")))
    Res))

(defun findObjects (Ray Side Env)
  ;; find edges pierced by Ray in proximity order to start of Ray S
  (let ((R (RProj Ray 1e7)))
    (mapcan
     '(lambda (Obj)
	;; (print (list "Examining " (rav (cadr X))))
	(let ((L (findPoIs R (objType Obj) (getGeom Obj) Side)))
	  (if (gt (len L) 0)
	      (list (list Obj (sort '(lambda (X Y)
				       (lt (r 'dist (- (cadr X) S))
					   (r 'dist (- (cadr Y) S)))) L))))))
     Env)))

(defun nearestOE (S L)
  (let ((Far 1e7) Obj E Edge D)
    (mapc '(lambda (X)
	     (a	E (caadr X) ; nearest Edge of X
		D (r 'dist (- (getPoI E) S))) 
	     (if (lt D Far) (a Obj (car X) Edge E Far D)))
	  L)
    (list Obj Edge)))

(defun addPath (X Y)  ;;(print (list "addPath" X Y))
  (a Path (append Path (list (p [2 2] {X Y})))))

(defun Error (S) (if (not DEBUG) (error S) (print S) (break)))

(defun Newton (Frame Env Ray Index)
  (prog ((S (RStart Ray)) ; Source ray coords
	 (PropStk nil) ; property stack
	 (N 1) ; Index of refraction of current object
	 (R 0) ; Reflectivity of Frame (perfect absorber)
	 NOE NOB NOP NON NEL NED Side PoI NextN Norm L Exited
	 Delta Rin Rint Rout Phi1 p1 N1 Phi2 p2 N2
	 (RintV []) ; vector of internal ray angles for debug
	 (Path nil) ; set of LS of light path
	 (#IO 1) (#PP 2))
	
    (if (not (inside S Frame)) (error "Ray starts outside of frame"))
    (if (gt (len (PtInEnvObjs S Env)) 0)
	(error "Ray must not start in another object"))
    ;; (ShowV ("Enter newton " Index))
NextObj   ;; For each object
    ;;(print (list "Searching for objects pierced by " (DEG (RSlope Ray))))
    (while (lt 0 (len (a L (findObjects Ray Side Env))))
      ;;(print (list 'num-objs (len L)))
      
      (a Rin (RSlope Ray)    ; Incoming ray angle
	 NOE (nearestOE S L) ; get nearest (object edge) combo
	 NOB (car NOE)       ; nearest object
	 NOP (getProps NOB)  ; properties of NOB
	 NON (getPropN NOP)  ; Function for refractive index of NEO
	 NED (cadr NOE)      ; get edge
	 Side (getSide NED)  ; nearest side
	 PoI  (getPoI NED))  ; point of Incidence of ray and nearest side

      (a NextN (if (null NON) 1 (NON (aref VisFR Index)))) ; get actual ref ind
      ;;(ShowV ((DEG Rin) N NextN))

      ;;(princl (fmt "Found a " (aref ObjDes (+ 1 (objType NOB))) " at "  PoI))
      (addPath S PoI)

      (a S PoI) 
      ;; (DrawNorm Norm) ; Draw the normal so we can see what is going on
      
      (cond ((isLine NOB)
 	     (if (zerop (getPropR NOP)) (return Path)) ;; zero reflectivity
	     (a N1 (Slope (a Norm (mkNorm Side 2 PoI)))
		Phi1 (Adj (- (Flip Rin) N1)) ;; angle between Rin and Normal
		Rout (Adj (- N1 Phi1)) ;; Calc reflected angle
		Ray (mkRay S Rout))    ;; Make new ray with Rout
	     ;;(ShowV DEG (N1 Rin Phi1 Rout))
	     (go 'NextObj))
	    ((isArc NOB) ;; Do Arc thing
 	     (if (zerop (getPropR NOP)) (return Path)) ;; this ray going nowhere
	     ;; N1 is slope of line from S to centre of curvature of arc
	     (a N1 (SlopeP S (ACtr (getGeom NOB)))
		Phi1 (Adj (- (Flip Rin) N1))
		Rout (Adj (- N1 Phi1)) ;; Calc reflected angle
		Ray (mkRay S Rout))    ;; Make new ray with Rout
	     ;;(ShowV DEG (N1 Rin Phi1 Rout))
	     (go 'NextObj))
	    ((isPoly NOB));; drop through
	    (t (mapc PLLS Path) (Error "Unsupported object")))

      (a N1 (Slope (a Norm (mkNorm Side 2 PoI)))
	 Phi1 (Adj (- (Flip Rin) N1)) ;; < bet Rin and Norm
	 p1 (snell N NextN Phi1))

      (push {N R} 'PropStk) ; save current properties

      (a Rint (Adj (+ (Flip N1) p1)) ; New internal Ray angle
	 RintV (cat Rint RintV)      ; Add to tracking vector
	 Ray (mkRay S Rint)          ; Make new Ray with Rint
	 N NextN R (aref NOP 2) ; set new properties for object entered by Ray
	 sProp (pop 'PropStk) NextN (aref sProp 1) ; restore previous props
	 Exited nil)
      ;;(ShowV ((DEG Rint) N NextN)) 
      (while (not Exited)
	;;(print (list "Checking out NOB " (rav (getGeom NOB))))
	(a L (findPoIs (RProj Ray 1e7) PolyTp (getGeom NOB) Side))
	;;(print (list "Found" (len L) "sides"))
	(cond ((zerop (len L)) (go 'Escape))
	      ((gt (len L) 1) (mapc PLLS Path)
	       (Error "More than one exit found")))
	(a ELS  (car L)       ;; First side and PoI (should only be one)
	   Side (getSide ELS) ;; Side
	   PoI  (getPoI ELS)) ;; point of intersection of ray and Side
	(addPath S PoI)
	
	(a S PoI                                   ;; Update source       
	   N2 (Slope (a Norm (mkNorm Side 2 PoI))) ;; internal normal
	   p2 (- N2 Rint))       ;; internal angle wrt internal normal
	;; (DrawNorm Norm)
	(if (ge (| (/ (* N (sin p2)) NextN)) 1) ;; internal reflection ?
	    (a Rint (+ (Flip N2) p2)
	       RintV (cat Rint RintV)
	       Ray (mkRay S Rint))
	  (a Exited t))
	)
      
      (a Phi2 (snell N NextN p2)  ;; Angle of exit wrt normal
	 Rout (Adj (- N2 Phi2))   ;; Absolute ange of exit
	 Delta (Adj (- Rin Rout)) ;; delta (check)
	 N NextN                  ;; restore previous N
	 Ray (mkRay S Rout))      ;; Make new Ray for the next round
      (when (onep Index) ()
	;;(ShowV DEG (N1 Phi1 p1 RintV p2 N2 Phi2))
	;;(ShowV DEG (Rin Rout Delta))
	)
      ) ; while
Escape
    ;; (princl "Escaping, checking Frame")
    (a L (findPoIs (RProj Ray 1e7) PolyTp Frame [0 0]))
    (if (gt (len L) 1)
	(let (T)
	  ;; Elminate side that the source might be sitting on
	  (if (a T (findc '(lambda (X) (PtOnLS? S (car X))) L t))
	      (a L (delete (car T) L)))))
    (cond ((onep (len L)) (if S (addPath S (cadar L)))) ; add last segment
	  (t (mapc PLLS Path) (print "Ray has escaped again!")))
    (return Path)
    ))
