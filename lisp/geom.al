; Computational geometry in 2D Euclidean Space

;; Some basic objects:
;; Point: a vector [X Y]
;; Line segment: an array of 2 points (p [2 2] [X1 Y1 X2 Y2]
;; Normals: A normal to a line segment has its direction determined
;;          by the left-hand rule. If the intersection of thumb and
;;          index finger is placed on the first point of the line
;;          segment with the index finger running along the line
;;          segment the thumb then points in the direction of the
;;          normal.
;; Polygon: An N-agon is an array of N points
;;          (p {N 2} [X1 Y1 X2 Y2 X3 ... XN YN])
;;          The order of the coordinates determine the direction of
;;          then normals to the sides. A polygon with a Clockwise
;;          order of its vertex points has normals pointing away from
;;          the polygon. A polygon with vertices in counter-clockwise
;;          order will have the normals pointing inwards.
;; 
;; Ray or Line: functional value capturing start point and slope
;;
;; Arc: For convenience an arc is specified by the two points
;;      representing the ends of the arc and a radius of
;;      curvature. The centre of curvature lies on the normal of the
;;      line segment joining the end points of the arc. Thus the order
;;      of the points determines the position of the centre of
;;      curvature. The data structure representing the resulting arc
;;      is the quad (r R S E) where r -> length of radius, R -> centre
;;      of curvature, S -> the angle of the line segment from R to the
;;      first endpoint and E -> the angle of the line segment from R
;;      to the second endpoint.

(require 'trig)
(setq LineTp 0 PolyTp 1 ArcTp 2)
(a ObjDes ${"Line" "Polygon" "Arc" "Composite"})

(defmacro Xc  (X) `(aref ,(car X) ,@(cdr X) #IO))        ;; General X coord
(defmacro Yc  (X) `(aref ,(car X) ,@(cdr X) (+ #IO 1)))  ;; General Y coord
(defun ARad   (A) (car A))    ; radius of Arc A
(defun ACtr   (A) (cadr A))   ; centre of curvature of Arc A
(defun AStrt  (A) (caddr A))  ; Start angle of Arc A
(defun AEnd   (A) (nth 3 A))  ; End angle of Arc A
(defun AChord (X) (nth 4 A)) ; Chord spannig Arc A

(defun inside (P Poly)
  "Is point P inside N-sided ploygon Poly (p Poly) -> {N 2}"
  (let* ((Px (Xc P)) (Py (Yc P))
	 (Vx (Xc Poly ()))
	 (Vy (Yc Poly ()))
	 (T (<= Vy Py))
	 (R (v (^ T (~ (rot 1 T))) (^ (~ T) (rot 1 T)))))
    (a Xi (/ (k R (- Py Vy)) (k R (- (rot 1 Vy) Vy))))
    (a Xj (k R (- (rot 1 Vx) Vx)))
    (a Xk (+ (k R Vx) (* Xi Xj)))
    (oddp (r '+ (< Px Xk)))))

(defun IntLSS (LS1 LS2) ; adapted from xlines in Graphics Gems II
  "Does line segment LS1 intersect line segment LS2
    If it does return point of intersection or 'COLLINEAR else nil"
;;;  Compute a1/2, b1/2, c1/2, for line segments 1/2 where eqn for the lines
;;;  are: a1/2 x  +  b1/2 y  +  c1/2  =  0.
;;;  ab1/2 for LS1/2 = {(- y2 y1)  (- x1 x2)} = {a1/2 b1/2}
;;;  where LS1/2 = (p [2 2] {x1 y1 x2 y2})
  (let* ((ab1  (r '- 1 (rot 1 (rot 1 [0 1] LS1))))
	 (ab2  (r '- 1 (rot 1 (rot 1 [0 1] LS2))))
	 (c1 (- (det LS1)))
	 (c2 (- (det LS2)))
         (r2 (+ (. '+ '* LS2 (tr ab1)) c1))
         (r1 (+ (. '+ '* LS1 (tr ab2)) c2))
	 denom)
    ;; (ShowV (ab1 ab2 c1 c2 r1 r2))
    ;; no intersection if both points of LS2 lie on the same side of LS1
    ;; or both points of LS1 lie on the same side of LS2
    (if (onep (r 'v (< 0 (r '* 1 (cat r1 r2))))) nil ;; no intersection
      ;; else  compute intersection point. 
      (if (zerop (a denom (det (a ab12 (cat .5 ab1 ab2))))) 'COLLINEAR
	(/ (* (. '- '* {c2 c1} (rot 1 ab12)) [-1 1]) denom))
      )
    ))

(defun ILCirc (LS R r)
  "Intersection of Line LS and  Circle with centre R and radius r"
  (let* ((RLS (- LS (p (p LS) R))) ; translate LS to origin
	 (d   (r '- 1 RLS))
	 (dr2 (r '+ (sqr d)))   (c (- (det RLS)))
	 (xy0 (/ (* (rev d) c [1 -1]) dr2))
	 (d2 (- (sqr r) (/ (sqr c) dr2))) md)
    ;; (ShowV (d dr2 c d2 x0 y0))
    (cond ((lt d2 0) nil) ; no intersection -> no points
	  ((lt (| d2) #CT) xy0) ; tangent -> one point only
	  (t (a md (* (sqrt (/ d2 dr2)) d)) ; two points
	     (cat .5 (+ xy0 R md) (- xy0 (- R) md))))))

(defun ILAChk (IC) ; C, AS, AE free, bound in ILA
  (let ((LA (SlopeP C IC)) (#PP 2))
;    (ShowV DEG (LA AS AE))
;    (ShowV (IC))
    (if (AngBtwxt AS AE LA) (list IC))))

(defun ILArc (LS A)
  "Intersection of line LS with arc A"
  (let* ((#IO 1)
	 (C  (ACtr  A))
	 (AS (AStrt A))
	 (AE (AEnd  A))
	 (IC (ILCirc LS C (ARad A)))
	 S1 S2)
    (cond ((null IC) nil)
	  ((onep (rank IC)) (ILAChk IC))
	  (t (nconc (ILAChk (aref IC 1 ()))
		    (ILAChk (aref IC 2 ())))))))
	   
(defun Between (P XV) (or (and (ge P (aref XV 1)) (le P (aref XV 2)))
			  (and (le P (aref XV 1)) (ge P (aref XV 2)))))

(defun PtOnLS? (P LS)
  "Is point P on line segment LS including endpoints ?"
;;; Using parametric approach
;;; y=(y1 - y0)t + y0 => t1=(y - y0)/(y1 - y0)
;;; x=(x1 - x0)t + x0 => t2=(x - x0)/(x1 - x0)
;;; point on line if 0 <= t1,t2 <= 1       
       (let* ((#IO 0) (dt  (/ (- P (aref LS 0 ())) (- (r '- 1 LS)))))
;;	 (ShowV (P (rav LS) dt))
	 (onep (r '^ (^ (>= dt 0) (<= dt 1))))))

(defun PtInLS? (P LS)
  "Is point P *Inside* line seg LS excluding endpoints ?"
;;; point in line if 0 < t1,t2 < 1       
       (let* ((#IO 0) (dt  (/ (- P (aref LS 0 ())) (- (r '- 1 LS)))))
	 (ShowV (P (rav LS) dt))
	 (onep (r '^ (^ (> dt 0) (< dt 1))))))

(defun mkRay (S Slope)
  "(mkRay [X Y] Slope) -> (Ray X) -> {X (Ray X)}"
  (let (SX SY
	   (F '(lambda (X)
		 (if (lt (| (- (| Slope) pi/2)) #CT)
		     {SX (- X SX)} ;; use X to move vertically along Y
		 {X (+ SY (* (tan Slope) (- X SX)))})))
	(Proj '(lambda (L)
		 (if (lt (| (- (| Slope) pi/2)) #CT)
		     (cat .5 S (cat SX (+ SY (* (* Slope) L))))
		   (cat .5 S (F (+ SX (* L (cos Slope))))))))
	(getSlope '(lambda () Slope))
	(setSlope '(lambda (X) (a Slope (Adj X))))
	(getStart '(lambda () S))
	(setStart '(lambda (X) (a S X SX (Xc S) SY (Yc S))))
	)
    (setStart S)
    (setSlope Slope)
    (fun F)))
(defun RSlope (R) (se '(getSlope) R))
(defun RStart (R) (se '(getStart) R))
(defun RProj (R L) (se `(Proj ,L) R))

(defun sgn* (X) (if (zerop (* X)) 1 (* X)))

(defun Slope (LS)
  (let* ((#IO 1) A
	(X1 (aref LS 1 1)) (X2 (aref LS 2 1))
	(Y1 (aref LS 1 2)) (Y2 (aref LS 2 2)))
    (if (eq X1 X2)  (* (* (- Y2 Y1)) pi/2) ; +- pi/2 depending on direction
      (+ (atan (/ (- Y1 Y2) (- X1 X2))) (* (> X1 X2) (sgn* (- Y2 Y1)) pi)))))

(defun SlopeP (P1 P2)
  (let* ((#IO 1) A (X1 (Xc P1)) (X2 (Xc P2)) (Y1 (Yc P1)) (Y2 (Yc P2)))
    (if (eq X1 X2)  (* (* (- Y2 Y1)) pi/2) ; +- pi/2 depending on direction
      (+ (atan (/ (- Y1 Y2) (- X1 X2))) (* (> X1 X2) (sgn* (- Y2 Y1)) pi)))))

(defun mkTriang (<BAC AB AC A)
  ;; based on (eql (/ (sin alpha) a)  (/ (sin beta) b)) etc 
  "<BAC apex angle, AC AB lengths, A coords of apex, BC assumed  horizontal"
  (let* ((BC (sqrt (- (+ (sqr AB) (sqr AC)) (* 2 AB AC (cos <BAC)))))
	 (<ABC (acos (/ (+ (sqr AB) (sqr BC) (- (sqr AC))) (* 2 AB BC))))
	 (AD (* AB (sin <ABC)))
	 (BD (* AB (cos <ABC)))
	 (<ACB  (- pi <BAC <ABC))
	 (DC (* AC (cos <ACB)))
	 (AX (aref A 1)) (AY (aref A 2)))
    ;(print (mapcar '(lambda (X) (* 180 (/ X pi))) (list <ABC <BAC <ACB)))
    (p [3 2] {A (- AX BD) (- AY AD) (+ AX DC) (- AY AD)})))

(defun mkRect (W) "make rectangle with diagonal W"
       (p [4 2] (rot 1 (rav (rot 1 (tr [3 1 2] (cat .5 (tr W) (tr W))))))))

(defun midpt (LS)
  "Return midpoint of line segment LS"
  (let* ((#IO 1)
	 (X1 (Xc LS 1)) (Y1 (Yc LS 1))
	 (X2 (Xc LS 2)) (Y2 (Yc LS 2))
	 (XM (/ (- X2 X1) 2))
	 (YM (/ (- Y2 Y1) 2)))
    (cond ((eq Y1 Y2) {(+ X1 XM) Y1})
	  ((eq X1 X2) {X1 (+ Y1 YM)})
	  (t {(+ X1 XM) (+ Y1 (* (tan (Slope LS)) XM))}))))

(defun LSLen (LS) "Length of line segment LS" (r 'dist (r '- 1 LS)))


(defun incAng (LSS) ; included angle based on cosine law 
  "Angle of two intersecting line segments BA AC (p LSS) -> [3 2]"
 (let ((BA (LSLen (aref LSS [1 2] ())))
       (AC (LSLen (aref LSS [2 3] ())))
       (BC (LSLen (aref LSS [1 3] ()))))
   (asin (/ (+ (sqr BA) (sqr AC) (- (sqr BC))) (* 2 BA AC)))))

(defun mkNorm (LS L Pt) ; normal to LS at Pt len L
  (let* ((S (Slope LS)) Ang (R (r '- 1  LS)))
    (a Ang (cond ((lt (| (Xc R)) #CT) (* (> (Yc R) 0) pi))
		 (t        (- S pi/2))))
    (p [2 2] {Pt (+ Pt (* L (% [2 1] Ang)))})))
	 
(defun mkRayLS (LS) "Make Ray from line segment"
       (mkRay (aref LS 1 ()) (Slope LS)))

(defun mkArc (LS r)
  "Make arc spanning LS with radius of curvature r"
  (let* ((#IO 1)
	 (h (/ (LSLen LS) 2)) ; half the length of LS
	 (Ang (if (ge r h) (asin (/ h r)) pi))
	 (R (r '- 1  LS))
	 (N (mkNorm LS (sqrt (- (sqr r) (sqr h))) (midpt LS)))
	 (S (Flip (Slope N))))
    (ShowV (r h Ang R (rav N) S))
    (list r (aref N 2 ()) (Adj (- S Ang)) (Adj (+ S Ang)) LS)))

;;; Normalise X Y such that the length of LS [0 0] (Norm2 X Y) is 1
(defun Norm2 (X Y) (let ((D (dist X Y))) (/ {X Y} D)))

(defun MovP (Pl Point)
  "Move Polygon or line segment Pl by displacement Point"
  (+ Pl (p (p Pl) Point)))

(defun RotP (Pl Point Angle)
  "Rotate Polygon or line segment Pl around Point by Angle"
  (let* ((D (p (p Pl) Point))
	 (S (sin Angle)) (C (cos Angle))
	 (M (p [2 2] {C S (- S) C})))
    (+ D (. '+ '* (- Pl D) M))))

(defun Flip (X) (Adj (+ pi X))) ; Flip an angle by pi

(defun Adj (X)
  (a X (| pi*2 X))
  (if (gt X pi) (a X (- X pi*2)))
  (if (lt (| X) #CT) 0) X)

(defun AngBtwxt (A B C)
  "Is angle C between A and B"
  (if (or (eq (sgn* A) (sgn* B)) (le A B)) (and (ge C A) (le C B))
    (and (ge (| pi*2 C) A) (le (| pi*2 C) (+ pi*2 B)))))
     
