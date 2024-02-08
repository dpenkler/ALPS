;; Time-stamp: <2024-02-07 22:16:41 dave>
;; -*- mode: emacs-lisp; -*-
;; princl, listf and ll
;; prime stuff moved to prime.al
;; exp and backquote changes
;; more trig in degrees
;; hasCAP becomes HASCAPS
;; using #CP in SHOWCAPS
;; moved set stuff to set.al
;; moved graf stuff to graf.al
;; Added FindFun which find arg in all loaded files
;; Added twoc
;; fixed phys
;; ToUpper/ToLower become cap/low
;;

(a defun 'de defexpr 'df defmacro 'dm)
(df $ (a b) (list* 'lambda a b))
; character sets
(a #p (chr (i (+ 32 [0 95]))))          ; printable characters
(a #d "0123456789")                     ; digits
(a #lc (chr (i {(num "a") (num "z")}))) ; lower case alphabetic
(a #uc (chr (i {(num "A") (num "Z")}))) ; upper case alphabetic
(a #a (cat #uc #lc))
(a #ss "'\"`,;:\\()[]{}")              ; special symbol chars
(a #sc (k (r '^ 1  (o '<> (ind #p {#d #a #ss}) (i 96))) #p)) ; the rest
(defun isDigit (C) (onep (elt C #d)))
(defun isAlpha (C) (onep (elt C #a)))
;; Convert character array to lower case
(defun low (X) "Convert char arg X to lower case"
    (let ((N (num X))) (chr (+ N (* 32 (^ (> N 64) (< N 91)))))))
;; Convert character array to upper case
(defun cap (X)  "Convert char arg X to upper case"
    (let ((N (num X))) (chr (- N (* 32 (^ (> N 96) (< N 123)))))))

(defun Printable (C X) "Replace by C the non-printable chars in X"
  (let ((T (rav X)))
    (aset T C (k (v (< (num T) 32) (> (num T) 127)) (i (p T))))
    (p (p X) T)))

(defun getConst (S) "Get constant S from global environment"
  (if (symbolp S) (eval S nil) (error "(getConst <name>:sym)")))

(defconst #hexdigs "0123456789ABCDEF")
(defun hex (S) "Convert hex digits in string S to number"
  (if (onep (r '^ (elt S #hexdigs))) (dec 16 (- (ind #hexdigs S) #IO))
    (error "invalid hex digits")))
(defun n2hex (N) (aref #hexdigs (+ (enc (p (c (l 16 (+ N 1))) 16) N) #IO)))

(defconst #octdigs "01234567")
(defun oct (S) "Convert octal digits in string S to number"
  (if (onep (r '^ (elt S #octdigs))) (dec 8 (- (ind #octdigs S) #IO))
    (error "invalid octal digits")))
(defun n2oct (N) (aref #octdigs (+ (enc (p (c (l 8 (+ N 1)))  8) N) #IO)))

(defconst #bindigs "01")
(defun bin (S) "Convert binary digits in string S to number"
  (if (onep (r '^ (elt S #bindigs))) (dec 2 (- (ind #bindigs S) #IO))
    (error "invalid binary digits")))
(defun n2bin (N) (aref #bindigs (+ (enc (p (c (l 2 (+ N 1)))  2) N) #IO)))

;; use to define primitive recognised as such by dff
;;(defmacro defprim (N B) (a #prims (cons N #prims)) `(defun ,N ,@B))

(defun pwd (N) "generate password of N chars" (aref #p (? (p N (p #p)))))

(defun PR (X Y) ; does list X match the beginning or whole of list Y 
    (cond ((null X) t)
	  ((null Y) nil) 
	  ((eq (car X) (car Y)) 
	   (PR (cdr X) (cdr Y))) 
	  (t nil)))

(defun POS (X Y) (find ($ (Z) (PR X Z)) Y t)) ; position of list X in list Y

(defun ASORT (A) (aref A (gup A)))

(defun COPYLS (X) (cond ((null X) nil)
			((atom X) X)
			(t (cons (COPYLS (car X)) (COPYLS (cdr X))))))
(defun COUNT (X) 
  (cond ((null X) 0)
	((atom X) 1)
	(t (+ (COUNT (car X)) (COUNT (cdr X))))))

(defun COUNTW (X W) 
  ((label CWaux lambda (X)
	  (cond ((null X) 0)
		((atom X) (cond ((eq X W) 1) (t 0)))
		(t (+ (CWaux (car X)) (CWaux (cdr X)))))) X))

(defun LCOUNTF (X F) ; count non nil occ
  (let ((C 0)) (mapc '(lambda (X) (if (F X) (incr C))) X) C))

(defun clr () (prat 1 1 "\[[2J") t)

(defun big () (princ"\[]710;9x15medium,xft:fixed\G"))
       
(defun def (F) 
"construct string for prototype of function named by symbol F"
   {"(" F ((label M lambda (X) (cond ((null X) "") 
				     (t {" " (car X) (M (cdr X))}))) 
	   (cadr (eval F))) ")"})

(df doc (F) "Get doc string for F. For a symbol quote it."
    (let (Found
	  (FindDoc '(lambda (x)
		      (let ((F (open "doc/userdoc.txt")) L (N 1))
			(a Found nil)
			(while (and (not (eof F)) (not Found))
			  (a L (getl F))
			  (if (and (ge (p L) (p x)) (eql L (tk (p L) x)))
			      (a Found (- N 2))
			    (incr N)))
			(close F))
		      Found)))
      (cond ((null F)
	     (sys (fmt "emacsclient -e '(view-file \"doc/userdoc.txt\")'")))
	    ((gt (len F) 1) (error "doc: takes at most 1 arg"))
	    ((primp (car F))
	     (if (FindDoc (car F))
		 (sys (fmt "emacsclient -e '"
			   "(let () (find-file "
			   "\"doc/userdoc.txt\")"
			   "(goto-line " Found ")"
			   "(view-mode)"
			   "(recenter-top-bottom 0))'"))
	       (fmt "Sorry " (car F) " not documented yet")))
	    (t (a F (eval (car F)))
	       (cond ((atom F) (get F '#DOC))
		     ((member (car F) '(lambda fexpr macro))
		      (if (chrp (caddr F))
			  (list (cons (car F) (cadr F)) (caddr F))
			(cons (car F) (cadr F)))))))))
		   
(defun ASSOC (Y X) ; assoc with equal as opposed to eq on keys
    (car (find '(lambda (X) (equal (caar X) Y)) X t)))

(defun REPL (X Y Z) ; replace X in Y by Z (all lists)
    (prog (REPaux)
      (defun REPaux (R Y S)
	  (cond ((null R) (append Z (REPaux X Y nil)))
		((null Y) S)
		((eq (car R) (car Y)) (REPaux (cdr R) (cdr Y) (cons (car Y) S)))
		(t (append (rev (cons (car Y) S)) (REPaux X (cdr Y) nil))))) 
      (REPaux X Y nil)))

(defun Ddup (x) ; remove in order repetitions
  (mapn '(lambda (x) (if (equal (car x) (cadr x)) nil (list (car x)))) x))
 
(defun SortByLen  (X) (sort '(lambda (X Y) (lt (len X) (len Y))) X))

(defun FLAT (X)
  (cond ((null X) nil)
	((atom X) (list X))
	((null (cdr X)) (FLAT (car X)))
	(t (append (FLAT (car X)) (FLAT (cdr X))))))

(defun FLAT1 (X) ;; re-use singleton list in car
  (cond ((null X) nil)
	((atom X) (list X)) 
	((null (cdr X)) (cond ((atom (car X)) X) (t (FLAT (car X))))) 
	(t (append (FLAT (car X)) (FLAT (cdr X))))))

(defun Linear (L) ; same as FLAT, fewer gc's
  ((label LIN lambda (L R)
	  (cond ((null L) R) 
		((atom L) (cons L R)) 
		(t (LIN (car L) (LIN (cdr L) R))))) L nil))

(de lin (x) ; same as Linear but more efficient
    (cond ((null x) nil)
	  ((atom x) (list x))
	  (t (mapcan '(lambda (y)
			(cond ((null y) nil)
			      ((atom y) (list y))
			      (t (nconc (lin (car y)) (lin (cdr y))))))
		     x))))

(defun ATOMLIST (X) 
  (prog ((#FW 10) 
	 (M (ReduceL 'c (mapcar '(lambda (X) (p (car X))) X))))
;	 (M (r 'c (implod (mapcar '(lambda (X) (p (car X))) X)))))
	(mapc '(lambda (X) (cond (X (princ (car X)) (tab M) 
				    (prinl (cdr X)) (terpri)))) X) 'ok))

(defun LONGAL ()
    (let ((A (oblis)) (B 0) (C)) 
      (while A 
	(cond ((gt (len (car A)) B) 
	       (setq C (car A)) 
	       (setq B (len C)))) 
	(setq A (cdr A)))C))

(defmacro prog1 (prog1_parm) "Returns the result of evaluating the first form in prog1_parm"
    `(let ((prog1_parm ,(car prog1_parm)))
       ,@(cdr prog1_parm) prog1_parm))

(defmacro swp (x y) "Swap the values of symbols x and y"
	  `(a ,(car y) (prog1 ,x (a ,x ,(car y)))))

(defmacro swap (x y) "Swap the values of symbols x and y"
  `(let ((# ,x))
     (a ,x ,(car y) ,(car y) #)))

(defun TPERF (N)
  (cond ((HASCAPS "T") (stats t) (TQ N) (stats nil) (SS 4))
	(t "No stats capabililty")))

(defexpr Profile (Exp) "Show profile executing Exp"
  (cond ((HASCAPS "T") (unwind-protect
			   (let () (stats t) (stats) (a Exp (eval (car Exp))))
			 (print "Unwinding") (stats nil))
	 (SS 4) Exp)
	(t "No stats capabililty")))

(defun SS (N)
    (let ((FL $[car 
	       (lambda (X) (aref (cadr X) 1))
	       (lambda (X) (aref (cadr X) 2))
	       (lambda (X) (aref (cadr X) 3))
	       (lambda (X) (aref (cadr X) 4))])
	  (F 'car) (S (stats)) C T R CP TP (I #IO) (PS 0))
      (setq C (implod (mapcar 'cadr S)) T (implod (mapcar 'caddr S)))
      (setq CP (RP (/ (* 100 C) (r '+ C)) 2)
	    TP (RP (/ (* 100 T) (r '+ T)) 2))
      (setq R {(vtr C) (vtr CP) (vtr T) (vtr TP) })
      (mapc '(lambda (X) (rplacd X (list (aref R I ()))) (incr I)) S) ; hacked S
      (if (le N (p FL)) (setq F (aref FL N)))
      (a S (sort '(lambda (X Y) (lt (F X) (F Y))) S))
      (mapc '(lambda (X) (a PS (+ PS (aref (cadr X) 4) )) 
	       (nconc X (list (f PS)))) S)
      (ATOMLIST S)
      ;;(mapc '(lambda (X) (princl (fmt (car X):16
      ;;			      (implod (cdr X)):8:[0 2 0 2 0]))) S)
      (princl (fmt "Total instruction count: " (r '+ C) 
		   "; Total time : " (/ (r '+ T) 1e5)))
      ))

(df ed (X) (if (sys (fmt "emacsclient " (car X))) (load (car X))))

(defexpr edit (X)
  (prog (FNAME LINE)
	(a FNAME (FindFun (car X)))
	(cond (FNAME (a LINE (cadr FNAME) FNAME (car FNAME)))
	      (t 
	       (a FNAME (cat (car X) ".al") LINE 1)
	       (cond ((not (member "." #LP)) (a #LP (cons "." #LP))))
	       (print FNAME)
	       (eval (list 'pftof (car X) FNAME))))
	(if (sys (fmt "emacsclient " "+" LINE ":1 " FNAME)) (load FNAME))))


; Basic Object Oriented Functions
; An object is simply a closure aka funarg or functional value
; i.e. its a list that looks suchly: (funarg (lambda ()..) #<env>)
 
(defun se (X Y) (eval X (caddr Y)))  ; evaluate sexp X in object Y

(defun ms (F L) ; evaluate sexp F in list of ojects L
    (mapc (fun (lambda (X) (se F X))) L))

(defun jio (X) (se '(break) X)) ; Jump Into Object

(defun IPI (X) 
    (cond ((null X) nil)
	  ((atom X) X)
	  ((null (cdr X)) (car X)) ; remove bracket
	  ((null (cddr X)) (list (car X) (IPI (cadr X)))) ; monadic
	  ((null (cdddr X)) (list (IPI (cadr X)) (IPI (car X)) (IPI (caddr X))))
	  (t (append (list (IPI (cadr X)) (IPI (car X)))
		     (IPI (cons (IPI (car X)) (cddr X)))))))
(defun I2P (X) 
    (let ((P '((| 1 v)  (& 2 ^)
	       (< 3 <) (<= 3 <=) (== 3 =) (!= 3 <>) (>= 3 >=) (>  3 >)
	       (ind 4 i) (p 4 p) (% 4 %) (sin 4 sin) (cos 4 cos) (abs 4 |)
	       (+ 5 +) (- 5 -) (* 6 *) (/ 6 /) (^ 7 exp)
	       (= 0 setq) (+m 8 +) (-m 8 -)))
	  Prec Op IsOp I2Pr I2Paux I2Pscan)
      
      (defun Prec (X) (cond ((atom X) (cadr (assoc X P)))
			 (t 7))) ; list has highest precedence
      (defun Op (X) (caddr (assoc X P)))
      
      (defun IsOp (X) (member X '(- / abs sin cos tan sqr sqrt exp log i p %)))
      
      (defun I2Paux (X OP ARG) 
	  (cond ((IsOp (car X))
		 (I2Pscan (cddr X) OP 
			  (cons (list (Op (car X)) (I2Pr (cadr X))) ARG)))
		(t (I2Pscan (cdr X) OP (cons (I2Pr (car X)) ARG)))))
      
      (defun  I2Pscan (X OP ARG)
	   (cond ((and (null X) (null OP)) (car ARG))
		 ((and (not (null X)) 
		       (or (null OP) (gt (Prec (car X)) (Prec (car OP)))))
		  (I2Paux (cdr X) (cons (car X) OP) ARG))
		 (t (I2Pscan X (cdr OP) 
			     (cons (list (Op (car OP)) (cadr ARG) (car ARG))
				   (cddr ARG))))))
      
      (defun I2Pr (X) (cond ((atom X) X) (t (I2Paux X nil nil))))
      
      (I2Pr X)))

(defun APL () 
    (prog ((#PR "apl> ") X)
	  (while (and (prompt) (setq X (readl))) (print (eval (I2P X))))
	  "apl: exit."))

(defmacro apl (X) (IP X))

(defun type (X) 
    (cond ((listp X)   "list") 
	  ((primp X)   "primitive")
	  ((symbolp X) "symbol")
	  ((refp X)    "ref") 
	  ((nump X)    "num") 
	  ((cpxp X)    "cpx") 
	  ((chrp X)    "chr") 
	  (t           "?")))

(defun APL79BC ()
  (let ((#PW 128) (CHARS (LoadFont "data/font")))
    (mapc '(lambda (X) (princl (Banner X)))
	  '("HAR TALAS APL" "HER TALES APL" "APL HIC DICITUR" "APL SPOKEN HERE"
	    "HER SNAKKES APL" "ICI ON PARLE APL" "QUI SI PARLA APL"
	    "AQUI SE HABLA APL" "HIER SPRICHT MAN APL"))))

(de be (f g) `(lambda (x y) (,f x (,g y)))) ; APL beside operator

(defun ReduceL (F X) ; F dyadic on lists
    (let ((R (cond ((null X) nil)
		   ((null (cdr X)) (car X))
		   (t (F (car X) (cadr X))))))
      (map '(lambda (X) (setq R (F R (car X)))) (cddr X))
      R))

(defun ReduceLR (F X) ; F dyadic on lists APL semantics
    (let ((R (cond ((null X) nil)
		   ((null (cdr X)) (car X))
		   (t (F (cadr X) (car X))))))
      (map '(lambda (X) (setq R (F (car X) R))) (revl (cddr X)))
      R))

(defmacro Reduce (F X)
  (cond ((eq (len X) 3)
	 (if (ge (eval (cadr X)) 0) 
	     `(ReduceL ,F (explod ,(car X) ,(caddr X)))
	   `(ReduceLR ,F (explod ,(car X) ,(caddr X)))))
	((eq (len X) 2)
	 `(ReduceL ,F (explod ,(car X) ,(cadr X))))
	((onep (len X)) `(ReduceL ,F (explod ,(car X))))
	(t (error "too many arguments"))))

(defun ScanL (F X) ; F dyadic on lists
    (let ((R (cond ((null X) nil)
		   ((null (cdr X)) (car X))
		   (t (F (car X) (cadr X))))))
      (cons (car X) (cons R (mapcar '(lambda (X) (setq R (F R X))) (cddr X))))))

(defun outer (F X Y) ; F dyadic arrays
  (if ((symbolp F) (o F X Y)
    (prog* (PX PY (R (p {(a PX (p X)) (a PY (p Y))} 0)))
       (FOR I 1 PX (FOR J 1 PY (aset R (F (aref X I) (aref Y J)) I J)))R))))

(defun scalar (X) (p [] X)) ; return Scalar of X

(defun rReduce (F A) "recursively Reduce A with F along all dimensions"
       (cond ((zerop (rank A)) A) (t (rReduce F (r F A)))))

(defun zipv (L) "zip together vectors in list L" (rav (tr (implod .5 L))))

(defun vtr (x) "vector transpose"
  (if (not (onep (rank x))) (error "vtr expects vector")
    (p {(tally x) 1} x)))
;;
;; More self explanatory synonyms for selected APL primitives
;;
(setqq compress k)
(setqq expand   ex)
(setqq drop     dp)
(setqq take     tk)
(setqq shape    p)
(setqq ravel    p)
(setqq plus     +)
(setqq minus    -)
(setqq times    *)
(setqq divide   /)
(setqq mod      |)

(defun sqr (X) (* X X))

; outer product on lists 
(defun Outer (F X Y) (mapcar '(lambda (X) (mapcar '(lambda (Y) (F X Y)) Y)) X))

(defun ma (S) "make array of shape S with unique elements starting from #IO"
  (p S (i (r '* S))))

;; some basic constants in physics
(a c 299792458) ;schpeed of light m/sec
(a s 343)       ;" of sound in dry air @ 20 deg C
(defun wvln (F) (/ c F)) ; wavelength from frequency
(defun freq (L) (/ c L)) ; frequency from wavelength
(a h 6.62607004081e-34) ; planck's constant Js
(a h- (/ h (% 2))) ; reduced planck's constant
(a k 1.380650e-23) ; Boltzmann's constant J/K
(a cal 4.186) ; 1 calory = 4.18 joules
(a hp 735.49875) ; 1 hp in Watts or newton metres per second
;; Electrodynamics
(a e 1.602176620898e-19) ; Elementary electric charge
(a m0 (* (% 4) 1e-7)) ; magnetic constant or Vaccum permeability N/(m squared)
(a e0 (/ (* m0 c c))) ; electric constant or vacuum permittivity Farads/m
(a z0 (* m0 c))       ; impedance of free space
(a Fine-structure-constant (/ (sqr e) (* h- e0 c (% 4))))
;; Gravity
(a G 6.6740831e-11) ; gravitational constant m^3 kg-1 s-1
;; Temperature
(defun f2c (F) (* (- F 32) (/ 5 9))) 
(defun c2f (C) (+ 32 (/ (* 9 C) 5)))
;; Distance
(defun km (m) (/ (* m 1760 3 12 2.54) 1e5))
(defun mile (k) (/ (* k 1000) (* 1760 3 12 .0254)))
(defun metre (f) (* f 12 .0254))
(defun foot (m) (/ m (* 12 .0254)))
;; Astronomy
(a Earth2Moon 384400) ; km
(a Earth2Sun 149.6e6) ; km
(a Mars2Sun  210.19e6); km
(a EarthDiam 12742)   ; km
(a Earth2AlphaCentauri 4.3) ; ly
;; Maths
(defconst GR (/ (+ 1 (sqrt 5)) 2))  ; Golden Ratio

;; some basic stats
(defun min (X) (r 'f X))

(defun max (X) (r 'c X))

(defun sum (X) (rReduce '+ X))

(defun sumap (N D) "Sum arithmetic progression over N terms dist D"
  (/ (* N (+ (* N D) (- D) 2)) 2))

(defun norm (X) (/ X (r 'c (| X))))

(defun range (X) (- (min X) (max X)))

(defun avg (X) (/ (r '+ X) (c 1 (p X))))

(defun median (X) (nth (- (/ (p X) 2) 1) (sort 'lt (explod X))))

(defun meandev (X) (/ (r '+ (| (- X (avg X)))) (p X)))

(defun stddev (X) (sqrt (/ (r '+ (sqr (- X (avg X)))) (- (p X) 1))))

(defun corr (X Y) "full correlation of X with Y"
  (let* ((#IO 0) (A (norm X)) (V (norm Y)) (M (p A)) (N (p V))
	 (N-1 (- N 1)) (M+N-1 (+ M N-1)))
    (. '+ '* (tk {M+N-1 N}
		 (rot (i M+N-1) (p {M+N-1 M+N-1} (cat (p N-1 0) A))))
       V)))

(defun count (X MN MX CNT)
    (prog ((R (p CNT 0)) I (L (- (p X) 1)) (S (/ (- MX MN) (- CNT 1))) (#IO 0))
	  (FOR I 0 L (setq T (aref X I))
	       (if (or (gt T MX) (lt T MN)) nil
		 (setq T (/ (- T MN) S))
		 (aset R (+ (aref R T) 1) T))) R))

(defun diff (X) 
  "difference between successive elements of a Vector X (p res) = (- (p X) 1)" 
    (let ((L (- (p X) 1))) (- (tk (- L) X) (tk L X))))

(defun HI (X) ; Histogram
  (prog ((P (p X)) (M (r 'c X)) H) (a H (p M 0)) 
	(FOR I 1 P (a J (aref X I)) (aset H (+ (aref H J) 1) J)) H))
; (HI (implod (mapcar 'p (Linear (oblis))))) 

;; basic number ops

(defun twoc (X N) "Two's complement of X over N bits"
  (+ 1 (dec 2 (~ (enc (p N 2) (| X))))))

;; shorthand for some complex functions
(defun carg (X) (%  12 X))
(defun im   (X) (%  11 X))
(defun re   (X) (%   9 X))
(defun conj (X) (% -10 X))

;; simple math formulae
(defun sigmoid (X) (/ (- (exp X) 1) (+ (exp X) 1)))

(defun rootq (a b c) "roots of a quadratic ax^2+bx+c"
       (implod (mapcar '(lambda (op)
			  (/ (op (- b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a)))
	  '(+ -))))

(defun sumseq (n) "sum of seq of first n integers"
       (/ (* n (+ n 1)) 2))

(defun sumsubs (n m) "sum of all j's for j from m to n"
       (/ (* (+ n m) (- n m -1)) 2))

;; (revsum (subseq n)) ;=> n
(defun revsum (k) "seq of integers that sum to k"
       (/ (- (sqrt (+ (* 8 k) 1)) 1) 2))

(defun revsumsub (k m) "seq of integers from m that sum to k"
       (scalar (tk 1 (rootq 1 1 (- m (sqr m) (* 2 k))))))

(defun REP (X N) (cond ((zerop N) nil) (t (cons X (REP X (- N 1))))))

;;
;; Time and date utilities
;;
(defun DATE () (FD (date)))

(defun USDATE () (FD (date (- (clock) (* 9 3600)))))

(defun FMTDATE (T)
    (let ((M (p [12 3] "JanFebMarAprMayJunJulAugSepOctNovDec"))
	  (#PP 0))
      (fmt (aref M (+ (aref T [5]) 1) ())
	   " " (aref T 4):-2 (+ 1900 (aref T 6)):5
	   " " (aref T 3):-2 ":" (aref T 2):-2 ":" (aref T 1):-2)))


(defun FMTNDATE (T) 
    (implod (mapcar '(lambda (X Y) (tk -2 (fmt (+ Y (aref T X))))) 
		    '(4 5 6) '(100 101 1900))))

(defun FD (D) "Format Date nicely into YYYY/MM/DD HH:MM:SS"
       (let ((#IO 1))
	 (fmt (+ (aref D 6) 1900):4:0 "/" (+ 1 (aref D 5)):-2:0 "/"
	      (aref D 4):-2:0  " " (aref D 3):-2:0 ":" (aref D 2):-2:0 ":"
	      (aref D 1):-2:0)))


;;
;; Len
;;

(defun clen (X) (cond ((null X) 0)(t (clenaux X (cdr X) 1))))

(defun clenaux (X Y Z) (cond ((null Y) Z)
			  ((null (cdr Y)) (+ Z 1))
			  ((eq X Y) (list 'circular_list 'at Z))
			  (t (clenaux (cdr X) (cdr(cdr Y)) (+ Z 2)))))

(defun ML (N) (prog (P) (setq P (explod (i N))) (rplacd (last P) P) (clen P)))

(defexpr ls (X) (colprt (sort 'lt (apply 'dl X))))
(defun al () (colprt (FLAT (oblis))) "(al) done")
(defexpr ps (X) (sys "ps " (cond ((null X) "") ((car X)))))
(defun princl (X) (princ X) (terpri))

;;
;; File system utilities
;;

(defun listf (F S W) 
    (let ((R "-UGSrwxrwxrwx") (T "fc?d?b?-?l?s???") (Ind [9 2 3 1]))
      (if S (fmt (aref T (nth 8 S))
		 (aref R (+ #IO (* (i 12) (enc (p 12 2) (nth 7 S))))) 
		 (implod (mapcar '(lambda (X) (fmt " " (nth (aref Ind X) S):
						  (aref W X))) (explod (i 4))))
		 " " (FD (date (nth 5 S)))  ; mtime
		 " " F))))                  ; name

(defexpr ll (F) 
  (a F (cond ((null F) ".")
	     ((chrp (car F)) (car F))
	     ((error "ll bad file name"))))
  (let* ((SL (dl F))
	 (FL (if (listp SL) (sort '(lambda (X Y) (lt (low X) (low Y))) SL) SL))
	S L (W (p 4 0)) (Ind [9 2 3 1]) (#IO 1) )
    (cond ((eq SL F) (princl (listf F (st F) W)))
	  ((null SL) (error "no such file or directory"))
	  (t (a L  (mapcar '(lambda (X) (st {F "/" X})) FL))
	     (FOR I 1 4
		  (aset W 
			(ReduceL 'c
				(mapcar 
				 '(lambda (X) (p (fmt (nth (aref Ind I) 
							   X)))) L))
			I))
	     (mapc '(lambda (X Y) (princl (listf X Y W))) FL L)))))


(defexpr lls (Suffix Dir)
  "Generate list of files with Suffix in optional dir Dir"
  (let ((A (cond ((gt (len Dir) 1) (error "(lls Suffix [Dir])"))
		 ((null Dir) (dl))
		 (t (DL (car Dir)))))
	(L (- (p Suffix))))
    (sort 'lt (mapcan  '(lambda (X) (if (eql (tk L X) Suffix) (list X))) A))))

(defun SUM () 
  (let ((X 0) Y) 
      (while (setq Y (read)) 
	(print (setq X (+ X Y))))))

(defmacro macroexpand (F B) 
    (if (or (not (symbolp F)) (ne 'macro (car (eval F))))
	(error (fmt F " is not a macro")))
    `((fexpr ,@(cdr (eval F))) ,@B))

(defmacro FOR (X) 
  (cond ((eq (caddr X) 'DOWNTO) ;; (FOR I 10 DOWNTO 1 ...)
	  `(let ((,(car X) ,(cadr X)))
	     (while (ge ,(car X) ,(car (cdddr X)))
	       ,@(cdddr X) 
	       (decr ,(car X)))))
	 ((eq (cadr X) 'IN)      ;; (FOR I IN list ...)
	  `(mapc '(lambda (,(car X)) ,@(cdddr X)) ,(caddr X)))
	  (t `(let ((,(car X) ,(cadr X)))  ;; (FOR I 1 10 ...)
		(while (le ,(car X) ,(caddr X))
		  ,@(cdddr X) 
		  (incr ,(car X)))))))

(defun fe  (F X) (implod  (mapcar F (explod X))))
(defun fe1 (F X) (implod .5 (mapcar F (explod 1 X))))
(defun fe2 (F X) (implod .5 (mapcar F (explod 2 X))))

(defun FE (F X) ; TODO add axis 
  (let ((D (p (p X) (p 0 X)))) (FOR I 1 (p D) (aset D (F (aref X I)) I)) D))

(defmacro printl (printl_args) `(print (list ,@printl_args)))

(defun HASCAPS (X) (onep (r '^ (elt X #CP))))

(defun sc () 
    (let ((CapAlist '(
		      ("B" "Internal Debug")
		      ("C" "Check Arguments")
		      ("D" "Debug/Trace for Lisp")
		      ("E" "Experimental")
		      ("F" "Functional Values")
		      ("G" "Graphics")
		      ("I" "Instrument Control")
		      ("L" "Try/Recover")
		      ("N" "Network Support")
		      ("P" "Preemption")
		      ("M" "System timer")
		      ("R" "Range Checking")
		      ("S" "Sound")
		      ("T" "Trace Statistics")
		      ("W" "Workspace Persistence")
		      ("c" "Deep Binding CVC")
		      ("d" "Deep Binding")
		      ("s" "Shallow Binding")
		      ("t" "Supress Tail Recursion")
		      ("j" "Complex numbers")
		      )))
      (princ (fmt "Capabilities: " #CP "\n"))
      (mapc '(lambda (X) (tab 3) (princ (cadr (assoc X CapAlist))) (terpri)) 
	    (explod #CP)) t))

(defun SPC () ; Show performance counters
  (let ((W (r 'c (implod (mapcar '(lambda (X) (p (car X))) #PC)))))
    (incr W)
    (mapc '(lambda (X) (prin0 (car X)) (tab W) (princl (cdr X))) #PC)))

(defun RPC () (mapc '(lambda (X) (aset (cdr X) 0 ())) #PC)) ; Reset PCs

(defun LOG (L) (princ (fmt (DATE) " " L "\n") #LF) L)

;;
;; Debug helpers (should go into a separate file
;;

(defexpr ShowV (L); Show value of variables in list L using optional function F
  ;; eg to show the value of A1 A2 A3 in degrees rather than
  ;; their actual value in radians (ShowV  deg (A1 A2 A3))
  (let ((F 'arg)) ; builtin that returns its argument
    (if (gt (len L) 1) (a F (eval (car L)) L (cadr L)) (a L (car L)))
    (print (mapcan '(lambda (X)
		      (if (or (listp X) (symbolp X)) (list X (F (eval X)))
			(list X)))
		      L))))

(defexpr Brk (F) ; set breakpoint on function F
    (let ((Name (car F)) (Exp (eval (car F))))
      (if (member (car Exp) '(lambda macro fexpr))
	  (and (set Name
		    (cons (car Exp)
			  (cons (cadr Exp)
				(cons (list 'break 
					    (cons Name (cadr Exp)))
				      (cddr Exp)))))
	       (fmt "Brk enabled on function " Name))
	(fmt "Can't enable Brk on function " Name))))

(defexpr Krb (F) ; Remove breakpoint on function F
    (let ((Name (car F)) (Exp (eval (car F))))
      (if (not (equal (caddr Exp) (list 'break (cons Name (cadr Exp)))))
	  (list "Brk not enabled on function " Name)
	(set Name (cons (car Exp) (cons (cadr Exp) (cdddr Exp))))
	(fmt "Brk disabled on function " Name))))

(a alpsTraceCount 0)
(defun alpsTraceFun () (princ (fmt "Trace" (incr alpsTraceCount):-4 ": ")))

(defexpr Trace (F) ; set tracepoint on function F
    (let ((Name   (car F)) 
	  (Exp    (eval (car F))) 
	  (Body   (cddr (eval (car F))))
	  (Result (gensym)))
      (if (member (car Exp) '(lambda macro fexpr))
	  (and (set Name
		    `(,(car Exp) ,(cadr Exp)
		      (alpsTraceFun)
		      (print (cons ',Name (list ,@(cadr Exp))))
		      (a ,Result (let () ,@Body))
		      (princ (fmt "Trace" alpsTraceCount:-4 "=>"))
		      (decr alpsTraceCount)
		      (print ,Result)))
	       (fmt "Trace enabled on function " Name))
	(fmt "Can't enable Trace on function " Name))))

(defexpr Ecart (F) ; Remove tracepoint on function F
    (let ((Name (car F)) (Exp (eval (car F))))
      (if (not (equal (caddr Exp) '(alpsTraceFun)))
	  (list "Trace was not enabled on function " Name)
	(set Name (cons (car Exp) 
			(cons (cadr Exp) 
			      (cddr (caddr (nth 4 Exp))))))
	(fmt "Trace disabled on function " Name))))

(defexpr TIME (X) (let ((T1 (time))) (print (eval (car X))) (- (time) T1)))
(defexpr Time (X) (let ((T1 (time))) (eval (car X)) (- (time) T1))) ; no print
(defexpr ETIME (X) ; like time with elapsed also
  (let ((T1 (time)) (E1 (clock)))
    (print (eval (car X)))
    (list  'Elapsed: (- (clock) E1) 'CPU: (- (time) T1))))

(defexpr CTIME (X) ; measure total CPU times
  (let ((S (open "/proc/stat")) T0 T1 R P)
	(a T0 (readl S)) (rewind S)
	(a P (eval (car X)))
	(a T1 (readl S))
	(a R (- (implod (cdr T1)) (implod (cdr T0))))
	(princ "    User    Nice  System    Idle  IOwait     Irq SoftIrq   Steal   Guest\n")
	(aset R (r '+ (aref R [9 10])) 9) ; accumulate guest times
	(princl (fmt (/ (tk 9 R) 100):8:2))))

(de colprt (L) "Column print list L to fit in console width #CW"
    (let* ((W (implod (mapcar '(lambda (X) (+ 1 (p X))) L)))
	   (Len (p W)) (WV) (#IO 1)      ; Length of L & Column Width Vector
	   (MW (+ #CW 1)) (CI 1)         ; Max width + 1 & Column index
	   (NC (r '+ (<= (s '+ W) MW)))) ; Number of columns
      (while (lt MW (r '+ (a WV (r 'c 1 (p {(c (/ Len NC)) NC}
					   (cat W (p (- NC (| NC Len)) 0)))))))
	(decr NC))
      (a WV (- WV (tk (- (p WV)) 1))) ; slim WV
      (FOR I IN L
	   (princ (fmt I:(aref WV CI)))
	   (when (gt (incr CI) NC) (a CI 1) (terpri)))
      (terpri (<> CI 1))))
    
(defun getf (Fname) "Return character array of contents of text file FName"
    (let* ((#IO 0) (EOL (chr 10)) C P (N 0) (L 0) (maxLen 0) (nLines 0)
	   (F (open Fname)))
      (while (a C (getc F))
	(cond ((eq C EOL) (a maxLen (c maxLen N) N 0) (incr nLines))
	      (t (incr N))))
      (rewind F)
      (cond ((zerop N) nil) (t (incr nLines) (setq N 0)))
      
      (setq P (p {nLines maxLen} " ") L 0 N 0)
      (while (a C (getc F))
	(cond ((eq C EOL) (incr L) (setq N 0))
	      (t (aset P C L N) (incr N))))
      (close F)
      P))

(defun readfl (N)
  "outputs list of lines read with getl for file with name N"
   (let (L (F (open N "r")))
	(while (not (eof F)) (a L (cons (getl F) L)))
	(close F) (nrev L)))

(defun readf (f)
  "outputs list of lines read with readl for file with name f"
   (let (L R) (a f (open f "r")) 
	(while (not (eof f)) (a R (readl f)) (if R (a L (append L (list R)))))
	(close f) L))

(a ERRNOS ())
(defun errno (X)
  (if (not ERRNOS) (a ERRNOS(readf "data/errnos")))
  (cond ((nump X) (car (findc 'cadr ERRNOS X)))
	((chrp X) (car (findc 'car ERRNOS X)))
	(t (error "errno needs number or name"))))

(defun dirp (N) ((lambda (X) (and (consp X) (null (car X)))) (st N)))

(defun filep (N) ((lambda (X) (and (consp X) (eq (car X) N))) (st N)))

(defun PickFiles (L C) 
  "Pick files in list L that match condition C on filename F"
  (mapcan '(lambda (F) (if (and (filep F) (eval C)) (list F))) L))

(defun Prefix (P L)  (mapcar '(lambda (X) (cat P X)) L))

(defun PrefixD (P L) 
  (mapcan '(lambda (X) (if (not (member X '("." ".."))) (list (cat P X)))) L))

(defun DLA (D) ; Directory list all with prefix
  (if (not (dirp D)) D
    (let ((P (if (eql (tk -1 D) "/") D (cat D "/")))) (Prefix P (dl D)))))

(defun DL (D) ; Directory list with prefix no . or ..
  (if (not (dirp D)) D
    (let ((P (cond ((eql D ".") "")
		   ((eql (tk -1 D) "/") D)
		   (t (cat D "/")))))
	  (PrefixD P (dl D)))))

(defun RDLC (D C) ; Recursive directory list of files with prefix and condition
  (let ((RDLAux '(lambda (D)
		   (cond ((and (filep D) (C D)) (list D))
			 ((and (dirp D) (not (member D '("." ".."))))
			  (mapcan RDLAux (DL D)))))))
    (cond ((dirp D) (mapcan RDLAux (DL D)))
	  (t (C D)))))

(defun GetDirs (D) "Get list of directories in D"
       (cond ((dirp D) (mapcan '(lambda (X) (cond ((dirp X) (list X)))) (DL D)))))

(defun GetLeafDirs (D) "Get list of directories in D containing no subdirectories"
       (let ((DD (GetDirs D))) (if DD (mapcan GetLeafDirs DD) (list D))))

(defun FindFun (F)
  "Search for F amongst files in load history"
  (prog* ((C  '(member (tk (- 1 (ind (rev F) "/")) F) #LH)) ; file loaded ?
	  (Files (mapcan '(lambda (D) (PickFiles (DL D) C)) #LP)))
	 (grep {" " F " "}Files)))

(defun SA () ; show all atoms
    (prog ((K (FLAT (oblis))) (N 0) (M 0) (#PD 3) (#LL 5) (C (chr 32)))
	  (while (and K (ne C 'q))
	    (cond ((and (boundp (car K)) (eval (car K)) )
		   (print (list (incr N) (car K) (eval (car K))))
		   (if (zerop (| 40 N)) (setq C (getc))))
		  (t (incr M)))
	    (setq K (cdr K)))M))

(defun HL () 
    (let ((#FW 10)) (princ (i (f (/ #PW 10)))) (terpri))
    (princ (p #PW (cat (p 9 "-") "|")))(terpri))

(defun Copyf (f1 f2) (let ((#PF (open f2 "w"))) (du f1) (close #PF)))

(defun Save (Vars Dest) 
    (cond ((or (not (symbolp Dest)) (not (listp Vars)))  "bad args") 
	  (t (set Dest (mapcar '(lambda (X) (list X (eval X))) Vars)) "ok")))

(defun Restore (X) 
    (mapc '(lambda (X) (print (car X)) (set (car X) (cadr X))) X))

(defun grep (P X)
    (mapcan '(lambda (X) 
	     (let ((F (open X)) (First nil) (Found nil) (Line 1)) 
	       (while (not (eof F))
		 (setq S (getl F))  
		 (if (zerop (ss P S)) ()
		   (if First () (a First t) (princl (fmt X ":")))
		   (princ (fmt S "\n"))
		   (a Found (list X Line)))
		 (incr Line)
		 )
	       (close F) Found))
	  X))

(defun fgrep (Fn P X)
    (mapc '(lambda (X) 
	     (let ((F (open X)) (First nil) (Pos)) 
	       (while (setq S (getl F))  
		 (if (zerop (a Pos (ss P S))) ()
		   (if First () (a First t) (princl (fmt X ":")))
		   (Fn Pos S))
		 )
	       (close F)))
	  X))

(defun FF (P S) 
    (let ((T (dp (+ P (p 'defun)) S)) U V) ; drop defun
      (a U (k (s 'v (<> " " T)) T))        ; drop leading spaces
      (a V (ss " " U))                     ; find first space
      (if (gt V 0) (intern (aref U (i (- V 1)))) nil)))

(defun TraceDefunsInFile (F) 
    (fgrep '(lambda (P S)  (princl (eval (list 'Trace  (FF P S)))))
	   'de (list F)))

(defun Title (T) (princ {(chr 27) "]2;" T (chr 7)}) (terpri 0))

(defexpr note (X) 
    (let ((F (open "note" "wa"))) 
      (mapc '(lambda (X) 
	       (if (gt (+ (lpos F) (plen X)) #PW) (terpri 1 F))
	       (princ X F) (princ " " F))
	    X)
      (if (gt (lpos F) 1) (terpri 1 F))
      (close F)))

(defun pg (F) 
    (prog* 
     ((Text (getf F)) (I 0) (L  (- #CH 3)) C (Run t)
      N (NL (aref (p Text) 1)) (IL (i NL))
      (Disp '(lambda (L)
	       (cond ((lt I 0) (a I 0)))
	       (let ((T (min (cat L (- NL I)))) L1 len)
		 (clr)
		 (FOR J 1 T 
		      (princ (fmt (if N (fmt (+ I J):4 " ") "") 
				  (ElimTB (aref Text (+ J I) ())) "\n")))
		 (a I (+ I T))))))
     (Disp L)
     (while Run
       (a C (getc))
       (cond ((eq C "q") (a Run nil))
	     ((eq C "P") (a I (max (cat 0 (- I (+ L 1))))) (Disp L))
	     ((eq C "N") (a I (- I (- L 1))) (Disp L))
	     ((eq C "p") (a I (scalar (- (tk -1 (k (< IL (- I L))   IL)) 1))) (Disp L))
	     ((eq C "n") (a I (scalar (- (tk  1 (k (> IL (- I L -1)) IL)) 1))) (Disp L))
	     ((eq C "/") (princ "search:") 
	      (a IL (k (> (ss (read) Text) 0) (i NL)))
	      (a I (scalar (- (tk 1 (k (> IL I) IL)) 1)))
	      (Disp L) (rclr))
	     ((eq C "b") (break pg))
	     (t (Disp L))))))

(defexpr fx (D)
  "select file to pg or dir from dir D"
  (require 'select)
  (let ((OD (if (null D) (wd)
	      (wd (if (symbolp (car D)) (car D) (eval (car D))))))
	(tkon/ '(lambda (X)
		  (mapcan '(lambda (X) (list (if (dirp X) {X "/"} X))) X)))
	DD)
    (unwind-protect 
       (while (a DD (SELECT "?" (tkon/ (cdr (sort 'lt (dl)))) 1))
          (cond ((filep DD) (pg DD)) ((dirp DD) (wd DD)) 
           (t (print DD) (error "unexpected entry")))) (wd OD))))

(defexpr pftof (Fun File)
  "Print function Fun to file [File] eg (pftof pftof \"pftof.al\")"
    (prog (#PF (L '((lambda defun) (fexpr df) (macro dm))) F)
	  (cond ((a F (assoc (car (eval Fun)) L)) ; izzit a function ?
		 (a #PF (open (car File) "w"))    ; quietly overwrite file
		 (princ ";; -*- mode: emacs-lisp; -*-\n")
		 (pp (cons (cadr F) (cons Fun (cdr (eval Fun)))))
		 (princ (fmt ";; End of file: " (car File) "\n")) 
		 (close #PF))
		(t (error "Bad function")))))
