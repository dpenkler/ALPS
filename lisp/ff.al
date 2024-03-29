;;;-*- mode: emacs-lisp -*-
;;;
;;; Find free variables
;;;

(defun FindFree (F Vars Detail)
  "Find free variable refences in expression F with initial list of bound variables Vars"
  (let ((FreeVars nil) Parse CheckSym AddVars AddVars* ParseList ParseCond GetLabels T)
    (de AddVars  (V mVars) (cond ((null V) mVars) ;; Vars free here 
				 ((atom (car V)) (AddVars (cdr V) (cons (car V) mVars)))
				 ((atom (caar V)) (Parse (cadar V) Vars)  (AddVars (cdr V) (cons (caar V) mVars)))))
    (de AddVars* (V Vars) (cond ((null V) Vars)
				((atom (car V)) (AddVars* (cdr V) (cons (car V) Vars)))
				((atom (caar V))  (Parse (cadar V) Vars) (AddVars* (cdr V) (cons (caar V) Vars)))))
    (de ParseList (L Vars) (cond ((null L) nil)
				 (t (Parse (car L) Vars)
				    (ParseList (cdr L) Vars))))
    (de ParseCond (L Vars) (cond ((null L) nil)
				 (t (Parse (caar L) Vars)
				    (ParseList (cdar L) Vars)
				    (ParseCond (cdr L) Vars))))
    (de ParseSetq (L Vars) (cond ((null L) nil)
				 (t (cond ((symbolp (car L)) (CheckSym (car L)) (ParseList (cdr L) Vars))
					  (t (print L) (error "Assignment to non symbol")))
				    (ParseSetq (cddr L) Vars))))
    (de CheckSym (S) (when (and (not (null S)) (not (memq S Vars)) (not (memq S FreeVars)))
		     ;;  (print `(Got ,S in ,E))
		       (a FreeVars (cons S FreeVars))))
    (de GetLabels (P) (mapcan '(lambda (X) (atom X) (list X)) P))
    (de Parse (F Vars)
					; (print (list 'Parse F Vars))
	(cond ((null F) nil)
	      ((atom F) (cond ((eq F t) nil)
			      ((eq F :) nil)
			      ((symbolp F) (CheckSym F))))
	      ((atom (car F)) ;; (a E F)
	       (cond ((eq (car F) 'quote)) ; do nothing
		     ((memq (car F) '(let let* prog prog*))
		      (cond ((null (cdr F)) nil)
			    ((listp (cadr F))
			     (cond ((eq (car F) 'let) (ParseList (cddr F) (AddVars (cadr F) Vars)))
				   ((eq (car F) 'let*) (ParseList (cddr F) (AddVars* (cadr F) Vars)))
				   ((eq (car F) 'prog)
				    (ParseList (cddr F) (append GetLabels (cddr F) (AddVars (cadr F) Vars))))
				   ((eq (car F) 'prog*)
				    (ParseList (cddr F) (append GetLabels (cddr F) (AddVars* (cadr F) Vars))))
				   ))
			    (t (error "Parsing let"))))
		     ((memq (car F) '(lambda fexpr macro))
		      (ParseList (cddr F) (append (cadr F) Vars)))
		     ((eq (car F) 'label)
		      (ParseList (cdddr F) (append (list (cadr F)) Vars)))
		     ((eq (car F) 'cond) (ParseCond (cdr F) Vars))
		     ((memq (car F) '(de df dm defun defexpr defmacro))
		      (CheckSym (cadr F))
		      (when (and Detail (a T (FindFree (cddr F) (cons (cadr F)(caddr F)) t)))
			(princ (fmt (cadr F) " : ")) (print T)) ;; should rather collect these
		      (ParseList (cddr  F)  (append (caddr F) Vars)))
		     ((memq (car F) '(setq a)) (ParseSetq (cdr F) Vars))
		     ((eq (car F) 'setqq) (CheckSym (cadr F))) ;; ignore implicitly quoted value
		     ((primp (car F)) (ParseList (cdr F) Vars))
		     ((symbolp (car F)) (CheckSym (car F)) (ParseList (cdr F) Vars))
		     (t (print (list 'Untreated F)))))
	      (t (Parse (car F) Vars) (ParseList (cdr F) Vars))))
    
    (Parse F Vars)
    (revl FreeVars))) ;; return list of free variables in order of occurrence

(defun findFile (N) "Find file name N in load path #LP"
       (car  (ngenc '(lambda (X) (st {X "/" N ".al"})) #LP))) 

(defun procDefs (N) ;; TODO respect the order of the requires
  "Return list of defined symbols in file name N respecting (require)'s"
  (let* ((Files) Treated FF F RF T
	 (SysVars '(#AI #AL #CC #CH #CP #CR #CS #CT #CW #DB #DD #DL #DM #DP #DX #EP #EX #FM #FN #FS #FW #GB #GC #GH
			#GS #GW #HF #HS #IB #IO #KB #LF #LH #LL #LP #LS #LX #MI #MN #MX #NF #NM #NN #OB #OF #OP #PC
			#PD #PF #PL #PM #PN #PP #PR #PW #QQ #RA #RF #RL #RS #SD #SW #TR #VN #VP #WD #WN #WS))
	 (DefVars (mapcar '(lambda (X) (list X 'assg)) SysVars))
	 (CK '(lambda (X Typ) (cond ((a T (assoc X DefVars))
				     (princl (fmt "Warning: duplicate define " X))
				     (princl (fmt "         of type " Typ " in " NF))
				     (princl (fmt "     was of type " (cadr T) " in " (caddr T)))
				     (rplaca (cdr T) Typ)
				     nil)
				    (t))))
	(NF (findFile N)))
    (unless NF (error "No such file"))
    (push NF 'Files)
    (while Files
      (unless (member (a NF (pop 'Files)) Treated)
	;;(print (list 'procDefs 'Reading NF))
	(a FF (open NF))
	(push NF 'Treated)
	(while (not (eof FF))
	  (a F (read FF))
	  (cond ((atom F) nil)
		(t (cond ((and (memq (car F) '(de defun)) (CK (cadr F) 'expr))
			  (push (list (cadr F) 'expr NF) 'DefVars))
			 ((and (memq (car F) '(df defexpr dm defmacro)) (CK (cadr F) 'fexpr))
			  (push (list (cadr F) 'fexpr NF)  'DefVars))
			 ((and (eq (car F) 'define) (CK (caadr F) 'expr))
			  (push (list (caadr F) 'expr NF) 'DefVars))
			 ((memq (car F) '(a setq))
			       (a F (cdr F))
			       (while F
				 (if (CK (car F) 'assg)  (push (list (car F) 'assg NF) 'DefVars))
				 (a F (cddr F))))
			 ((and (memq (car F) '(setqq defvar defconst)) (CK (cadr F) 'assg))
			  (push (list (cadr F) 'assg NF) 'DefVars))
			 ((eq (car F) 'require)
			  (when (ne (caadr F) 'quote) (princ "expected quoted arg in ") (print F)
				(error "Cannot handle require argument"))
			  (a RF (findFile (car (cdadr F))))
			  (unless RF (print RF) (error "Required file not found"))
			  (push RF 'Files))
			 ))))
	(close FF)))
    DefVars))

(defmacro FF (N) "Find free variables (FF Function-Name [Details-Flag])"
	  `(FindFree ,(car N) (list ',(car N)) ,(cadr N)))

(defun ShowFree (N)
  (let ((NF (findFile N)))
    (unless NF (print (list 'ShowFree N))(error "File not found"))
    (showFreeAux NF nil)))

(defun ShowUndef (N)
 (let ((NF (findFile N)))
    (unless NF (print (list 'ShowUndef N))(error "File not found"))
    (showFreeAux NF (mapcar 'car (procDefs N)))))
        
(defun showFreeAux (N D)
  "Show free variable references in defines and expression in file name N"
    (let ((FF (open N)) F L (#PL 66))
    (while (not (eof FF))
      (a F (read FF))
      (cond ((and (listp F) (memq (car F) '(de df dm defun defexpr defmacro)))
	     (when (a L (FindFree F (cons (cadr F) D) nil)) (princ (fmt (cadr F) ": ")) (print L)))
	    ((and F (a L (FindFree F D nil))) (princ "Free in ") (print F) (princ "  ==>   ") (print (FindFree F nil nil)))))
    (close FF)))
