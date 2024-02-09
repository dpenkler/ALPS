;;;-*- mode: emacs-lisp -*-
;; Quasi-Sesqui-Lisp: a LISP 1.5 evaluator in ALPS

(defun sqltest () 
  (prog (sqlapply sqleval sqlevcond sqlprogn sqlevalargs sqlookup sqlset sqlerror sqlisp initenv sqlenv 
		  sqlog sqres sqlresetenv)

(defun sqlapply (Fun Args Env)
  (cond	((atom Fun) (cond ((eq Fun 'car)   (caar Args))
			  ((eq Fun 'cdr)   (cdar Args))
			  ((eq Fun 'cons)  (cons (car Args) (cadr Args)))
			  ((eq Fun 'null)  (null (car Args)))
			  ((eq Fun 'atom)  (atom (car Args)))
			  ((eq Fun 'eq)    (eq (car Args) (cadr Args)))
			  ((eq Fun 'zerop) (zerop (car Args)))
			  ((eq Fun '+)     (+ (car Args) (cadr Args)))
			  ((eq Fun '-)     (- (car Args) (cadr Args)))
			  ((eq Fun 'set)  (sqlset (car Args) (cadr Args) Env))
			  ((eq Fun 'eval) (sqleval (car Args) Env))
			  (t (sqlerror "Invalid function" Fun))))
	((eq (car Fun) 'label) (sqlapply (cddr Fun) Args (cons (cons (cadr Fun) (cddr Fun)) Env)))
	((eq (car Fun) 'lambda) (sqlprogn (cddr Fun) (pairlis (cadr Fun) Args Env)))
	((eq (car Fun) 'sqlfunarg) (sqlapply (cadr Fun) Args (caddr Fun)))
	(t (sqlerror "Invalid Funtion" Fun))))

(defun sqleval (Form Env)
  (cond ((atom Form) (cond ((or (null Form) (eq t Form)) Form)
			   ((symbolp Form) (sqlookup Form Env))
			   (t Form)))
	((atom (car Form))
	 (cond ((member (car Form) '(car cdr cons null atom eq zerop + - set eval))
		(sqlapply (car Form) (sqlevalargs (cdr Form) Env) Env))
	       ((eq (car Form) 'quote) (cadr Form))
	       ((eq (car Form) 'fun)   (list 'sqlfunarg (cadr Form) Env))
	       ((eq (car Form) 'cond)  (sqlevcond (cdr Form) Env))
	       ((eq (car Form) 'progn) (sqlprogn (cdr Form) Env))
	       ((eq (car Form) 'setq)  (sqlset (cadr Form) (sqleval (caddr Form) Env) Env))
	       ((eq (car Form) 'alps)  (eval (cadr Form) Env))
	       (t (sqleval (cons (sqlookup (car Form) Env) (cdr Form)) Env))))
	(t (cond ((member (caar Form) '(lambda label sqlfunarg))
		  (sqlapply (car Form) (sqlevalargs (cdr Form) Env) Env))
		 ((eq (caar Form) 'fexpr)
		  (sqlprogn (cddr (car Form)) (cons (cons (caadr (car Form)) (cdr Form)) Env)))
		 ((eq (caar Form) 'macro)
		  (sqleval (sqlprogn (cddr (car Form)) (cons (cons (caadr (car Form)) (cdr Form)) Env)) Env))
		 (t (sqleval (cons (sqleval (car Form) Env) (cdr Form)) Env))))))

(defun sqlevcond (Clauses Env)
  (cond ((null Clauses) (sqlerror "Null cond clause" nil))
	((sqleval (caar Clauses) Env) (sqlprogn (cdar Clauses) Env))
	(t (sqlevcond (cdr Clauses) Env))))

(defun sqlprogn (Prog Env) 
  (cond ((null Prog) nil)
	((atom Prog) (sqleval Prog Env))
	((null (cdr Prog)) (sqleval (car Prog) Env))
	(t (sqleval (car Prog) Env) (sqlprogn (cdr Prog) Env))))

(defun sqlevalargs (Args Env) (mapcar '(lambda (X) (sqleval X Env)) Args))

(defun sqlookup (Sym Env) (prog ((R (assoc Sym Env))) (cond ((null R) (sqlerror "Unbound symbol: " Sym)) (t (cdr R)))))

(defun sqlset (Sym Val Env) (prog ((R (assoc Sym Env))) (cond ((null R) (nconc Env (list (cons Sym Val))))
							       (t (rplacd R Val))) Val))

(defun sqlerror (Err Info) (go 'SQLREPL))

(defun sqlisp () 
  (prog (sqlres sqlerror)
	(defun sqlerror (Err Info) (princ (fmt "sqlisp error: " Err)) (print Info) (break) (go 'SQLREPL))
	(sqleval '(setq end_of_programme nil) sqlenv)
	(sqleval '(defun quit () (setq end_of_programme t)) sqlenv)

	SQLREPL	(princ "sqlisp> ") 
	        (a sqlres (sqleval (read) sqlenv))
		(cond ((sqleval 'end_of_programme sqlenv) (princl "sqlisp: Done.") (return t))
		      (t (print (subst '#ENV sqlenv sqlres)) (go 'SQLREPL)))))

(setq initenv '(
	    (equal lambda (X Y) (cond ((atom X) (cond ((atom Y) (eq X Y)) (t nil)))
					  ((equal (car X) (car Y)) (equal (cdr X) (cdr Y)))
					  (t nil)))
	    (append lambda (X Y) (cond ((null X) Y) (t (cons (car X) (append (cdr X) Y)))))
	    (subst lambda (X Y Z) (cond ((atom Z) (cond ((eq Y Z) X) (t Z)))
					   ((equal Y Z) X)
					   (t (cons (subst X Y (car Z)) (subst X Y (cdr Z))))))
	    (mapcar lambda (F X) (cond ((null X) nil) (t (cons (F (car X)) (mapcar F (cdr X))))))
	    (ngenc  lambda (F X) (cond ((null X) nil) ((F (car X)) t) (t  (ngenc F (cdr X)))))
	    (list fexpr ($list) (mapcar (fun eval) $list))
	    (or   fexpr ($or)   (ngenc  (fun eval) $or))
	    (defun macro (X) 
	      (list 'progn 
		    (list 'setq (car X) (list 'quote (cons 'lambda (cdr X)))) 
		    (list 'quote (car X))))
	    (defclos macro (X) 
	      (list 'progn 
		    (list 'setq (car X) (list 'fun (cons 'lambda (cdr X))))
		    (list 'quote (car X))))
	    (if macro (X) (list 'cond (list (car X) (car (cdr X))) (cons t (cdr (cdr X)))))
	    ))

(setq sqlog (list
	  '(setq A '(a b c))
	  '(setq P '(list equal subst)) 
	  '(defun TEST (A) (if (atom A) (cons A nil) 
			     (setq A (append (subst '(Sukey take it off again) '(Polly put the kettle on) A) 
					     '(they have all gone away))) A))
	  '(list 'a 'b 'c (list 1 2 3 (TEST (list 'Polly 'put 'the 'kettle 'on))))
	  '(defun F (X Y Z) (cons X (Y Z)))
	  '(defclos G (X) (cons X Z))  
	  '(defun H (X) (cons X Z))
	  '(F 'Hello H 'Is)
	  '(F 'Hello G 'Is)
	  '(setq Z '(this is my Z ?))
	  '(F 'Hello G 'Is)
	  '((car (cdr P)) A '(a b c))
	  '(defun Queens (N) (FB nil nil N N nil))
	  '(defun Pair (X Y) (cond ((or (null X) (null Y)) nil) (t (cons (list (car X) (car Y)) 
									 (Pair (cdr X) (cdr Y))))))
	  '(defun TA (TR TC) ; Threat analysis
	     (cond ((null TR) nil)
		   ((or (eq MR (car TR)) (eq MC (car TC)) 
			(eq (- MR MC) (- (car TR) (car TC)))
			(eq (+ MR MC) (+ (car TR) (car TC)))) t)
		   (t (TA (cdr TR) (cdr TC)))))
	  '(defun FB (BR BC MR MC Res) ; Find Board
	     (cond ((zerop MC) (FB BR BC N N (cons (Pair BC BR) Res)))
		   ((zerop MR) (cond ((null BR) Res) (t (FB (cdr BR) (cdr BC) (- (car BR) 1) (car BC) Res))))
		   ((TA BR BC) (FB BR BC (- MR 1) MC Res)) ;under threat ->try next row
		   (t (FB (cons MR BR) (cons MC BC) N (- MC 1) Res))))        ; and try next column
	  '(Queens 4)
	  ))

(setq sqres (list
	  '(a b c) 
	  '(list equal subst) 
	  'TEST 
	  '(a b c (1 2 3 (Sukey take it off again they have all gone away))) 
	  'F 
	  'G 
	  'H 
	  '(Hello Is . Is) 
	  nil 
	  '(this is my Z ?) 
	  '(Hello Is this is my Z ?) 
	  t
	  'Queens
	  'Pair
	  'TA
	  'FB
	  '(((1 3) (2 1) (3 4) (4 2)) ((1 2) (2 4) (3 1) (4 3)))
	  ))

(defun sqlresetenv () (a sqlenv (append initenv nil)))  ; Make a clean copy of initial env

(sqlresetenv)
(print (if (equal sqres (mapcar '(lambda (X) (prog () (return (sqleval X sqlenv)) SQLREPL (print X) nil)) sqlog)) 'ok "sqltest failed"))
(sqlisp)
))
