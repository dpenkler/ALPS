; -*- mode: emacs-lisp; -*-

; cwcc tests

(defun TCC (X N) ;; R is free here so we can play with it in TT
  ;; (TCC '(a b c end) 3)    ;; no continuation called
  ;; (TCC '(a b c r end) 3)  ;; continuation called at r
  (unless (listp X) (error "TCC wants a list"))
  (if (zerop N)
      (let ()
	(print "start")
	(cwcc '(lambda (V) (setq R V)
		 (print "about to recurse")
		 (REC X)
		 (print "recursion completed")))
	(print "returned from cwcc"))
    (TCC X (- N 1))
    (print (list "TCC finished" N)))
  'TCC-done)

(defun REC (X) ; R is free here => the continuation captured in TCC
  ;; REC recurses normally until it invokes the continuation R
  ;; which rips it out of its recursion stack and returns on the
  ;; captured continuation stack
  (cond ((null X) nil)
	((eq (car X) 'r) (R (print (list 'this 'is 'my X))))
	(t (print (car X)) (REC (cdr X)))))

(defun TT (X) ;; Demo of invoking a continuation in the primitive: list
  (print (list 'this 'is (R X) 'ok?))) ;; R is the R captured in TCC above

(defun TMCCWCC ()  ; Test mappcar with cwcc
  (let* ((A '(a b c d e f g h i j k l m n o p q r s t u v w))  
	 (F '(lambda (X) X))  K
	 (CC '(lambda () 
		(let ((W (mapcar '(lambda (X) 
				    (cond ((eq X 'k) 
					   (cwcc '(lambda (Y) (a K Y)
					   (list 'Captured X))))
					  ((list 'Hello X)))) A))) (F W)))))
    (cond ((and 
	    (findc 'car (CC) 'Captured)
	    (equal (subst 'k 'Polly (mapcar 'cadr 
					    (cwcc '(lambda (Z) 
						     (setq F Z)
						     (K '(Hello Polly))))))
		   A))
	  "TMCCWCC Rocks")
	  ("TMCCWCC Failed"))))

(defun coroutine ()
 (let
  ((local-state
    '(lambda (firstv) (toggle 0) (toggle 1) (toggle 2)
       (finish "this is the end")))
;   (error '(lambda (X) (princ (fmt "Error: " X)) (terpri)))
   (toggle '(lambda (value)
	      (cwcc '(lambda (cc) 
		       (let ((olocal-state local-state))
			 (setq local-state cc)
			 (olocal-state value))))))
   (finish 
    '(lambda (fin)
       (let ((last-exit local-state))
	 (setq local-state
	       '(lambda (rv)
		  (error (fmt "expired coroutine with rv: " rv))))
	 (last-exit fin)))))
  (fun toggle)))

(defun TCOR () (let ((R1 (coroutine)) (R2 (coroutine)))
		 (FOR I 1 10 (print (list (R1 I) (R2 I))))))

(defun TCWCCN (N) 
  (prog (F K FK captchacont (I 1))

	(defun captchacont () 
	  (let ((X (* .5 (* 2 (+ 1 2 3 (cwcc '(lambda (X) (setq K X) 4)) 5)))))
	    (F X)))

	(defun F (X) (list 'captchacont X))
	
	(defun FK (X) (cwcc '(lambda (Y) (setq F Y) (K X))))
	
	(print (captchacont)) 
	
	(while (lt I N)  (print (list I (FK I))) (incr I))))

(defun TCWCCP (N I) ; test cwcc based on communicating processes
  (prog 
   (RUN K SUSP (ENV '((A 1) (B 2) (C 'gello))) E  F 
	ProcTab ; process table
	Stat    ; correctly terminated processes added here in rme
	Disp Run msend SR sf findProc addProc mkp  MyProg Ping Pong Hector)
    
   ; K is the continuation captured in invocation of RUN 
   ; SUSP is the continuation captured when MyProg invokes msend
   ; when this continuation is invoked the msend
   ; invocation in MyProg returns with the value
   ; passed in the invocation of SUSP

   (defun Disp (P) (cwcc '(lambda (V) (P V)))) ; returns (Kont X)

   (defun Run (N) ; this is the "OS"
     (prog ((L (append ProcTab)) ; make a copy to play with 
	    T S (I 0))
	   (while (setq T L) ; running processes
	     (mapc '(lambda (X)
		      (setq S (Disp (cadr X)))
		      (cond (S (se '(procio) (cadr X)))
			    (t  (setq L (delete X L)))) ; remove
		      (cond ((gt (incr I) N) 
			     (return (list "done after" I "Iterations")))))
		   T))
	   (cond ((SETEQ Stat (mapcar 'car ProcTab)) "TCWCCP rocks")
		 (t "TWCC sucks"))))
   
   (defun msend (X) (cwcc '(lambda (V) (setq SUSP V) ; capture prog cont
			    (K X) ; return to RUN
			    )))
	
   (defun SR (R) (princ (fmt Name (incr N):5 "  result was " )) (print R))
   (defun sf (F A E) (eval (list 'apply F (list 'quote A)) (caddr E)))
   (defun findProc (Name) (cadr (assoc Name ProcTab)))
   (defun addProc (Name Ent) (setq ProcTab 
				   (append ProcTab (list (list Name Ent)))))
   (defun mkp (Name Code Env) ;; make a process 
     (prog ((Kont) SUSP Self Res (IQ (mkdll)) (OQ (mkdll)) ; input & output Q's
	    (addEnv  '(lambda (X) (setq Env (cons X Env))))
	    (getEnv  '(lambda (X) (cadr (assoc X Env))))
	    (setKont '(lambda (X) (setq Kont X))); X comes from calling env
	    (msend   '(lambda (D M) ; dest message
			(puthead OQ (cons Name (cons D M))) ; (from to m)
			(cwcc '(lambda (V) (setq SUSP V) ; capture prog cont
			    (Kont (list 'msend Name)) ; return to RUN
			    ))))
	    (mrecv   '(lambda ()  ; returns (from m)
			(let (M)
			 (cwcc '(lambda (V) (setq SUSP V)))
			 (cond ((peektail IQ) (setq M (gettail IQ))
				(cons (car M) (cddr M))); drop the to
			      (t (print "nuffink")
				 (Kont (list 'mrecv Name)))))))
	    (putq    '(lambda (X) (puthead IQ X)))
	    (procio  '(lambda ()
			(let (M D)
			  (while (setq M (gettail OQ))
			    (cond ((setq D (findProc (cadr M)))
				   (sf 'putq (list M) D)) ; send to dest
				  (t (print (list "dest for " M " not found"))))
			    ))))
	    (rme '(lambda (X)
		    (setKont X)
		    (cond (SUSP (SUSP Res)))   ; if suspended resume - funky ?
		    (setq Res (Code Name))
		    (setq Stat (cons Name Stat)) ; add to succesful comp list
		    (cwcc '(lambda (V) (setq SUSP V) (Kont Res)))
			; SUSP now here from which there is no escape
		    (Kont nil) ; nil return says i'm dead to OS
		    ))) 
	   (cond ((assoc Name ProcTab) 
		  (list Name "already exists") 
		  (return nil)))
	   (setq Self (fun rme) )    
	   (addProc Name Self) ; add me to the proctable
	   Self ; return one's self
	   ))

   (defun MyProg (Name)
     (prog ((N 0))
	   (print (list Name 'Started))
	   (SR (msend "hello peter"))
	   (print (COUNT TPROC))
	   (SR (msend '(a b c)))
	   (print (i 10)) 
	   (SR (msend 'Goodbye))
	   'ok))
   
   (defun Ping (Name)
     (prog ((N (getEnv 'Turns)) (Dest (getEnv 'Dest)) Mes)
	   (princ (fmt Name " Started for " N " Turns")) (terpri)
	   (FOR I 1 N 
		(msend Dest (list 'MSG Name I)) ;;
		(setq Mes (mrecv)))
	   (msend Dest (list 'QUIT Name))
	   (princ (fmt Name " Done")) (terpri)
	   'ok ))
    
   (defun Pong (Name)
     (prog ((Mes) Dest Type Body)
     (princ (fmt Name " Started")) (terpri)
     (while (setq Mes (mrecv))
       (setq Dest (car Mes) Type (cadr Mes) Body (cddr Mes))
;       (princ (fmt Name " Got  ")) (print Mes)
       (cond ((eq Type 'MSG) (msend Dest (list 'ACK Name (cadr Body))))
	     ((eq Type 'QUIT) (princ (fmt Name " Done")) (terpri) (return 'ok))
	     (t (print (list "crazy message" Mes)))))
     'ok ))

   (defun Hector ()
     (setq E ENV)
     (while (ne 'ok (cwcc '(lambda (V) (setq K V) (MyProg 'Hector))))
       (setq F (car E))
       (setq E (cdr E))
       (SUSP F) 
       (print "never executed"))
     )

  (setq P1 (mkp 'Ping Ping nil)) ; P1 & P2 leak out
  (setq P2 (mkp 'Pong Pong nil))
  (sf 'addEnv (list (list 'Turns N)) P1)
  (sf 'addEnv (list (list 'Dest 'Pong)) P1)
  (Run I)
))

(defun weird () ; a cwcc obfuscation cribbed from scheme
  (let ((C 'cwcc))
    (apply 
     '(lambda (X Y) (X Y))
     (mapcar ((lambda (R) 
		(fun ((C C) '(lambda (S) (R (fun (lambda (L) ((S S) L))))))))
	      '(lambda (F)
		 (fun (lambda (L) 
			(if (null L) C 
			  (fun (lambda (K) 
				 (princ (car L))
				 ((F (cdr L)) (C K)))))))))
	     '(("D" "v" " " e k e ".") (a e P n l r "\n"))))) t)

(defun control-state (ret)
  (mapc '(lambda (element)
	   (setq ret
		 (cwcc
		  '(lambda (resume-here)
		     ;; Grab the current continuation
		     (setq control-state resume-here)
		       (ret element)))))
	lst)
  (ret 'you-fell-off-the-end))

(defun generate-one-element-at-a-time (lst) (cwcc control-state))
 
(defun gdgt ()  (generate-one-element-at-a-time '(0 1 2 3 4 5 6 7 8 9)))

(defun mks () (let ((cc 0)) (fun (lambda () (setq cc (+ cc 1)) cc))))

(defun tmks () (let ((egs (mks)) (egs1 (mks)))
		 (FOR I 1 10 (print (list  (egs1) (egs))))))

(defun ACCT () ; functional value tutorial
  (let ((bal 100) ; initial balance
	(Show '(lambda (X) (princ X) (princl (fmt " => " (eval X))))))
    (setq getbal (fun (lambda () bal))) 
    (setq dep (fun (lambda (amt) (setq bal (+ bal amt)))))
    (defun withdraw (amt) (dep (- amt))) ; global
    (Show '(getbal))
    (Show '(dep 50))
    (Show '(getbal))
    (Show '(withdraw 108))
    '(Now try it yourself)))

(defun tcc (X)
 ; Guy Steele's  (pathological) little test program in alps
 ; from http://lib.store.yahoo.net/lib/paulgraham/cint.lisp
 ; (tcc 'start | 'froz | 'next | 'last)
  ((cwcc
    '(lambda (goto)
       (let* ((start (fun (lambda () (print "start") (goto next))))
	      (froz  (fun (lambda () (print "froz")  (goto last))))
	      (next  (fun (lambda () (print "next")  (goto froz))))
	      (last  (fun (lambda () (print "last")  (* 2 3 (+ 3 4))))))
	 (eval X))))))
