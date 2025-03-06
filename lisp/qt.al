;; -*- mode: emacs-lisp; -*-
;;
;; File: qt.al
;; Desc: Quality / non-regression test
;; Vers: 7.68
;; Auth: dmp
;;
(if (not (boundp 'DEBUG)) (a DEBUG nil))
(require 'cl)
(require 'set)
(require 'genset)
(require 'dll)
(require 'idioms)
(require 'ttr)
(defun Dprint (X) (if DEBUG (print X) X))
(defun Dbreak (X) (if DEBUG (break X)))
(defun edqt ()
  (sys (fmt "emacsclient -e '(setq inferior-lisp-program \"alps\")'"))
  (a #DX '(let nil (prompt) (a #EX (read)) (rclr) (a #RS (eval #EX)) 
	       (cond ((or (nlistp #EX) (ne (car #EX) 'a)) 
		      (princ ";; => ") (print #RS))))))


(defun TEST ()
  (mapc 'require '(peval fib pi prime ))
  (TPEVAL 2)
  (TFIB)
  (TPI 90)
  (TPRIME 100)
  (TALCOMP)
)

(defun TALCOMP ()
   (let (A D R
	 (B (cat (/ #CT 1e4) (s '+ (p 4 (/ #CT 2)))))
	 (C (cat (exp 2 (exp 2 (i 9))) #MX))
	 (LC '(lt le eq ne ge gt))
	 (AC '(<  <= =  <> >= >)))
     (a A {(- (rev C)) (- (rev B)) 0 B C})
     (mapc '(lambda (LCX ACX)
	      (princ LCX)
	      (a D (o ACX (i (p A)) (i (p A))))
	      (FOR I 1 (p A) 
		   (FOR J 1 (p A)  
			(a R (onep (aref D I J)))
			(if (not (and
				  (eq R (LCX I J))
				  (eq R (onep (ACX (aref A I) (aref A J))))
				  (eq R (LCX (aref A I) (aref A J)))))
			    (error (fmt ACX " - " LCX " mismatch at " 
					(aref A I) "  " (aref A J))
				   ))))) 
	   LC AC))
   " all good")

(defun TCOMP ()
  (cat "TCOMP "
  (prog (AC AN res ses
	 (F '(lt le eq ne ge gt))
	 (C1 '(""  a  b "a" "b" "ab"  "ba")) ; these literals are 
	 (C2 '(""  a  b "a" "b" "ab"  "ba")) ; different from these
	 (N1 '([]  1  2 [1] [2] [1 2] [2 1]))
	 (N2 '([]  1  2 [1] [2] [1 2] [2 1]))
	 (RC '((nil t t t t t t nil nil t nil t t t nil nil nil nil nil 
		   nil t nil nil t nil t t t nil nil nil nil nil nil t nil 
		   nil t nil t nil t nil nil nil nil nil nil nil) 
	      (t t t t t t t nil t t t t t t nil nil t nil t nil t nil t 
		 t t t t t nil nil t nil t nil t nil nil t nil t t t nil 
		 nil nil nil nil nil t) 
	      (t nil nil nil nil nil nil nil t nil nil nil nil nil nil nil
		 t nil nil nil nil nil nil nil t nil nil nil nil nil nil
		 nil t nil nil nil nil nil nil nil nil nil nil nil nil nil
		 nil nil nil)
	      (nil t t t t t t t nil t t t t t t t nil t t t t t t t nil t
		   t t t t t t nil t t t t t t t t t t t t t t t t)
	      (t nil nil nil nil nil nil t t nil t nil nil nil t t t t t 
		 t nil t t nil t nil nil nil t t t t t t nil t t nil t nil 
		 t nil t t t t t t t) 
	      (nil nil nil nil nil nil nil t nil nil nil nil nil nil t t 
		   nil t nil t nil t nil nil nil nil nil nil t t nil t nil t 
		   nil t t nil t nil nil nil t t t t t t nil)))
	 (RN '((nil t t t t t t nil nil t nil t t t nil nil nil nil nil 
		   nil t nil nil t nil t t t nil nil nil nil nil nil t nil 
		   nil t nil t nil t nil nil nil nil nil nil nil) 
	      (t t t t t t t nil t t t t t t nil nil t nil t nil t nil t 
		 t t t t t nil nil t nil t nil t nil nil t nil t t t nil 
		 nil nil nil nil nil t) 
	      (  t   nil nil nil nil nil nil nil t   nil nil nil nil nil
		 nil nil t   nil nil nil nil nil nil nil nil nil nil nil
		 nil nil nil nil nil nil nil nil nil nil nil nil nil nil
		 nil nil nil nil nil nil nil)
	      (nil t t t t t t t nil t t t t t t t nil t t t t t t t t t
		   t t t t t t t t t t t t t t t t t t t t t t t)
	      (t nil nil nil nil nil nil t t nil t nil nil nil t t t t t 
		 t nil t t nil t nil nil nil t t t t t t nil t t nil t nil 
		 t nil t t t t t t t) 
	      (nil nil nil nil nil nil nil t nil nil nil nil nil nil t t 
		   nil t nil t nil t nil nil nil nil nil nil t t nil t nil t 
		   nil t t nil t nil nil nil t t t t t t nil)))

	 (TLFO '(lambda (F X A) 
		  (mapcar '(lambda (X A) 
			     (cond ((eq (F (car X) (cadr X)) A))
				   (t (a res X) nil))) X A)))
	 )
	(a AC (COMB C1 C2) AN (COMB N1 N2))
	(mapc '(lambda (F RC RN)
		 (if (apply 'nand (append (TLFO F AC RC) (TLFO F AN RN)))
		     (return (fmt "failed at "  F (car res) (cadr res))))
	  ;; (a res 
	  ;;    (append res (list 
	  ;; 	     (mapcar '(lambda (X Y) 
	  ;; 			(cond ((ne ((car F) (car X) (cadr X))
	  ;; 				   ((car F) (car Y) (cadr Y))) 
	  ;; 			       (print (append (list (car F) X Y))))
	  ;; 			      (t ((car F) (car X) (cadr X))))) AC AN))))
	  ) F RC RN)
;	(pp res)
	"succeeded")))

(defun TCOND () 
  (prog ((L '(() (nil) ((1)) ((1 2)) ((t 'ok)) (() (t 'ok)) 
	      (((not (member '((1 2)) L)) "bad boy") (nil 'nok) ('ok)))))
	(if DEBUG (mapc '(lambda (X) (print (cons 'cond  X))) L))
	(cond ((equal (mapcar '(lambda (X) (apply 'cond  X)) L)
		      '(nil nil 1 2 ok ok ok)) "TCOND passed")
	      ("TCOND failed"))))

(defun TPRED ()
 (prog ((A  (cons 'a 'b)) (B 'a) (N 7) (S "S") (W $[1 "sd" (a b c)])
	(O 13))
       (cond ((and
	       (null nil) (not (null B)) (not (null A)) (not (null N)) 
	       (not (null S))
	       (atom nil) (atom t) (atom B) (not (atom A)) (atom N) (atom S)
	       (symbolp nil) (symbolp t) (symbolp B) (not (symbolp A))
	       (not (symbolp N)) (not (symbolp S))
	       (not (consp nil)) (not (consp t))  (not (consp B)) (consp A) 
	       (not (consp N)) (not (consp S))
	       (listp nil) (not (listp t)) (listp A) (not (listp B))
	       (not (listp N)) (not (listp S))
	       (not (nlistp nil)) (not (nlistp A)) (nlistp B) (nlistp N)
	       (zerop 0) (not (zerop 1)) (not (onep 0)) (onep 1)
	       (oddp 1) (not (oddp 2)) (evenp 2) (not (evenp 1))
	       (nump 0) (nump 1) (not (nump nil)) (not (nump B)) (not (nump A))
	       (nump N) (not (nump S)) 
	       (chrp S) (chrp B) (not (chrp A)) (not (chrp N)) (not (chrp W))
	       (refp W) (not (refp B)) (not (refp A)) (not (refp N))
	       (not (refp A))
	       (not (or)) (and) (nand nil nil)
	       (nand t nil) (nand nil t) (not (nand t t)) (nor nil nil)
	       (not (nor nil t)) (not (nor t nil)) (not (nor t t))
	       (eq A A) (ne A B) (oddp O) (not (evenp O)))
	      "predicates ok")
	  (t "predicates failed"))))

(defun TAPLPRED () ; testing apl predicates
  (if (eq 300 (r '+ (r '+ (implod (mapcar '(lambda (X) (o X (i 10) (i 10))) 
				    '(< <= =  <> >= >))))))
      "TAPLPRED ok" "TAPLPRED failed"))

(defun TAPLOPS () ; testing APL logical operators
  (prog ((R (pairlis '(^ x v ~^ ~v)
		    '([0 0 0 1] [0 1 1 0] [0 1 1 1] [1 1 1 0] [1 0 0 0])
		    ())))
	(mapc '(lambda (X) (if (neql (cdr X) (rav (o (car X) [0 1] [0 1])))
			       (return "TAPLOPS failed"))) R)
	"TAPLOPS OK"))

(defun TNCONC ()
  (catch 'Fail
    (FOR I 1 (? 1000)
	 (prog ((L (list nil 
			 (list 'a)  
			 (list (list 'a 'b 'c))
			 (list (list 'a 'b 'c) nil)
			 (list nil (list 'a 'b 'c) nil)
			 (list (list 'a 'b 'c) (list 'd 'e 'f))
			 (list (list 'a 'b 'c) nil (list 'd 'e 'f) 'z)
			 (list (list 'a 'b 'c) nil (list 'd 'e 'f) (list 'z))))
		(R '(nil
		     a
		     (a b c)
		     (a b c)
		     (a b c)
		     (a b c d e f)
		     (a b c d e f . z)
		     (a b c d e f z))))
	       (mapc '(lambda (X Y) 
			(unless (equal (apply 'nconc X) Y)
			  (throw 'Fail "TNCONC fail")))
		     L R)))
    "TCONC OK"))

(defun TCAR ()
  (prog ((F '(car   cdr
	      caar  cadr  cdar  cddr  
	      caaar caadr cadar caddr cdaar cdadr cddar cdddr))
	 (A '((a) b))
	 (B '(((a) c) ;a
	      (b)     ;d
	      d))     ;d
	 (C '((((a) e) (c) g) ;a
	      ((b) f)         ;d
	      (d)             ;d
	      h)))            ;d
	(mapc '(lambda (C L R) 
		 (if (not (eq (car (C L)) R)) 
		     (return (cat "TCAR failed at " C))))
	      F 
	      (list A A B B B B C C C C C C C C)
	      '(a b a b c d a b c d e f g h))
	"TCAR ok"))

(defun TLACS () ; test list accessors
  (prog* ((L  (list  'a 'b 'c 'd))
	  (L* (list* 'a 'b 'c 'd 'e))
	  (LT (implod nil t (cons 'e (subst '(d . e) '(d)
					    (revl (maplist 'arg L))))))
	  (LC (implod (append (maplist 'arg L) (list nil))))
	  (#IO 0)
	  (LA (implod nil t (append L (list nil)))))
	 (FOR I 0 4 
	      (if (or (ne (nth I L)   (aref LA I))
		      (ne (nthcdr I L)(aref LC I))
		      (nequal (last L*) '(d . e)) ; no harm in repeating test
		      (nequal (last L* I) (aref LT I))
		      )
		  (go 'FAIL)))
	 (return "TLACS Rocks")
   FAIL (break) "TLACS Sucks"))

(defun TLC () ; test apl and lisp logical connectives
  (prog (TA TL)
	(defun Perm (S) (COMB S S))
	(defun TA (F) (mapcar '(lambda (X) (apply F X)) (Perm '(0 1))))
	(defun TL (F) (mapcar '(lambda (X) (apply F X)) (Perm '(nil t))))
	(if (apply 'and 
		   (mapcar '(lambda (A L) (equal (mapcar 'onep (TA A)) (TL L)))
			   '(^ v ~^ ~v) '(and or nand nor))) "TLC: OK"
	  "TLC: foobar")
	))

(defun TPLIST () ; property lists
   (unwind-protect
       (prog 
	((A '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
	(freeze)
	(if (and
	     (genc '(lambda (P)
		      (map '(lambda (X)
			      (mapc '(lambda (Y) (put Y P X)) X)) A)
		      (gen '(lambda (X) (eq X (get (car X) P))) A)) A)
	     (genc '(lambda (X) (genc '(lambda (Y) (remprop X Y)) (revl A))) A))
	    "TPLIST ok"
	  "TPLIST failed")))
   (thaw)
)

(defun TSET () "Test set operations"
  (let*  ((L1 '(A B C D E F G))
	  (L2 '(E F G H I J K))
	  (l1 (implod L1))
	  (l2 (implod L2))
	  )
    (if (and (SETEQ (SUNION L1 L2) (sunion L1 L2))
	     (SETEQ (UNION L1 L2)  (sunion L1 L2))
	     (SUBSET (SETINT L1 L2) L1)
	     (subsetp (setintq L1 L2) L2)
	     (null (setintq (SCOMP L1 L2) (SCOMP L2 L1)))
	     (SETEQ (explod (intern (ACOMP l2 l1))) (SCOMP L2 L1))
	     (SETEQUAL (explod (ACOMP l2 l1)) (SCOMP L2 L1))
	     (eql (AINT l1 l2) (AINT l2 l1))
	     (SETEQUAL (explod (cat (ACOMP l1 l2) (AINT l1 l2))) L1)
	     (SETEQUAL (explod (AUNION l1 l2)) (sunion L1 L2))
	     ) 
	"TSET ok" 
      "TSET fails" (break))))

(defun TCAT (X Y) ; not for the weak hearted
  (prog* ((A (p {X Y} (i (+ X Y))))
	  (B (rev A)) ; make rev work a bit too
	  (LMR (+ A B)) ;laminate result to check against 
	  (CTR (r '+ (r '+ LMR))) ; catenate result
	  (RR {2 X Y}) (RRI RR) (#IO 0) TL TR GI TC0 RI
	  (LI (i 3)) ; laminate shape index
	  (CI [0 1])) ; catenate shape index
; GI is an obscure way of shifting 1st el of V along its length
	 (defun GI (V M N) (rev (| M (+ N V)))) 
	 (defun RI (T I) (aset T (aref T (rev I)) I)) ; Reverse indexed items
	 (FOR I 0 2
	      (setq TL (cat (+ I .5) A B)) ; our laminate candidate
	      (if (or (neql RRI (p TL)) 
		      (neql LMR (r '+ (+ I 1) TL)))
		  (return (break "TCAT lam failure"))
		(setq RRI (aref RR (setq LI (GI LI 3 2))))))
	 (setq TR (cat A B)) ; the default case
         (setq RR (+ (p A) (* CI (p B))))
	 (if (or (neql RR (p TR))
		 (neql CTR (r '+ (r '+ TR)))) (return "TCAT cat def failed"))
	 (FOR I 0 1
	      (setq TR (cat (+ I 1) A B)) ; our catenate candidate
	      (setq CI (rot 1 CI))
	      (setq RR (+ (p A) (* CI (p B))))
	      (if (or (neql RR (p TR))
		      (neql CTR (r '+ (r '+ TR)))) 
		  (return "TCAT cat failed")))
	 (setq CTR (cat A (tr (p X 1))))
	 (if (or (neql CTR (cat A 1)) (neql (rot -1 CTR) (cat 1 A))) 
	     (return "TCAT catenate scalar 1 failed"))
	 (setq CTR (cat 1 A  (p Y 1)))
	 (if (or (neql CTR (cat 1 A 1)) (neql (rot 1 -1 CTR) (cat 1 1 A)))
	     (return "TCAT catenate scalar 2 failed"))
	 ; test cat with zero element dimensions
	 (let* ((A [2 3 4]) (B [0 1 1]) V
		(D (+ 1 (i 24))) E N NZV
		(TC0 
		 '(lambda () ; test catenate on zero dim axis
		    (let* ((#IO 1)
			   (Res (p V 0)) 
			   (Axis (ind V 0))
			   (L (r '* NZV)))
		      (FOR I 1 N 
			   (a Comp (p NZV (+ (* (- I 1) L) (i L))))
			   ;; catenate Comp onto Res at Axis
			   (a Res (cat Axis Res Comp))) Res))))
	   (FOR I 0 2
		(a N (aref A I) 
		   V (* (rot (- I) B) A)  ; replace successive dims in A by 0
		   NZV (k (<> 0 V) V)  ; remaining non zero dims
		   E (cat N NZV))      ; tack missing dim to the front for tr
		(if (neql (TC0) (tr (ind E A) (p E D)))
		    (print "TCAT TC0 failed"))))
	 (return "TCAT: its a winnah")))


(defun TIMPL () ; test implod
  (let ((L (mapcar '(lambda (X) (p [2 3] (+ (* X 10) (i 6) )))
		   '(1 2 3 4 5)))
	(R '(((nil) []) ((nil nil nil) []) ((nil t nil) $[])))
	(CCP '(lambda (A X)  ;; repeated con-cat-enation
	     (let ((R []))
	       (cond ((null X) nil)
		     (t (a R (car X) X (cdr X))
			(while X
			  (if (lt A 0) (a A (+ (rank R) A 1)))
			  (a R (cat A R (car X))
			     X (cdr X)
			     ;; adjust axis after first cat
			     A (int (+ .5 A)))))) R))))
    (if (and (apply 'and (mapcar '(lambda (A) (eql (implod A L) (CCP A L)))
				 (explod (RemElem  (i [-2.5 .5  2.5]) '= 0))))
	     (apply 'and (mapcar '(lambda (X) (eq (apply 'implod (car X))
						  (cadr X))) R))) "TIMPL OK"
      "TIMPL Failed")))


(defun TTR ()  ; test transpose 
  (prog ((#IO 0) (R '([2 3 4] [4 3 2] [3 2 4] [2 4 3] [4 4 4])))
	(mapc '(lambda (X)  (if (not (apply 'and (GenTr X)))
				(return "TTR: Failed"))) R)
	"TTR: OK"))

(defun TPROG () 
  (prog* ((LIM 20)
	  (X  (- LIM 1)))
	 ((label PP1 lambda (I)
		 (cond ((eq I X) (return "prog is cool"))
		       ((lt I LIM) (PP1 (+ I 1)))
		       (t "prog is Dead"))) 1)))

(defun TG  ()
   (prog (X L G) 
      (setq X t)
(setq G (implod nil t '(A B C D E F G H I J K L M N O P Q R S T U V X Y Z ())))
      (defun dprint (X) 
	(if (member X LAST) ()
	  (Dprint  (setq LAST (append LAST (list X) )))))
     F (while (ne X nil) 
	 (let ((A (p [10 10] (i 100))) (B "hello dolly") (C [1 2 3]))
	   (prog ()
		 (setq L (aref G (scalar (? (p G))))) (incr GTC)
	;	 (print (list "you typed " L))
				;	(break "zz")
		 (if (memq L '(A B C Q)) (go L)) 
		 (if (null L) (return 'ciao)
		  ; (print (list " lousy label" L))
		   )
		 (go 'D)
		 A (dprint  "you got A") (go 'D)
		 B (dprint  "its B baby") (go 'E)
		 C (dprint "ucC!") (go 'D)
		 D nil)))
     E (dprint "escape") (go 'F)
     (print "should n't get here!")
     Q 'done
     ) )

(defun TGOTO ()
  (prog (X LAST (GTC 0))
	(setq X "Goto Success")
	(FOR I 1 100 (if (ne (TG) 'done)  (return "goto failed")))
	;;(Dbreak 'TGOTO) (Dprint GTC)
	X))

(defun TRET ()
   (let* ((A (prog (A B) 
                (setq A 100) 
                (setq B 200) 
                (mapcar '(lambda (X) 
			   (let ((A 1) (B 3) (C 4)) 
			     (cond ((eq X 'A) (return A)) (t X))))
			   '(1 2 3 A 5))
	 100 ))
	(B (prog (A B) (setq A 3) (setq B 2) (+ 1 (return B)) 200))
	(C (catch 'A (let ((A 2)) (throw 'A (+ 1 A)))))
	)
     (cond ((equal (list A B C) '(1 2 3)) "TRET OK")(t "TRET KO"))))

(defun TCT () 
  (prog ((R '(lambda (X) (if (zerop X) 0 
			  ; (print X) 
			   (if (eq X 6) (throw 'A "TCT OK"))
			   (R (- X 1)))))
	 (X "TCT prog's value for X")
	 (L (explod (i 10))))
	(catch 'A (mapc R L) "TCT Failed")))

(defun TUWDP ()
  (prog* 
   (RL Scylla Charybdis Sisyphus RP
       (TR "Thrown")
       (RR "Returned")
       (PR "Prog End")
       (UR "Uwdp End")
       (CR "Catch End")
       (CCR "Error Out")
       (TestV '(Normal Return Goto Throw Error))
       (RVals (list (list CR  CR  UR  PR)   ; Normal
		    (list nil nil nil RR)   ; Return
		    (list nil nil nil PR)   ; Goto
		    (list TR  TR  nil PR)   ; Throw
		    (list CCR nil nil PR)   ; Error  
		    )) 
       (SVals  (list [1 2 3 4 5 7 8 9 10] ; Normal 
		     [1 3 4] 		  ; Return 
		     [1 3 4 8 9 10] 	  ; Goto	 
		     [1 3 4 7 8 9 10] 	  ; Throw	 
		     [1 3 4 6 7 8 9 10])) ; Error  
       (TUWDPaux
	'(lambda (X)
	   (prog ()
            (a Charybdis
	     (condition-case err
		 (a Scylla
		    (catch 'A
		      (a Sisyphus
			 (unwind-protect 
			     (let ()
			       (push (list 1 "Start Uwdp form") 'RL)
			       (cond ((eq X 'Return) (return  RR))
				     ((eq X 'Goto) (go 'A))
				     ((eq X 'Throw) (throw 'A TR))
				     ((eq X 'Error) (error "Let me out!")))
			       (push (list 2 "End uwp form") 'RL)
			       UR)
			   (push (list 3 "Cleaning up") 'RL)
			   (push (list 4 "Uwdp clean up done") 'RL)))
		      (push (list 5 "Catch normal exit") 'RL)
		      CR))
	       (user_err (push (list 6 (fmt "caught error: " (car err)
					    " : " (cdr err))) 'RL)
			 CCR)))
		 (push (list 7 "back to prog") 'RL)
	 A       (push (list 8 "about to end prog") 'RL)
		 (push (list 9 (list "Uwdp  res" Sisyphus)) 'RL)
		 (push (list 10 (list "Catch res" Scylla)) 'RL)
		 PR)))
       (MTUWDP '(lambda (T RV SV)
		  (a RL nil Charybdis nil Scylla nil Sisyphus nil RP nil)
		  (a RP (TUWDPaux T))
		  (when DEBUG (print T) (mapc 'print (revl RL)))
		  (unless 
		      (and (equal RV (list Charybdis Scylla Sisyphus  RP))
			   (equal SV (implod (mapcar 'car (revl RL)))))
		    (when DEBUG
		      (print (list Charybdis Scylla Sisyphus  RP))
		      (print RV)
		      (print (implod (mapcar 'car (revl RL))))
		      (print SV))
		    (return "TUWDP Failed")))))
       (mapc MTUWDP TestV RVals SVals)
       "TUWDP Succeeded"))

(defun TSTKOVFL (C)
  (let (M) ;; expose recursion depth of mkc in M
    (let ((mkc '(lambda (N) (a M N)
		  (cond ((zerop N) nil)
			(t (cons (mkc (- N 1)) nil))))))
    (if C (let () (mkc 9000) "TSTKOVFL OK, no trap , no overflow")
      (condition-case err
	  (let () (mkc 9000) "TSTKOVFL OK, no overflow")
	(stk_ovfl (Dprint (fmt "Stack overflow caught at " M " aborting test"))
		  "TSTKOVFL Succeeded"))))))

(defun TLET ()
  (prog (A B C D)
	(setq A '(a b c d e))
	(setq B (explod (i 5)))
	(cond ((not (equal (let ((C A) (D (list C B))) D)
			   (list nil B))) (print "let failure"))
	      (t "let ok"))
        (cond ((not (equal (let* ((C A) (D (list C B))) D)
			   (list A B))) (print "let* failure"))
	      (t "let* ok"))))


(defun TREP () ; test repeat
  (prog (APrimes isPrime LPrimes)
	(defun APrimes (R) ; calculate primes <= R apl style
	  (cond ((le R 2) 2)
		(t (prog ((P 2) A S I) 
			 (a S (c (sqrt R)) I (+ 1 (* 2 (i (f (/ (- R 1) 2))))))
			 (repeat 
			  (ge A S) 
			  (a A (tk 1 I) P (cat P A) I (k (<> 0 (| A I)) I)))
			 (return (cat P I))))))

	(defun isPrime (X) 
	  (prog 
	   ((L  (f (sqrt X))) (K 1))
	   (repeat (gt K L) 
		   (if (and (ne X K) (not (onep (v X K)))) (return nil))
		   (a K (+ K 2))) 
	   t))
  
	(defun LPrimes (R) 
	  (cons 2
		((label GG lambda (X) 
			(cond ((gt X R) nil) 
			      ((isPrime X) (cons X (GG (+ X 2))))
			      (t (GG (+ X 2))))) 3)))

	(cond ((equal (APrimes 100) (implod (LPrimes 100))) "repeat ok")
	      ("repeat has failed"))))

(defun TIOTA () (prog ((#IO 1) RG ZZ DP (#CT 1e-11) (C [-3.14 .01 3.14]))
  (defun RG (X) ; range generation
    (let* ((L (p X)) (#IO 0)) 
      (cond ((eql L 2) (+ (aref X 0) (i (- (aref X 1) (aref X 0) -1))))
	    ((eql L 3) (ZZ (s '+ (cat (aref X 0) 
				      (p (/ (- (+ #CT (aref X 2)) (aref X 0)) 
					    (aref X 1))
					 (aref X 1))))))
	    (t X))))
  (defun ZZ (X) (* (> (| X) #CT) X)) ; cross close to zero on zero
  (defun DP (I) 
   (let ((R (RG I)) (S (i I))) 
      (FOR I 1 (p R) (princl (fmt I:4 (= (aref R I) (aref S I)):3
				 (aref R I):20 (aref S I):20
				 (- (aref R I) (aref S I)):26
				 )))))

  (if (and (equal (i 10) (RG [1 10]))
	   (equal (rev (i 10)) (i [10 1]))
	   (equal (i [-10 10]) (RG [-10 10]))
	   (equal (RG C) (i C))
	   (equal [] (i [10 1 9]))
	   (equal [] (i [10 .1 9]))
	   (equal [10] (i [10 10]))
	   (equal [10] (i [10 10.1]))
	   (equal [10] (i [10 9.9999]))
	   (equal [10] (i [10 .2 10.1]))
	   (setq #IO 0)
	   (equal (i 10) (RG [0 9]))
	   (equal (rev (i 10)) (i [9 0]))) "TIOTA ok" (DP C) "TIOTA crashed" )))


(defun LTT (M N) ;; debug for tiset, not a test
  (let* ((#IO 1)(MMF t)
	 (R (p {M M} 0))
	 (RO (p {M M} 0))
	 (P (* M M))	 
	 (I (p  {2 P} {(p P (i M)) (s '+ 1 (p {M M} 1))}))
	 (L (aref (p I) 2))
	 (RI (? P L)) ; debug 
	 (RRI (aref I [1 2] RI))
	 (Dummy '(lambda (X) (if MMF (p (p X) 1) 1)))
	 (Clear '(lambda () (aset R 0 () ())))
	 (Set   '(lambda (X) (aset R (Dummy R) () ()))))
    (FOR J 1 N
	 (Clear)    
	 (if (neql R RO) (return "Clear Failed"))
	 (Set J)
	 (if (ne P (r '+ (r '+ R))) (break "LTT Failed!")))
    "LTT rocks"))

(defun TISETA (M N)
  (prog* ((#IO 1) 
	  (R (p {M M} 0))
;	  (RO (p {M M} 0))
	  (P (* M M))
;	  (Q (explod (i M)))
;	  (I (implod (mapcar 'tr (mapcar 'implod (COMB Q Q)))))
	  (I (p  {2 P} {(p P (i M)) (s '+ 1 (p {M M} 1))}))
	  (L (aref (p I) 2))
;	  (RI (? P L))
;	  (RRI (aref I () RI))
	  (Clear '(lambda () (aset R 0 () ())))
	  (Set   '(lambda (X) (iset R 1 (aref I () (? X L))))))
					  
	 (if (neql L P) (return "TISET code problem"))
;	 (break)
	 (FOR J 1 N
	      (Clear)
;	      (if (neql R RO) (return "Clear Failed"))
	      (Set J)
	      (if (ne J (r '+ (r '+ R))) (break "TISET Failed!")))
	 "TISET rocks"))

(defun TISET () (TISETA 33 1089))

(defun TASET ()
  (prog* (#IO (P [2 3 4]) (A (p P (i [0 23]))) (B (p P 0)) Off)
	(FOR Org 0 1
	     (a #IO Org Off (= Org 0))
	     (aset B -1 $[::]) ;; funky syntax
	     (FOR I 1 2
		  (FOR J 1 3
		       (FOR K 1 4 
			    (aset B (dec P (- {I J K} 1))
				  (- I Off) (- J Off) (- K Off)))))
	     (if (zerop (r '^ (rav (= A B)))) (break "TASET + Failed"))
	     (aset B -1 () () ())
	     (FOR I 2 DOWNTO 1
		  (FOR J 3  DOWNTO 1
		       (FOR K 4 DOWNTO 1
			    (apply 'aset B (dec P (- P {I J K}))
				   (explod (- {I J K}))))))
	     (if (zerop (r '^ (rav (= A B)))) (break "TASET - Failed")))
  "TASET rocks"))


(defun TAREF () 
  (prog (#IO (A (p [2 3 4] (i [0 23]))) (B (p [2 3 4] (i [23 0]))) C D E F)
	(FOR Org 0 1
	     (a #IO Org F (~ Org)C (i 2) D (i 3) E (i 4))
	     (if  (or (neql A (aref B (rev C) (rev D) (rev E)))
		      (neql A (aref B (- (+ F C)) (- (+ F D)) (- (+ F E)))))
		 (break "TAREF shot")))
  "TAREF cool"))

; test non local go functional values 

(defun TNLG () 
  (prog (C F TF I M CRES FVL FVA LIM #PP #FW R S T X Y Z )
	(setq #PP 0)
	(setq #FW 4)
	(defun C () (not (list I A B X Y Z R S)))
	(defun F (Z) 
	  (if (le Z 100)
	      (prog () (C)
		    (if M (list '(break) (go 'AOUL))) (+ A B X Z)) ; show vars
	    (prog (P Q R S)
		  (let ((A 6) (B 7) (X 8) (Y 9))
		   (setq P (/ Z 1000)) ; calc local funval index 1..10
		   (setq Q (- (+ LIM 1) P)) ; calc mirror funval index
		   (setq R ((aref FVA Q) Q)) ; invoke mirror funval
		   )
		  (setq S (F P)) ; local funval vars
		  (C) R
		  )))
	(defun TF (X FUN) (let ((A (+ X X)) (B (* X X))) (fun FUN)))
	(setq FVA (p 10 $[nil])) ;  funval array
	(Dprint "getting ready to set funval array" )
	(FOR I 1 5  (aset FVA (TF I F) I))
	(Dprint "Done first half" )
	(let ((A 11) (B 22) (X 33) (Y 44) (Z 55))
	  (FOR I 6 10 (aset FVA (TF I F) I))
	  (Dprint "done second half");	(setq FVL (explod FVA))
	  (setq LIM 10)
	  (FOR I 1 LIM (Dprint ((aref FVA I) I)))
	  (Dprint "all ok")
	  (let ((A 111) (B 222) (X 333) (Y 444) (Z 555) (Q 666))	  
	    (prog (R S TRES)
		  (setq R 777) (setq S 888)
		  (C)
		  (FOR I 1 LIM 
		       (let ((R1 ((aref FVA I) (* (- (+ LIM 1) I) 1000)))
			     (R2 ((aref FVA I) I)))
			 (C)
			 (if (neql R1 R2)  (go 'FAIL)
			   (setq TRES (append TRES (list R1))))))
		  (setq M t) ; cause non local go out of funval
		  (Dprint "TNLG: going for the jump")
		  ((aref FVA 4) 7000)
		  (print "TNLG should never get here")
	  AOUL   
		  (Dprint "we came here") 
		  (setq M nil)
		  (C)
		  (FOR I 1 LIM 
		       (if (neql ((aref FVA I) I) (nth (- I 1) TRES))
			   (return "TNLG failed after non local go")))
		  (if (equal (implod (list A B X Y Z Q R S)) (* 111 (i 8)))
		      (return "TNLG ok"))
         FAIL     (return "TNLG failed")
		  )))))

(defun TLAB () (prog (Q A)
  (defun Q (X) (cond ((null X) nil) (t (cons (car X) (Q (cdr X)))))) 
  (setq A '(a b c d e f g))
  (cond ((equal (Q A) ((label F lambda (X)
			(cond ((null X) nil)
			      (t (cons (car X) (F (cdr X)))))) 
		 A)) "label works")
	(t "label fucked"))))

(defun TLIST () (let ((A (explod (i 100))))
  (cond ((equal A (explod (implod (rotl 1 (rotl -1 A))))) 
         "explod implod rlr & rll ok")
        (t "explod implod rlr & rll failed"))))


(defun TMEMB () ; membership
 (prog (ps TPSG (S '(a b c d e f g h i j)) UC)
       (defun ps (s) 
  	(if s (let ((a (car s)) (b (ps (cdr s)))) 
	  	(append (mapcar '(lambda (x) (cons a x)) b) b)) 
	  '(nil)))

      (defun UC (X)
       (prog* ((L (mapcar 'list X)) M N 
               (R (append L (list nil))) (S X) (Y X) (Z X))
	(while Y (setq X S M L L nil)
	  (while X (setq Z M)
	    (while Z
	      (setq N (cons (car X) (car Z)))
	      (cond ((memq (car X) (car Z)) nil)
		    ((findc '(lambda (X) (subset N X)) L t) nil)
		    (t (setq L (append L (list N)))))
	      (setq Z (cdr Z)))
	    (setq X (cdr X)))
	  (setq R (append L R) Y (cdr Y))) R))

    	(defun TPSG (X Y) (not (null (genc '(lambda (Z) (member Z Y)) X))))
        (if (TPSG (ps S) (UC S)) "member is ok" "member is dead")))

(defun TSUBST () ; none shall pass
  (let* ((TV '(lambda (L) 
		(mapcan 
		 '(lambda (X)  (if (atom X) (list X) (cons X (TV X)))) L)))
	 (CKS '(lambda (A B C) 
		 (let ((D (subst A B C))) (if (equal D C) nil D))))
	 (SS '(lambda (X Y) (CKS X Y (CKS Y X A))))
	 (A '(a (b c) 1 z [] "hello" $["a" "b" "c"] $[abc def]))
	 (B (TV A)) ; append flattened sublists
         (C '(xa 4 '(hello polly) "gonzo" $["d" "e"] $[hij klm] [4 5 6])))
    (if (genc '(lambda (X) (equal (apply SS X) A)) (UCOMB B C))
	"TSUBST rocks" "TSUBST spazzed out")))

(defun TQUAD ()
  (prog ((OPP #PP)
	 (QL '(#IB #OB #PP #PW #FW #SD #EP #VP #WN #NF #CS #CR #PM #PN
		   #LS #RL #IO #CT #TR #AI #MI #MN #MX #NN #RA #DM #CC
		   #OP #OF #VN #PL #PD #LL #DD #DL #GC #FM #FS #CW #CH
		   #GW #GH #FN #GS #CP #DX #RF #PF #LF #HF #RS #EX #LX
		   #WS #SW #PR #LP #LH #QQ #KB #GB))) ;#HS #PC
	(if DEBUG 
	    (while QL 
               (prin0 (car QL)) (princ " = ") (print (eval (car QL)))
               (setq QL (cdr QL))))
	(let ((#PP))
	  (cond ((ne OPP #PP) "tquad let failure")
		(t 
		 (let* ((#PP))
		   (cond ((ne OPP #PP) (fmt "tquad let* failure " OPP " " #PP))
			 (t (prog (#PP)
				  (cond ((ne OPP #PP) "tquad prog failure")
					(t "TQUAD ok")))))))))))

(defun TIND () (if (and (equal [1 5 3] (ind $[a b (c d e) f]  $[a (b) (c d e)]))
                        (equal (let ((A "PLMNBHGTREAN UWCZFJQVW"))
				 (aref (cat A '?) (ind A "HELLO")))
			       "HELL?") ;chr
			(equal [4 4 1 3] (ind [5 6 7] [2 4 5 7])) ; num
			(equal [6 3 2] (ind "eliminate" ())) ; atm chr
			(equal [11 11 11] (ind (i 10) ()))   ; mix trix
			) "TIND ok"
		 "TIND failed"))

(defun Walsh (N) 
  ((label F lambda (X J)
	(cond ((eq J N) X)
	      (t (F (cat 1 (cat X X) (cat X (- X))) (+ J 1))))) (p [1 1] 1) 1))

(defun TMAT () 
  (cond ((eq (r '+ (r '+ (. '+ '* (Walsh 5) (tr (Walsh 5))))) 256)
         "mat ok")
        (t "Mat sucked")))

(defun TRND (L) 
  (prog (#CT) (setq #CT 1)
	(cond ((eq (+ L 1) (Dprint (/ (r '+ (? (p 1000 L))) 500)))
	       "random ok")
	      (t "random failed"))))

(defun TCMPLX () 
  (prog (cmul cmul1 cmul2 mkvec test test1 (R [3j4 0j1 1j0 1j1 1 -1 1j-1]))
	(defmacro csub (X) ; snark builder
	  `(aref ,(car X) ,(cadr X) 
		 ,@(mapcar 'not (explod (i (- (rank (eval (car X))) 1))))))
	(defun cplx (x) (+ (csub x 1 ) (* 0j1 (csub x 2 ))))

	(defun cmul1 (x y) (cat (r '- (* x y)) (r '+ (* x (rot 1 y)))))

	(defun cmul2 (x y) (cat (r '- (* x y))
				(r '+ (* (aref x [1 2]) (aref y [2 1])))))

	(defun mkvec () ((lambda (X) (UCOMB X X)) 
			 (mapcar 'implod (COMB '(1 0 -1) '(1 0 -1)))))

	(defun test (F1 F2 X) 
	  (equal (mapcar '(lambda (X) (F1 (cplx (car X)) (cplx (cadr X)))) X)
		 (mapcar '(lambda (X) (cplx (F2 (car X) (cadr X)))) X)))

	(defun test1 (X) 
	  (let* ((#IO 1)
		 (A (implod 2.5 (mapcar '(lambda (X) (implod  1.5 X)) X)))
		 (A1 (k 3 (r '~^ 1 (= 0 (aref A () 1 ()))) A))) ; chop out zeros
	    (onep (r '^ (r '^ (= (/ (* (cplx (aref A1 () 1 ()))
				       (cplx (aref A1 () 2 ())))
				    (cplx (aref A1 () 1 ())))
				 (cplx (aref A1 () 2 ()))))))))
	(if (and (test '* cmul2 (mkvec)) 
		 (test '* cmul1 (mkvec))
		 (test1 (mkvec))
		 (eql (% -8 R) (- (% 8 R)))
		 (eql R (. '+ '% [-10 -11] (o '% [ 9 11] R)))
		 ;; can't use eql below because of IEEE754 sin inaccuracy
		 (gt #CT (| (- R (. '* '% [ -9 -12] (o '% [10 12] R))))))
	    "TCMPLX ok" "TCMPLX failed"))
  )

(defun TKOMP (N)
  (prog ((G (i [5 2 13]))
	 (K [1 1 0 1 0])
	 (X (/ (i (* N 10)) 10))
	 (Y (explod (i N)))
	 (Z (i N)))
	(cond ((and (eql (p 4 4) (k 4 4)) ;; two scalars
		    (eql (k K 5) (p 3 5)) ;; rarg scalar extension
		    (eql (k 2 G) (rav (tr (cat .5 G G)))) ;; larg scalar extn
		    (eql Z (k (= X (f (+ X 0.5))) X))
		    (eql Z (k (= X (c (- X 0.5))) X))
		    (eql Z (r '+ (o '= Z (k Z Z))))
		    (eql (k [-1 2 -3] 5) [0 5 5 0 0 0])
		    (eql (k [1 3 1  0 -4  2 -3]
			    [5 7 9 11    13   ])
			 [5 7 7 7 9 0 0 0 0 13 13 0 0 0])
		    )
	       "TKOMP f/c ok")
	      (t "TKOMP f/c broken"))))

(defun TEXPND ()
  (if (and (eql (ex [1 0  1 0 0 1 0 0 0 ] [1 2 3]) [1 0 2 0 0 3 0 0 0])
	   (eql (ex [1 0 1 0 0 1 0 0 0 ] "ace")  "a c  e   ")
	   (eql (ex [1 0 1 0 1] 'ace)  "a c e")
	   (eql (ex 1 [1 0 1] (p [2 3] (i 6)))  (p [3 3] [1 2 3 0 0 0 4 5 6]))
	   (eql (ex 2 [1 0 1 1] (p [2 3] (i 6))) (p [2 4][1 0 2 3 4 0 5 6]))
	   (eql (ex [0 0 0 0 0 0] []) [0 0 0 0 0 0])
	   (eql (ex [0 0 0 0 0 0] "") "      ")
	   (eql (ex [0 1 0 1 0] $[begin end]) $[nil begin nil end nil]))
      "TEXPND ok"
    "TEXPND failed"))


(defun TIDELEM () ; test id elements
  (prog ((F '(dist + - * / c f ^ x v ~^ ~v | exp l % ! > >= < <= = <>))
	 (L '(0    0 0 1 1 2 3 1 0 0  0  1 0 1   4 4 1 0  1 0 1  1 0))
	 (P {#AI #MI #MN #MX #NN})
	 (#IO 0)) 
	(mapc '(lambda (F X)
		 (if (ne (r F []) (aref P X))
		     (return (fmt "TIDELEM failed on " F X:2)))) F L)
	"TIDELEM ok"))

(defun TSCAN ()
  (prog* ((#IO 0) (A (enc (p 5 2) (i 32))))
    (FOR I 0 31 (if (neql (enc (p 5 2) (exp 2 (Int (l 2 I))))
			  (s '< 1 -1 (aref A () I)))
		    (return (break "TSCAN vector fail"))))
    (if (neql (s '< 2 -1 (tr A)) (tr (s '< 1 -1 A)))(return "TSCAN array fail"))
    "TSCAN ok"))

(defun tfa () 
  (prog (A Y F G)
    (setq Y "yes baby")
    (defun F (F Y) (F Y))
    (defun G (X) (Dprint (list X Y)))
    (cond ((and (equal (F 'G "hi") (list "hi" "hi"))
                (equal (F (fun G) "hi") (list "hi" Y))) "tfa ok")
          ( "tfa failed"))))

(defun TFA ()  ; test functional argument
  (let 
   ((FUN) 
    (A "hello A") (B "bye B") (C "see C")
    (R1 (list  "quote:" 1  2 "first X" "first X" '("first X" . "first X")))
    (R2 (list  "fun:" "hello A" "bye B" "see C" X '("other X" . "other X"))))
   (defun FUN (F X) 
     (let ((A 1) (B 2) (C X)) 
          ; (setq GLOBAL_F F) this is extremely illegal if not supporting funval
	   (F (cons X X))))
   (cond 
    ((and 
      (equal R1 (FUN '(lambda (Z) (Dprint (list "quote:" A B C X Z))) 
		    "first X"))
      (equal R2 (FUN (fun (lambda (Z) (Dprint (list "fun:" A B C X Z)))) 
		    "other X"))) "funarg ok")
    (t "funarg kaput"))))

(defun TCWCC (Turns Iterations) ; Squeezed from TPROC
  (prog  (P1 PT ST
   (Disp '(lambda (P) (cwcc '(lambda (V) (P V))))) ; returns (Kont X)
   (Run '(lambda (N)
     (prog ((L (append PT)) T (I 0))
      (while (setq T L) ; running processes
	(mapc '(lambda (X)
		 (cond ((Disp (cadr X)) (se '(procio) (cadr X)))
		       (t  (setq L (delete X L)))) ; remove
		 (cond ((gt (incr I) N) (return (list "done after" I)))))T))
      (cond ((SETEQ ST (mapcar 'car PT)) "TCWCC rocks")
	    (t "TWCC sucks")))))
   (se '(lambda (X Y) (eval X (caddr Y))))
   (sf '(lambda (F A E) (eval (list 'apply F (list 'quote A)) (caddr E))))
   (findProc '(lambda (Name) (cadr (assoc Name PT))))
   (addProc  '(lambda (Name Ent) (setq PT (append PT (list (list Name Ent))))))
   (mkp '(lambda (Name Code Env) ; Make process
     (prog ((Kont) SUSP Self Res M D (IQ (mkdll)) (OQ (mkdll)) 
	    (addEnv  '(lambda (X) (setq Env (cons X Env))))
	    (getEnv  '(lambda (X) (cadr (assoc X Env))))
	    (setKont '(lambda (X) (setq Kont X))); X comes from calling env
	    (msend   '(lambda (D M) ; dest message
			(puthead OQ (cons Name (cons D M))) ; (from to m)
			(cwcc '(lambda (V) (setq SUSP V) ; capture prog cont
			    (Kont (list 'msend Name))))))
	    (mrecv   '(lambda ()(let (M) (cwcc '(lambda (V) (setq SUSP V)))
				     (cond ((peektail IQ) (setq M (gettail IQ))
					    (cons (car M) (cddr M)))
					   (t (Kont (list 'mrecv Name)))))))
	    (putq    '(lambda (X) (puthead IQ X)))
	    (procio  
	     '(lambda ()
		(while (setq M (gettail OQ))
		  (cond ((setq D (findProc (cadr M))) (sf 'putq (list M) D))
			(t (print (list "dest for " M " not found")))))))
	    (rme '(lambda (X) (setKont X) (cond (SUSP (SUSP Res)))   ;  resume
		    (setq Res (Code Name)) (setq ST (cons Name ST))
		    (cwcc '(lambda (V) (setq SUSP V) (Kont Res))) (Kont nil)))
	    ) ; mkp main 
	   (cond ((assoc Name PT) (list Name " exists") (return nil)))
	   (setq Self (fun rme)) (addProc Name Self) Self)))

   (Ping '(lambda (Name)
     (prog ((N (getEnv 'Turns)) (Dest (getEnv 'Dest)))
	   (FOR I 1 N (msend Dest (list 'MSG Name I)) (setq Mes (mrecv)))
	   (msend Dest (list 'QUIT Name)) 'ok )))

   (Pong '(lambda (Name)
     (prog ((Mes) Dest Type Body)
     (while (setq Mes (mrecv))
       (setq Dest (car Mes) Type (cadr Mes) Body (cddr Mes))
       (cond ((eq Type 'MSG) (msend Dest (list 'ACK Name (cadr Body))))
	     ((eq Type 'QUIT)  (return 'ok))
	     (t (print (list "crazy message" Mes)))))'ok )))
   ); TCWCC Main
  (setq P1 (mkp 'Ping Ping nil)) (mkp 'Pong Pong nil)
  (sf 'addEnv (list (list 'Turns Turns)) P1)
  (sf 'addEnv (list (list 'Dest 'Pong)) P1)
  (Run Iterations)))

(defun TSUB ()
  (prog* ((#IO 0)
	  (N (explod (i 13)))
	  (NN (explod (- 12 (i 13))))
	  (R (explod (chr (+ (num 'a) (i 13)))))
	  (RR (revl R))
	  (L (maplist  '(lambda (X) X) R))
	  (LL (maplist  '(lambda (X) X) RR)))
;	 (print N) (print NN) (print R) (print RR) (print L) (print LL)
	 (if (and (subset N NN) 
		  (subset NN N) 
		  (subset R RR) 
		  (subset RR R) 
		  (subset L (revl L))
		  (subset LL (revl LL))
		  (not (subset R
			       (subst 'wrong 
				      (aref $[a b c d e f g] (? 6))
				      R))))
	     "subset ok" "subset dead")))

(defun TGENC () 
  (let (
	(A '(a b (c d) e f g h i j k l m n o p))
	(B)
	(SUBSET '(lambda (X Y) (genc (fun (lambda (Z) (memq Z Y))) X)))
	(M '(lambda (X) 
	      (let ((A)) (genc '(lambda (X) (setq A (cons X A))) X)))))
    (setq B (M A))
    (cond ((and (SUBSET A B) (SUBSET B A) (equal A (revl B))) "genc ok")
	     (t "genc kaputt"))))

(defun TFV () 
  (prog (F TF FUN) ; test functional values
   (defun F (Z) (list A B X Z))
   (defun TF (X) (prog (A B) (setq A 1) (setq B 2) (fun F))) 
   (setq FUN (TF 'hello))
   (cond ((equal (FUN 'bye) (list 1 2 'hello 'bye)) "funval ok")
         (t "funval kaput"))))

(defun TOOP ()
(prog (#PP myobj osend new mapsend toto X A B G)
  (setq #PP 0)
 
  (defun myobj (X Y)
     (prog (I S T )
           (defun I (x y) (setq T (+ (setq X x) (setq Y y))))
           (defun S () (Dprint (list X Y 'sum T)))
           (setq T (+ X Y))
           (fun TOOP))) ; return closure with X Y I S T bound
 
  (defun osend (X Y) (eval X (caddr Y))) 
 
  (defmacro new (X) (list 'not (list 'setq (car X) (cdr X))))
   
  (defun mapsend (F X) (mapc '(lambda (X) (osend F X)) X))

  (defun toto (Z) (Dprint (list 'X X 'Z Z)))

  (setq X 'global_value)
  (toto 'hello)
  (new A myobj 1 2)
  (osend '(toto 'bye) A)
  (osend '(I 10 20) A)
  (osend '(S) A)
  (new B myobj 3 4)
  (mapsend '(S) (setq G (list A B (myobj 5 6))))
  (cond ((equal '(30 7 11) (mapcar '(lambda (X) (osend 'T X)) G)) "oop ok")
	(t "oop KO"))
))

(defun fme (lambda) lambda)

 (defun TMCAR () (let* ((A '(a b c d e f g h i j k l m n o p q r s t u v w))
		       (B (explod (i (len A)))))
   (cond ((and 
	   (equal A (mapcar 'cadr (mapcar '(lambda (X) (list 'hello X)) A)))
	 ;  (equal (mapcar '(lambda (X Y) (nrev (list X Y))) A B)
		;  (pairls  B A nil))
	   )
          "mapcar OK")
         (t "mapcar KO"))))

(defun TMCF ()
  (prog (B C D)
	(let ((A '(a b c d e f g h i j k l m n o p q r s t u v w)))
	  (a B ((label P lambda (X)
		       (cond ((null X) nil) 
			     (t (cons (list 'Hello (car X))
				      (P (cdr X)))))) A))
	  (a C (mapcar ((lambda (Y) (fun (lambda (X)  (list Y X)))) 'Hello) 
		       A))
	  (a D (mapcar '(lambda (X) (fun (lambda (Y) (list Y X)))) A)))
	(if (and (equal B C) (equal C (mapcar '(lambda (X) (X 'Hello)) D)))
	    "TMCF mapcar with fun rocks" "TMCF fail")))

(defun TMCCWCC ()  ; Test mapcar with cwcc
  (let ((A '(a b c d e f g h i j k l m n o p q r s t u v w))  
	(F '(lambda (X) X))  K
	(CC '(lambda () 
	       (let ((W (mapcar '(lambda (X) 
				   (cond ((eq X 'k) 
					  (cwcc '(lambda (Y) (a K Y)
						   (list 'Captured X))))
					 ((list 'Hello X))))
				A)))
		 (F W)))))
    (cond ((and 
	    (findc 'car (CC) 'Captured)
	    (equal (subst 'k 'Polly (mapcar 'cadr 
					    (cwcc '(lambda (Z) 
						     (setq F Z)
						     (K '(Hello Polly))))))
		   A))
	  "TMCCWCC Rocks")
	  ("TMCCWCC Failed"))))


 (defun TMCAN () (let ((A '(a b c d e f g h i j k l m n o p q r s t u v w)))
   (cond ((equal A (mapcan '(lambda (X) (cond ((eq X 'hello) nil) 
                                              (t (list X)))) 
                            (mapcan '(lambda (X) (list 'hello X)) A)))
	"mapcan OK")
	(t "mapcan KO"))))

 (defun TMN () (let ((A '(a b c d e f g h i j k l m n o p q r s t u v w)))
   (cond ((equal ((label R lambda (X) 
			 (cond ((null X) nil) (t (append X (R (cdr X)))))) A)
	 (mapn 'append A)) "mapn OK")
	 (t "mapn KO"))))

(defun TMLIST () (let ((A '(a b c d e f g h i j k l m n o p q r s t u v w)))
   (cond ((equal ((label R lambda (X) 
			 (cond ((null X) nil) (t (cons X (R (cdr X)))))) A)
		 (maplist 'append A)) "maplist OK")
	 (t "maplist KO"))))

(defun TLF (L) (prog (A) (setq A (dll-head L)) (princ "Vorwärts:   ")
  (while (ne A L) (prin0 (dll-val A)) (setq A (dll-next A))))(terpri))
(defun TLB (L) (prog (A) (setq A (dll-tail L)) (princ "Rueckwaerts: ")
  (while (ne A L) (prin0 (dll-val A)) (setq A (dll-prev A))))(terpri))
(defun mapdar (F L) 
  (cond ((dll-null L) nil)
	(t ((label mapdaux lambda (X) 
		   (cond ((eq X L) nil)
			 (t (cons (F (dll-val X))
				  (mapdaux (dll-next X))))))
	   (dll-head L)))))


(defun TDLL ()
  (prog (R L)
	(dll-p (setq L (mkdll)))
	(mapc '(lambda (X) (puttail L X))  (explod (chr (+ 32 (i 94)))))
	(while (not (dll-null L))
	  (cond ((lt (setq R (c (? 6))) 3) (gethead L))
		((eq R 3) (puthead L 'head))
		((eq R 4) (puttail L 'tail))
		(t (gettail L)))
	  (cond ((or (not (equal (dll-hlist L) (revl (dll-tlist L))))
                     (ne (dll-len L) (len (dll-tlist L))))
		 (print (list "DLL argggh"  (dll-len L) (len (dll-tlist L)))))
	        (t "tdll ok")))))

; The sorty ones
(defun TMON (X C) " test X  for monotonicity ordered by comparison op C"
  (cond ((listp X)
  ; list C for strictly increasing use 'lt; for increasing use  'le etc
	 (gen '(lambda (X) (cond ((null (cdr X)) t) ((C (car X) (cadr X))))) X))
  ; atom C for strictly increasing use  '<; for increasing use  '<= etc
	(t (onep (r '^ (C (dp -1 X) (dp 1 X)))))))

(defun TGUP ()  (if (TGTST 'gup) "TGUP ok" "TGUP failed"))
(defun TGDN ()  (if (TGTST 'gdn) "TGDN ok" "TGDN failed"))
(defun TGTST (F)
  (prog ((TGaux '(lambda (A) ; test grade 
		   (TMON (aref A (F A)) (if (eq F 'gup) '< '>))))
	 
	 (TGrade '(lambda () ;check for order of equal elements preserved
		    (prog* ((A(+(k(p 4 5)(i 4))(/(cat(i [0 9])(i [9 0]))10)))
			    (B (rav (aref (p [4 5] A) [4 1 3 2] ()))))
			   (if (eq F 'gup)
			       (eql (aref B (gup (f B))) A)
			     (a B (rev B))
			     (eql (aref B (gdn (f B))) (rev A)))))))
	
	(and (TGaux (i 5))       (TGaux (i 6))
	     (TGaux (? 99 99))   (TGaux (? 100 100))
	     (TGrade))))
  
(defun TSORT () (prog ((O (COPYLS (oblis))))
  (let (T 
	(S (sort '(lambda (X Y) (ge (len X) (len Y))) O))
	(check '(lambda () 
		  (and (TMON (mapcar 'len S) 'ge)
		       (equal S (a T (sort '(lambda (X Y)
					      (ge (len X) (len Y)))
					   O)))))))
    (cond ((check) "tsort ok") (t (break "tsort screwed"))))))

(defun TFIXED_POINT () (prog (F)
    (defun F (X) 
       (let ((Y (cons 'quote (cons X nil)))) (cons Y (cons Y nil))))
     (cond ((equal (F F) (eval (F F))) "Fixed point ok")
	    ("Fixed point failed"))))

(defun TFOUR () (prog (I)
  (let ((Q) (T 1e-14)
        (SEQ '(lambda (N P) 
		(setq N (exp 2 N)) (% 1 (/ (% (- (/ N 2) (i N))) P)))))
       (FOR I 2 15 
	    (setq Q (SEQ I 32))
	    (cond ((neql (p Q) (r '+ (< (| (- Q (% 9 (y (w Q))))) T)))
		   (DEBUG (break))
		   (return "fourier ***FAILED***")))
	    )) "fourier OK"))

(defun TENC () (prog (Base decode)
  (setq Base (p 8 2)) 
  (defun decode (Base Vec)
   (setq Base (cat (aref Base (i (- (p Base)  1))) 1)) 
   (r '+  (* Vec (rev (s '*  (rev Base))))))
  (FOR I 0 255 (cond ((neql (decode Base (enc Base I)) I)
		      (return "enc failed"))))
  "enc succeeded"))

(defun TDEC () ; test decode 
; examples from APL\3000 ref manual PN: 32105-90002  pp 3-147 3-153
  (prog (tdec C T TR EQ A B ABR D ADR)
	(defun tdec (X Y) (equal X (dec (p 10 Y) (enc (p 10 Y) X))))
	(defun EQ (X Y) (and (equal (p X) (p Y)) (equal X Y)))
	(setq A   (p [3 2 2] [10 10 3 3 2 2])
	      B   (p [2 2] (i 4))
              ABR (p [3 2 2] [13 24 6 10 5  8 13 24 6 10 5 8]))
	(setq D   (p [2 2 2] (i 4))
	      ADR (p [3 2 2 2] [11 22 33 44 4  8 12 16  3  6 9 12]))
	(setq C   (p [3 2] [-1 2 7 8.9 -4 12]))
	(setq T   (p [ 3 2 2] (i 6)))
	(setq TR  (p [2 2] [43 64 55 76]))
	(if (and
	     (eq 5 (dec 5 5))
	     (eq 2728.5 (dec 5 [5 -3.2 0 .7 0]))
	     (eq 2728.5 (dec [5 5 5 5 5] [5 -3.2 0 .7 0]))
	     (eq 2240429 (dec [2000 16 28] [5000 15 9]))
	     (EQ (dec [1 3 5] T) TR)
	     (EQ ABR (dec A B))
	     (EQ ADR (dec A D))
	     (prog (Base)
		   (FOR I 2 16
			(setq Base (p 8 I))
			(FOR J 2 255 ; 256 should fail
;     (princ (fmt "(dec " I:2 J:4 ") => " (enc Base J) (chr 10)))
			     (if (not (equal J (dec Base (enc Base J))))
				 (return nil))))
		   t)
	     ) "TDEC OK" "TDEC Failed")))

(defun TENCDEC () ; test encoding and decoding 16 bit 2's complement chr vector
  (let ((V (i [-32768 32767])) (P16 (exp 2 16))
	(encBuf '(lambda (V)
		   (rav (tr (chr (enc [256 256] (+ (* (< V 0) P16) V)))))))
	(decBuf '(lambda (V)
		   (let ((V1 (dec 256 (num (tr (p {(/ (p V) 2) 2} V))))))
		     (- V1 (* P16 (> V1 32767))))))
	(decBuf2 '(lambda (V)
		   (let ((V1 (. '+ '*  (num (p {(/ (p V) 2) 2} V)) [256 1])))
		     (- V1 (* P16 (> V1 32767)))))))
    (if (onep (r '^ (= V (decBuf (encBuf V))))) "TENCDEC OK"
      "TENCDEC Failed")))


(defun TDQUOT () (let ((A "\A\B\C\D\E\F\G\H\I\n\K\L\M\N\O\P\Q\R\S\T\U\
\V\W\X\Y\Z\[\q\]\^\_ !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNO\
PQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~") (B "
"))
; " this comment to help emacs lisp colouring
(if (and (equal A (chr (i 126))) (equal B (chr 10))) "TDQUOT OK"
     "TDQUOT Failed!" (break))))

(defun TIO () 
  (prog ((#PL 1000000) (#PP -1) (#PW 256) (#SD -1) (#CT 0)
	 (I (/ (? (p 10000 1e6)) 1e11)) (S (chr (i 127))) K 
	 (F (open "junk" "rw")) Z)
	(print I F)
	(print S F)
	(rewind  F)             
	(setq K (read F))
	(setq T (read F))
	(close F)       
	(if (and (eql I K) (equal S T))"TIO success"
	  (princl (fmt (r '+ (<> I K)) " mismatches"))
	  (princl (fmt "First mismatch at "
		       (a Z (aref (k (<> I K) (i 10000)) 1))))
	  (princl (fmt "Calc: " (aref I Z) " Read: " (aref K Z)))
	  "TIO failed")
	))


(defun TDEAL (X) 
   (prog ((#IO 0) (Y (+ X 1)))
      (FOR I 1 1000 
           (if (onep (r '+ (= (? X Y) X))) (return "TDEAL failed"))))
	"TDEAL OK")

(defun Qprint (X) 
  (print X)
  (print (list "debug values #FW #PP" #FW #PP)))

          
(defun QT (X) (FOR I 1 X (prog (A)
        (FOR I 1 10 (setq A (p (cat I I) (i (* I I))))
         (Dprint (list I (r '+ (r '+ (= A (tr (tr A)))))))))))

(defun QT1 ()
  (let* ((A (FLAT (oblis))) (B (revl A)))
    (while (SETEQ A B)
      (setq A (cdr A))                    ; remove first el from A
      (rplacd (POS (list (car A)) B) nil) ; remove last el from B
      )))

(defun TPP () 
  (let ((Q '(TPP TCOMP TPRED TAPLPRED TAPLOPS TCAT TIMPL TGUP TGDN
		 TPROG TLET TIOTA TISET TASET TAREF 
		 TLAB TLIST TMEMB TQUAD TMCAR TMCF TMCCWCC TMCAN TMN
		 tfa TFA TCWCC TFV TOOP TGENC TMCN TMAT TRND
		 TSORT TFIXED_POINT TFOUR TENC TDEC TDLL QT fme))
	(RUN t)
	F C)
       (defun F (X) (princ X) (princ " ?") (setq C (getc))
	      (terpri)
	      (cond ((eq C 'y) (pp (eval X)))
		    ((setq RUN (ne C 'q)))))
       (while (and RUN Q) (F (car Q)) (setq Q (cdr Q)))))


(defun TRR ()
  (let (TREC ETREC LastLeaf Perms UPERM F G H Flat Flataux Delete 
	     GENTUPLES R Nextl Conv Ack Ack1
	     (L '(A (B (C D (E))))) (Q (i 10)) (NPERMS 5))
    (defun TREC (X K) 
      (cond ((zerop X) K) (t (TREC (- X 1) (+ K X))))) ; plain tail recursion
    (defun ETREC (N) 
      (cond ((onep N) 1) (t (+ N (ETREC (- N 1)))))) ; encapsulated tr

    (defun LastLeaf (Btree) (cond ((cdr Btree) (LastLeaf (cdr Btree)))
				  ((atom (car Btree)) (car Btree))
				  (t (LastLeaf (car Btree)))))

    (defun Perms (N R) (F 0 nil))
    (defun F (L E) 
      (if (lt L N) (H (+ L 1) (cons 1 E))
	(a R (nconc R (list (revl E))))
	(G L (car E) (cdr E))))
    (defun G (L X E) 
      (if (eq X N) 
	  (if E (G (- L 1) (car E) (cdr E)) R) ; plain
	(H L (cons (+ X 1) E))))
    (defun H (L E) 
      (if (memq (car E) (cdr E)) (G L (car E) (cdr E)) ; mutual
	(F L E))) ; mutual
    
    (defun R (A X) (cond ((null X) nil)
			 ((cons (append X A)
				(R (append A (list (car X))) (cdr X))))))

    (defun GENTUPLES (X) 
      (cond ((null (cdr X))  (list X))
	    (t (mapcan '(lambda (Y) (R () (cons (car X) Y))) 
		       (GENTUPLES (cdr X)))))) 

    (defun Flat (L) (Flaux L nil))  
    (defun Flaux (L R) 
      (cond ((null L) R) 
	    ((atom L) (cons L R)) 
	    (t (Flaux (car L) (Flaux (cdr L) R))))) ; plain + encapsulated

    (defun Delete (X L) 
      (cond ((atom L) L)
	    ((equal X (car L)) (Delete X (cdr L)))  ; plain tail recursion
	    (t (cons (Delete X (car L))
		     (Delete X (cdr L)))))) ; encapsulated tr
    
    (defexpr Nextl (X) (let ((Nextl_parm (car X)) (Nextl_Val (eval (car X))))
			 (set Nextl_parm (cdr Nextl_Val))
			 (car Nextl_Val)))
    (defun Conv (A B) ; no tr found...
      ((label CConv lambda (A) 
	     (if (null A) 0 ((lambda (X) (+ X (* (car A) (Nextl B))))
			     (CConv (cdr A))))) A))
    
    (defun Ack (X Y) 
      (cond ((zerop X) (+ Y 1))
	    ((zerop Y) (Ack (- X 1) 1))
	    ((Ack (- X 1) (Ack X (- Y 1))))))

    (defun Ack1 (X Y) (if (zerop X) (+ Y 1)
			(Ack1 (- X 1) 
			      (if (zerop Y) 1 (Ack1 X (- Y 1))))))

    (if (and
	 (eq (TREC 100 0) 5050)
	 (eq (ETREC 100) 5050)
	 (eq (LastLeaf L) 'E)
	 (eq (r '+ (* Q (rev Q))) (Conv (explod Q) (explod Q)))
	 (eq (Ack 3 4) (Ack1 3 4))
	 (equal (Flat L) '(A B C D E))
	 (equal (Delete '(C D (E)) L) '(A (B)))
	 (equal
	  (Perms NPERMS nil) ; This is the bugger that has different counts
	  (sort	'(lambda (X Y) (lt (implod X) (implod Y)))
		(GENTUPLES (explod (i NPERMS))))
	  )
	 )
	"TRR ok" "TRR failed")))


(defun TIDIOMS ()
  (let
      (N S ; these are free in T. One day we will find them automagically
	 (T
	  '(
;;;(shell-command "grep ';=>' idioms.al | sed -e 's/^;; /(/' -e 's/;=>//' -e 's/$/)/'" t nil)((RemRep "rrrambblerr")   "rambler")
((RemDupIII [10 5 2 9 8 3 7 8 8 4])  [2 3 4 5 7 8 9 10])
((TRemDup)  " abcdefghijklmnopqrstuvwxyz")
((p (Expand (p [4 6] "Hello Polly Fred  Joe   ") "Sebastien"))  [5 9])
((TO 2 5)   [2 3 4 5])
((TO 10 5)  [10 9 8 7 6 5])
((ElimLB "   This is the end !   ")  "This is the end !   ")
((ElimTB "   This is the end !   ")  "   This is the end !")
((RedBlank "   This    is    the       end  !     ")  " This is the end ! ")
((NDigs 12345)  5)
((eql (MkVec 12345) (i 5))  t)
((MkStr 123450)  "123450")
((StoV "4321")   [4 3 2 1])
((EncV [4 3 2 1 1 2 3 4])  43211234)
((eql (Arep [1 2 19 123]) (p [4 3] "001002019123"))  t)
((blzs "00010000")  "   10000")
((eql (ALTOAM (AMTOAL (LTM 5))) (LTM 5))  t)
((setq N 10)  10)
(     (r '+ 1 (= 0 (o '| (i N) (i N))))   [1 2 2 3 2 4 2 4 3 4])
((= 2 (r '+ 1 (= 0 (o '| (i N) (i N)))))  [0 1 1 0 1 0 1 0 0 0])
((i N)                                    [1 2 3 4 5 6 7 8 9 10])
((k (= 2 (r '+ 1 (= 0 (o '| (i N) (i N))))) (i N))  [2 3 5 7])
((IsInt [-1.1 -1 -.9 0 .5 1.3 4 4.5])  [0 1 0 1 0 0 1 0])
((intp [-1.1 -1 -.9 0 .5 1.3 4 4.5])     [0 1 0 1 0 0 1 0])
((IsNum 1)          [1])
((IsNum "ASD")      [0])
((IsNum $[a b c])   [0])
((eq []  (p 0 1))         t)
((eq ""  (p 0 "ASD"))     t)
((eq $[] (p 0 $[a b c]))  t)
((IsOf "aeiou"  "facetious")  1)
((IsSymm (o '+ (i 10) (i 10)))  1)
((IsSymm (o '- (i 10) (i 10)))  0)
((Unique (? 10 10))       1)
((Unique {1 (? 10 10)})   0)
((NonDecreasing  [-1.1 -1 -.9 0 .5 1.3 4 4.5])  1)
((VecEq (i 10) (i 10))  1)
((HasSome "aeiou"  "bcdfg")   0)
((HasSome "aeiou"  "bcdfgo")  1)
((HasDups (p [6 5] "Hello "))  0)
((HasDups (p [7 5] "Hello "))  1)
((NoDupSpace "sadas  ads asd")  0)
((NoDupSpace "sadas ads asd")   1)
((IsPerm (i 10) (? 10 10))   1)
((IsPerm (i 5) [5 2 3 4 3])  0)
((TAll1v0)  9)
((EQUALS (i 10) (rev (i [10 1])))  1)
((EQUALS "Hello" {"Hell" "o"})     1)
((EQUALS (i 10) "Hello")           0)
((EQUALS (i 10) (i 11))            0)
((EQUALS (p [2 3] 0) (p (i 3) 0))  0)
((s '+ (i 10))   [1 3 6 10 15 21 28 36 45 55])
((s 'c [3 1 2 4 8 5 6 7 10 9])   [3 3 3 4 8 8 8 8 10 10])
((s 'f [9 10 7 6 5 8 4 2 1 3])   [9 9 7 6 5 5 4 2 1 1])
((| (s '- 1 -1 (* 2 (i 10))))   [2 2 4 4 6 6 8 8 10 10])
((s 'v [0 0 1 0 0 1 1 0])    [0 0 1 1 1 1 1 1])
((s '< 1 -1 [0 0 1 1 0 1 1 0])  [0 0 1 0 0 0 0 0])
((s '<= 1 -1 [1 0 1 1 0 1 1 0])   [1 0 1 1 1 1 1 1])
((s '<> [1 0 1 0 1 0 1 0 1 1 0 0])  [1 1 0 0 1 1 0 0 1 0 0 0])
((s '^ [1 1 1 1 0 1 1 0])   [1 1 1 1 0 0 0 0])
((enc (p 9 2) 224)                   [0 1 1 1 0 0 0 0 0])
((rev (enc (p 9 2) 224))             [0 0 0 0 0 1 1 1 0])
((s 'v (rev (enc (p 9 2) 224)))      [0 0 0 0 0 1 1 1 1] )
((~ (s 'v (rev (enc (p 9 2) 224))))  [1 1 1 1 1 0 0 0 0] )
((r '+ (~ (s 'v (rev (enc (p 9 2) 224)))))  5            )
((NP2L 224)  5)
((NP2A 224)  5)
((genc '(lambda (X) (eq (NP2A X) (NP2L X))) (explod (i 256)))  t)
((PatMat "the" "the quick brown fox jumped over the lazy dog")  [1 33])
((RepByBlank "Next_Page" "_")  "Next Page")
((let ((L (p [3 5] "HelloPollySue  "))) (eql (ASort (rot 1 1 L)) L))  t)
((let ((L (p [3 5] "PollySue  Hi   "))) (eql (LSort L) (rev 1 L)))  t)
((let ((L (p [3 5] "HelloPollySue  "))) (eql (RemDupW (p [9 5] L)) L))  t)
((let ((L (p [3 5] "Hi   Sue  Hi   "))) (Wocc "Hi   " L))   2)
((ElimB "The old dog (called cerberus) barked.")  "The old dog  barked.")
((TIF 1)   "true")
((TIF 0)   "false")
((TIF 42)  "Argument can only be 0 or 1")
((Posns "aeiou" "facetious")  [2 4 6 7 8])
((RepChar ",." " " "This,is,an,example....")  "This is an example    ")
((RepElem '(lambda (X) (~(elt X #p))) "?" "\DHeader 1\M\J")  "?Header 1??")
((RemElem "\DHeader 1\M\J" '(lambda (X Y) (~ (elt X V))) #p)  "Header 1")
((RTNI 0.2342)   0)
((RTNI 0.5342)   1)
((p (a S (RP (* 10 (% 1 (% (/ (i [0 10]) 5)))) 3)))  11)
(S  [0 5.878 9.511 9.511 5.878 0 -5.878 -9.511 -9.511 -5.878 0])
((RTNI S)  [0 6 10 10 6 0 -6 -10 -10 -6 0])
((gup (gup [1 2 2 3 3 2 2 1]))  [1 3 4 7 8 5 6 2])
( (avgv [1 2 2 3 3 2 2 1])  [2])
((let ((F (/ (i 5) 2))) (eql (! (-  F 1)) (/ (! F) F)))  t)
((El "Sue" (p [4 5] "HelloPollyFred Joe  "))  0)
((El "Joe" (p [4 5] "HelloPollyFred Joe  "))  1)
((El (p [2 3] "JoeSue")   (p [4 5] "HelloPollyFred Joe  "))  [0 0 0 1])
((El (p [2 4] "FredJoe ") (p [4 5] "HelloPollyFred Joe  "))  [0 0 1 1])
((eql (Ccomp (LTM 5) (p [5 5] (i 25))) (* (LTM 5) (p [5 5] (i 25))))  t)
((LOOKUP "Fred" (p [4 5] "HelloPollyFred Joe  "))  3)
((/ (* 6 10) (* 3 5))     4)
((r '/ -1 -1 [6 3 10 5])  4)
((eql (CT3a) (CT3b))  t)
((eql (CT4a) (CT4b))  t)
((LeftZero [1 1 0 1 0 0 1 1])  3)
((RemElem [1 2 0 3 0 1 3] '= 0)  [1 2 3 1 3])
((Mask [1 5 9])  [1 0 0 0 1 0 0 0 1])
((CountOcc (p 31 (i 5)))  [7 6 6 6 6])
((let ((V [0 1 0 1 0 1 1 ])) (eql V (Mask (FindOnes V))))  t)
((LocMax [1 4 -99 3 100 12 2])  5)
((FFPos  [34  35 46  3 54 2 35] (i 5))  2)
((Frac 3.14159)  .14159)
((Int -1.1)  -1)
((int -1.23)  -1)
((FracSplit 3.14159)  [3 .14159])
((APV 1 10 3)  [1 4 7 10 13 16 19 22 25 28])
((CountS 2 [1 2 2 2 2 23 34  3 3 32 2 2 2 2])  8)
((Orig (s '+ (i 10)))  [1 2 3 4 5 6 7 8 9 10])
((UnScan (s '+ (i 10)))  [1 2 3 4 5 6 7 8 9 10])
((B2I [0 0 0 0 1 0 0 1])  9)
((let ((M (o '* (i 5) (i 5)))) (eql (FoFn ColDiag M 5) M))  t)
((FlipZero [1 2 0 4 0 2 1] 3)  [1 2 3 4 3 2 1])
((Poly  [4 -2 0 3] 1)  5)
((Poly  [4 -2 0 3] 2)  27)
((r '+ (o '= (i 5) (Bucket [6 7 8 12 13 13 17 20 23 24] 5)))  [3 3 0 2 2])
((M5a "abcde" "ABCDE" [1 0 1 0 1 0 1 0 1 0])  "aAbBcCdDeE")
((M5a "abcde" "ABCDE" [1 1 0 0 1 1 0 0 0 1])  "abABcdCDEe")
((M5b "abcde" "ABCDE" [1 0 1 0 1 0 1 0 1 0])  "aAbBcCdDeE")
((M5b "abcde" "ABCDE" [1 1 0 0 1 1 0 0 0 1])  "abABcdCDEe")
((Ins "Hello Polly" "dear " 6)  "Hello dear Polly")
((DPar "(i(|(- B A)))")  [1 1 2 2 3 3 3 3 3 3 3 2 1])
((CIFO 1 (IDM 5))  [1 2 3 4 5])
((CIFO 0 (IDM 5))  [2 1 1 1 1])
((r '+ (elt "aeiou" "facetiousness"))  5)
((TMkGap)  t)
((RP (+ (* (/ (i 4) 100) 10) 1.23456789) 3)   [1.335  1.435  1.535  1.635])
((DET (* 4(IDM 3)))  64)
((INTRSCT (p [4 2] [-1 -1 1 1 1 -1 -1 1]))  1)
((INTRSCT (p [4 2] [-1 -1 0 1 0 -1 1 1]))   0)
((CVX  (p [4 2] [-1 -1  -1 1  1 1   1 -1 ]))  [1])
((CVX  (p [4 2] [-1 -1  -1 1  1 1   0 0 ]))  [0])
((INSIDE [1  1]   (p [4 2] [-1 -1  -1 1  1 1   1 -1 ]))    1)
((INSIDE [1  1.1] (p [4 2] [-1 -1  -1 1  1 1   1 -1 ]))    0)
((mst 4)          [1])
((mst 5)          [1 1])
((mst 4)          [2 1])
((mst 7)          [2 1 1])
((mst 7)          [2 1 2])
((se 'A mst)      [4 5 7])
((se '(RST) mst)  [])
((se 'A mst)      [])
((eql (Rep (i 3) (i 3)) (k (i 3) (i 3)))  t)
((Rep (i 3) (i 3))  [1 2 2 3 3 3])
((tr (FIND (+ (p [2 3] (i 6)) (p [2 1] [12 14])) (p [5 5] (i 25))))  [3 3])
((MaxSet (i 10) [1 1 2 2 2 1 1 2 2 1])  [10 9])
((FSPalin 100)        [1 2 3 11 22 26])
((sqr (FSPalin 100))  [1 4 9 121 484 676])
((eql (Ulam 5) (p [5 5] "--*-*--**-*-----*-*---*-*"))   t)
((Monotonic '<= [1 2 3 3 4 5])  1)
((Monotonic '< [1 2 3 3 4 5])   0)
((R2A "MCMLIV")  1954)
((A2R 1954)  "MCMLIV")
((LowerCase "TheCamelIsNoMore")  "thecamelisnomore")
((UpperCase "please don't shout")   "PLEASE DON'T SHOUT")
((LeftBit [0 0 1 1 0 1 1 0])    [0 0 1 0 0 0 0 0])
((eql (s '< -1 -1 [0 0 1 1 0 1 1 0]) (LeftBit [0 0 1 1 0 1 1 0]))  t)
	  )))
    (if (member nil
		(mapcar '(lambda (X)
			   (cond ((eql (eval (car X)) (cadr X)) t)
				 (t (print X) nil))) T)) "TIDIOMS Failed"
      "TIDIOMS Success")))


(defun TBind ()
    (let ((A "hello")
	  (F3 '(lambda (A B A) (+ A B A)))
	  (F5 '(lambda (A B A C A) (+ A B A C A)))
	  (F7 '(lambda (A B A C A D A) (+ A B A C A D A))))
      (if (and (equal (let ((A 1) (B 2) (A 3)) (list A B)) '(3 2))
	       (equal (let* ((A 1) (B (+ 1 A)) (C 3) (A (+ A B C)))
			(list A B C))
		      '(6 2 3))
	       (equal (F3 1 2 3)       8)
	       (equal (F5 1 2 3 4 5)  21)
	       (equal (F7 1 2 3 4 5 6 7) 40)
	       (equal A "hello"))
	  "TBind OK"
	"TBind Failed")))

(defun SUBSTRING (S STRING) (equal S (k (elt STRING S) STRING))) ; kompress and element

(defexpr TTIME (X) (let ((T1 (time))) (eval (car X)) (- (time) T1)))

(if DEBUG 
    (defexpr CHECK (Z)
      (print (caar Z)) 
      (tab 15) (print (aref (TTIME (a Z (eval (car Z)))) 1))
					;  (Dbreak "CHECK")
					;  (print (list 'x 'is x))
      (if  ; (zerop (r '+ (elt [t] <-> (implod 
	  (not (findc '(lambda (S) (equal (tk (- (p  S)) (low Z)) S))
		      '(ok works success succeeded cool winnah rocks passed)
		      t))
	  (print Z)))
  
  (defun CHECK (Z)
    (if  ; (zerop (r '+ (elt [t] <-> (implod 
	(not (findc '(lambda (S) (equal (tk (- (p  S)) (low Z)) S))
		    '(ok works success succeeded cool winnah rocks passed)
		    t))
	(print Z)
      ))
  )

(defun FIGHT (F N) 
  (prog ((P1 (peval `(FOR I 1 ,N (CHECK (,F))) nil))
	 (P2 (peval `(FOR I 1 ,N (CHECK (,F))) nil)))
	(wait -1 (list P1))
	(wait -1 (list P2))
	(print (list 'FIGHT F N 'Done))))
  

(defun Coverage (S) ; S (stats)
  (let ((L (len S)) R)
    (a R (/ (* 100 (LCOUNTF S '(lambda (X) (gt (cadr X) 0)))) L))
    (princl (fmt "Coverage = " R:5:2 "%"))
    R))

(defun Untested (S) ; S (stats)
  (mapcan '(lambda (X) (cond ((zerop (cadr X)) (list (car X))))) S))

(defun TQ (L) (prog ((#IO 1) T1 (IW (f (+ 1 (l 10 (c 1 L))))))
 (FOR X 0 L
      (setq T1 (time))
      (CHECK (TCOND))
      (CHECK (TCOMP))
      (CHECK (TPRED))
      (CHECK (TAPLPRED))
      (CHECK (TLC))
      (CHECK (TNCONC))
      (CHECK (TCAR))
      (CHECK (TLACS))
      (CHECK (TAPLOPS))
      (CHECK (TPLIST))
      (CHECK (TSET))
      (CHECK (TCAT 12 34))
      (CHECK (TIMPL))
      (CHECK (TGUP))
      (CHECK (TGDN))
      (CHECK (TPROG))
      (CHECK (TGOTO))
      (CHECK (TCT)) 
      (CHECK (TRET))
      (unless DEBUG (CHECK (TUWDP)))
      (CHECK (TREP))
      (CHECK (TLET))
      (CHECK (TIOTA))
      (CHECK (TISET))
      (CHECK (TASET))
      (CHECK (TAREF))
      (CHECK (TLAB))
      (CHECK (TLIST))
      (CHECK (TMEMB))
      (CHECK (TSUBST))
      (CHECK (TMCAR))
      (CHECK (TMCAN))
      (CHECK (TMN))
      (CHECK (TMLIST))
      (CHECK (TQUAD))
      (CHECK (TSUB))
      (CHECK (TIND))
      (CHECK (tfa))
      (CHECK (TFA))
      (CHECK (TNLG))
      (CHECK (TFV))
      (CHECK (TOOP))
      (CHECK (TCWCC 10 28))
      (CHECK (TMCF))
      (CHECK (TMCCWCC))
      (CHECK (TGENC))
      (CHECK (TMAT))
      (CHECK (TRND 10))
      (CHECK (TDEAL 100))
      (cond ((HASCAPS "j")
	     (CHECK (TCMPLX))
	     (CHECK (TFOUR))))
      (CHECK (TKOMP (? 100)))
      (CHECK (TEXPND))
      (CHECK (TSCAN))
      (CHECK (TIDELEM))
      (CHECK (TSORT))
      (CHECK (TFIXED_POINT))
      (CHECK (TENC))
      (CHECK (TDEC))
      (CHECK (TENCDEC))
      (CHECK (TDLL))
      (CHECK (TRR))
      (CHECK (TIDIOMS))
      (CHECK (TBind))
      (CHECK (TTR))
;      (CHECK (TSTKOVFL nil))
      (QT (| 100 X))
      (princ (fmt "Iteration " X:IW:0
		  " CPU Usage" (aref (- (time) T1) 1):6:3 "s"))
      (terpri))
 (fme "TQ End")
))
