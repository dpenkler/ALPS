(a HOME    (chr [27 91 72])
   CLEARL  (chr [27 91 50 75])
   CLEARTE (chr [27 91 75])
   CLEAR   (chr [27 91 74])
   BRITE   (chr [27 91 55 109])
   DULL    (chr [27 91 109])
   BELL    (chr [7])
   BEGIN   "\A"
   LEFT    "\B"
   QUIT    "\D"
   END     "\E"
   RIGHT   "\F"
   DOWN    "\N"
   UP      "\P"
   CODES   (pairlis (explod "ABCD") (list UP DOWN RIGHT LEFT) nil)
   ACC     (chr [32])
   ESC     (chr [27])
   LSQB    "["
   SROW 10
   LAST_SELECT_LIST nil ; this is a global for eficiency
   LAST_SELECT_TABS nil) ; this is a global for eficiency

(defun MAX_FIELD (X) 
     (prog (MAX) 
           (setq MAX 0) 
	   (while X
	     (cond ((gt (plen (car X)) MAX) 
		    (setq MAX (plen (car X)))))
	     (setq X (cdr X)))
	   (+ 1 MAX)))

(defun MKTAB (X) 
     (prog (MAX) 
           (setq MAX (MAX_FIELD X))
           (cond ((zerop MAX) '(1 9 17 25 33 41 49 57 65 73))
                 (t (explod (- (* MAX (i (/ #PW MAX))) (- MAX 1)))))))
           
(defun Locate (X Y)
         (cond((null X) nil)
              ((onep (r (quote ^) (= Y (p (p Y) (car X))))) (car X))
              (t (Locate (cdr X) Y))))


(defun GW  (X) (cond ((null CWORD) X) (t  (cat CWORD X))))
              
(defun ITEM (X) (caddr (car X)))

(defun SELECT (P L R) ; globals LAST_SELECT_XXXX 
  (prog (IPOS CPOS POS C X Y RMIN CROW RMAX CCOL CMAX CWORD CLOSE TABS
	      (ROW 'car)
	      (COL 'cadr)
	      (ITEM 'caddr)
	      (BRT '(lambda ()
		      (prat (ROW CPOS) (COL CPOS) {BRITE (ITEM CPOS) DULL})))
	      (DLL '(lambda ()
		      (prat (ROW CPOS) (COL CPOS) {DULL (ITEM CPOS)}))))
        (cond ((atom L) L) 
              (t (cond ((eq L LAST_SELECT_LIST) (setq TABS LAST_SELECT_TABS))  
                       (t (setq TABS (setq LAST_SELECT_TABS (MKTAB L)))))
                 (setq CLOSE (list ACC QUIT))
                 (setq C TABS)
                 (cond ((null R) (setq R 1))) 
                 (setq RMIN R)
                 (prat R 1 CLEAR)
                 (setq X L) 
                 (while X (push (list R (car C) (car X)) 'POS) 
                          (prat R (car C) DULL) 
                          (princ (car X)) 
                          (setq RMAX R)
                          (setq CMAX (car C))
                          (cond ((null (setq C (cdr C))) 
                                 (setq R (+ R  1)) 
                                 (setq C TABS) 
                                 (terpri))) 
                          (setq X (cdr X))) 
                 (setq R (+ R  1)) 
                 (setq POS (revl POS)) 
                 (nconc POS POS) 
                 (setq CPOS (car POS)) 
                 (setq IPOS POS) 
		 (BRT)
                 (prat R 1 (fmt "Select " P "\n"))
		 (setq CWORD nil)
                 (while (not (member (setq X (getc)) CLOSE)) 
		   (cond ((and (equal X ESC) (equal (getc) LSQB))
			  (a X (cdr (assoc (getc) CODES)))))
		   (cond ((eql X BEGIN) 
                               (setq CWORD nil)
                               (DLL)
			       (setq CROW (car CPOS) CCOL (car TABS))
                               (setq POS
				     (find '(lambda (X)
					      (and (eql (caar X) CROW)
						   (eql (cadar X) CCOL)))
					   IPOS t))
                               (setq CPOS (car POS)) 
                               (BRT))
			 ((eql X END) 
                               (setq CWORD nil)
                               (DLL)
			       (setq CROW (car CPOS) CCOL (car TABS))
                               (setq POS
				     (find '(lambda (X)
					      (and (eql (caar X) CROW)
						   (gt (cadar X)
						       (cadar (cdr X)))))
					   IPOS t))
                               (setq CPOS (car POS))
                               (BRT))
			 ((eql X RIGHT) 
                               (setq CWORD nil)
                               (DLL)
                               (setq POS (cdr POS)) 
                               (setq CPOS (car POS)) 
                               (BRT))
			 ((eql X LEFT) 
                               (setq CWORD nil)
                               (DLL) 
                               (setq POS (find 'cdr IPOS POS)) 
                               (setq CPOS (car POS)) 
			       (BRT))
			 ((eql X UP) 
                               (setq CWORD nil)
			       (DLL)
                               (setq CROW (- (car CPOS) 1))
                               (setq CCOL (cadr CPOS))
                               (setq CROW (cond ((lt CROW RMIN) RMAX)
                                                ((gt CROW RMAX) RMIN)
                                                (t CROW)))
                               (setq CROW (cond ((and (eq CROW RMAX)
                                                      (gt CCOL CMAX)) 
                                                              (- RMAX 1))
                                                (t CROW)))
                               (setq POS 
                                (find '(lambda (X)
                                        (and (eq (caar X) CROW)
                                             (eq (cadar X) CCOL)))
                                      IPOS t)) 
                               (setq CPOS (car POS)) 
			       (BRT))
			 ((eql X DOWN) 
                               (setq CWORD nil)
			       (DLL)
                               (setq CROW (+ (car CPOS) 1))
                               (setq CCOL (cadr CPOS))
                               (setq CROW (cond ((lt CROW RMIN) RMAX)
                                                ((gt CROW RMAX) RMIN)
                                                (t CROW)))
                               (setq CROW (cond ((and (eq CROW RMAX)
                                                      (gt CCOL CMAX)) RMIN)
                                                (t CROW)))
                               (setq POS 
                                (find '(lambda (X)
                                        (and (eq (caar X) CROW)
                                             (eq (cadar X) CCOL)))
                                      IPOS t)) 
                               (setq CPOS (car POS)) 
			       (BRT))
                              ((eq X 'b) (break))
			    
			      ((setq Y (Locate L (GW X))) 
			       (DLL)
                               (setq POS (findc ITEM POS Y)) 
                               (setq CPOS (car POS)) 
			       (BRT)
                               (setq CWORD (GW X))) 
                              (t (setq CWORD nil) (princ BELL))) 
                        (prat R 1 DULL)) 
		 (prat R 1 CLEARTE)
                 (cond ((equal X ACC) (caddr CPOS))
                       (t nil))))))

(defun GSEL (P L R HI LO ACT) 
   (prog (IPOS CPOS POS C X CLOSE) 
         (cond ((atom L) L) 
               (t (setq X L) 
                  (while X (push (list nil nil   
                                         (car X)) 
                                 (quote POS)) 
                           (setq X (cdr X))) 
                  (setq CLOSE (list ACC ESC))
                  (setq POS (revl POS)) 
                  (nconc POS POS) 
                  (setq CPOS (car POS)) 
                  (setq IPOS POS) 
                  (prat R 1 CLEARTE)
                  (HI (caddr CPOS)) 
                  (repeat (not ACT) 
                   (while (not (member (setq X (readc (quote #2:))) CLOSE)) 
                         (cond ((eq X RIGHT) 
                                (LO (caddr CPOS)) 
                                (setq POS (cdr POS)) 
                                (setq CPOS (car POS)) 
                                (prat R 1 CLEARTE)
                                (HI (caddr CPOS))) 
                               ((eq X LEFT) 
                                (LO (caddr CPOS)) 
                                (setq POS (find (quote cdr) IPOS POS)) 
                                (setq CPOS (car POS)) 
                                (prat R 1 CLEARTE)
                                (HI (caddr CPOS))) 
                               (t (prin0 BELL))) 
                         )
                      (cond (ACT (cond ((eq X ACC) (ACT (caddr CPOS)))
                                       (t (setq ACT nil))))) ; term loop
                     ) 
                  (LO (caddr CPOS))
                  (cond ((eq X ESC) nil)
                        (t (caddr CPOS)))))))
                       
 (defun GETAT (VAR EXCLUSION)
    (prog (VAL INPUT PRMPT)
       (prat SROW 1 CLEARTE)
       (setq PRMPT (fmt "Enter " VAR " ? "))
       (while (not INPUT)
              (prat SROW 1 CLEARL)
              (prin0 PRMPT)
              (setq VAL (ratom))
              (cond ((null VAL) (setq INPUT t))
                    ((member VAL EXCLUSION) 
                     (print (list VAL ': 'Duplicate VAR)))
                    (t (setq INPUT t))))
       (prin0 CLEARTE)
       (set VAR VAL)))
       
 (defun GETNUM (VAR MIN MAX)
   (prog (VAL INPUT PRMPT)
       (setq PRMPT (fmt "Enter " VAR " ? "))
       (prat SROW 1 CLEARTE)
       (while (not INPUT)
              (prat SROW 1 CLEARL)
              (prin0 PRMPT)
              (setq VAL (ratom)) 
              (cond ((null VAL) (setq INPUT t))
                    ((or (not (nump VAL)) (or (lt VAL MIN) (gt VAL MAX))) 
                     (print (fmt VAL ":" "Range Error" MIN "<=" VAR "<=" MAX)))
                    (t (setq INPUT t))))
       (princ CLEARTE)
       (set VAR VAL)))
