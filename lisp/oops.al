(defun memc (X S) 
        (cond ((null S) nil) 
              ((eq (car (car S)) X) S) 
              (t (memc X (cdr S)))))
(defun dyi (X Y) (cond ((null X) Y)
                       ((assoc (caar X) Y) (dyi (cdr X) Y))
                       (t (dyi (cdr X) (cons (cons (caar X) (cdar X))Y)))))
(defexpr define_type (X) (put (car X) (quote type) (cadr X)) (car X))
(defun mk_inst (X) 
  (prog (Y Z I)
        (setq Y (get X 'type))
        (setq Z (cadr(assoc 'var Y)))
        (setq I (cdr (assoc 'inherit Y)))
        (setq Z (pairlis Z nil nil)) ; vars have precedence
        (setq Z (dyi (cdr(assoc 'method Y)) Z))
        (while I (setq Z (dyi (mk_inst (car I)) Z)) (setq I (cdr I)))
        Z))
(defun Send (X Y) (eval X Y))
(defun init_var (X Y Z) (eval (quote(set X Y)) Z))
(defun FIND_OBJ (OBLIS SELECTOR VALUE) 
                (car (find '(lambda (X) (Send SELECTOR (car X)))
                           OBLIS VALUE)))
(defun PR (X) (while X (pp (car X) 0 2) (setq X (cdr X))))
(defun B  (X) (Send '(break) X)) 
;(defun error (X) (prin0 '/error:) (print X) (break error))
(defun TOB () (prog (FRED JOE)
  (define_type TO (
    (var (V1 V2))
    (method
      (M0 . (lambda () (print (list V1 V2))))
      (M1 . (lambda (X) (setq V1 X)))
      (M2 . (lambda (X) (setq V2 X)))
    ) ; end methods
  ))
  (define_type SC (
     (var (V1 V2 J))
     (method 
        (M1 . (lambda () (setq V1 (cons V1 V2))))
	(MM . (lambda () (r '+ J)))
     )
     (inherit TO)
))
  (setq FRED (mk_inst 'TO))
  (break)
  (Send '(M0) FRED)
  (Send '(M1 'number_one) FRED)
  (Send '(M0) FRED)
  (Send '(M2 'number_two) FRED)
  (Send '(M0) FRED)
  (setq JOE (mk_inst 'SC))
  (init_var 'J (i 10) JOE)
  (B JOE)
  'done))

(setq OOPS t)

; the other way
(defun mbod (Mass Velocity)
  (let ((Colour)
	(Momentum '(lambda () (* Mass Velocity)))
	(Energy   '(lambda () (* Mass (r '+ (* Velocity Velocity))))))
    (if (lt Mass 0) (error "Negative mass"))
    (fun eval)))

; (a B (mbod 10 [3 4]))
; (a B (snd '(let ((Frog t)) (fun eval)) B)) ; add instance variable Frog to B
; (a Bods (mapcar mbod '(10 15 4) '([3 4] [0 0] [10 0])))   ; make a bunch of bodies
; (mapcar '(lambda (X) (snd '(Momentum) X)) Bods)  ; get their momentum
; => ([30 40] [0 0] [40 0])
; (implod 1.5 (mapcar '(lambda (X) (snd '(Energy) X)) Bods)) ; get their energy in a vector
; => [250 0 400]
; (r '+ (implod 1.5 (mapcar '(lambda (X) (snd '(Energy) X)) Bods))) ; total energy 
; => [650]
;;; Add a 'Show' method to all instances of mbod
; (a Bods (mapcar '(lambda (X) (snd '(let ((Show '(lambda () (princl (fmt "Velocity: " Velocity))))) (fun eval)) X)) Bods))
; (mapc '(lambda (X) (snd '(Show) X)) Bods)
; Velocity: [3 4]
; Velocity: [0 0]
; Velocity: [10 0]
(defun TB () 
  (prog (Body Earth)
	(defun Body (Mass Velocity)
	  (let ((Colour)
		(Momentum '(lambda () (* Mass Velocity)))
		(Speed '(lambda () (sqrt (r '+ (sqr Velocity)))))
		(Energy '(lambda ()(* Mass (sqr (Speed))))))
	    (if (lt Mass 0) (error "Negative mass")
	      (fun eval))))  ; return a closure around eval
	(setq Earth (Body 5.97e24 [14500 25114.73671]))
	(Earth '(break))))
; test abind
(defun TABIND () 
 (let ((Env (pairlis '(A B C A) '(1 2 3 4) nil))
	(S '(lambda () (print (eval 'A Env))))
	(SA '(lambda () (print Env)))
	(T '(lambda (X) (eval '(setq A X) Env))))
    (S)
    (SA)
    (T 10)
    (S)
    (SA)
    ))

