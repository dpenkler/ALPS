;;;-*- mode: emacs-lisp -*-
;;					
;; Whimsical attempt at an apl style direct function definition
;;
(require 'ff) ;; FindFree
;; body may not bind a (first arg) w (second arg) d (self ref to function)
(df dfm (x) `(label d lambda (w) ,@x))   ; strictly monadic function
(df dfd (x) `(label d lambda (a w) ,@x)) ; strictly dyadic function
(df dfmd (x) ; monadic that possible recurs dyadicallly
    (let* ((faa (dfm (cond ((atom w) nil) ; find assignment to a (a a val)
			   ((and (eq (car w) 'a) (eq (cadr w) 'a)) w)
			   (t (append (d (car w)) (d (cdr w)))))))
	   (frm (dfd (cond ((atom w) w)  ; remove a from w
			   ((equal a (car w)) (d a (cdr w)))
			   (t (cons (d a (car w)) (d a (cdr w)))))))
	   (a (faa x)))  ; assign assigment form to local a
      `(lambda (w) ((label d lambda (a w)
			   ,@(frm a x) ; remove assignment form from body
			   )
		    ,(caddr a) ; first arg is value in assignment form
		    w))))

(df ddd (f) ; try figure out which type of function it is at application time
    `(macro (x)
	    `(if (onep (len ',x))
		 ((label d lambda (w) (let (a) ,',@f)) ,(car x))
	       ((label d lambda (a w) ,',@f) ,(car x) ,(cadr x)))))

(de TDFN () ;; little test
    (let ((fibd (dfmd (a a [0 1]) (print a)
		      (if (zerop w) (p [] a)
			(d (dp 1 (cat a (r '+ a))) (- w 1)))))
	  (fibr '(lambda (n) ((label F lambda (x y)
				     (cond ((zerop n) x)
					   ((decr n) (F y (+ y x)))))
			      0 1))))
      (FOR N 1 100 (if (neql (fibd N) (fibr N)) (error (fmt "failed at" N))))))
      
(df dff (x) "direct function definition (dff <sexpr>+)"
    ;; if only w occurs in sexprs -> monadic
    ;; if a and w occur -> dyadic
    ;; if d occurs then use label d for recursion
    ;; if other symbols occur -> bind them into a (let)
    (let* ((f (FindFree x '(w) nil)) l)
      (cond ((memq 'a f) (a l '(a w) f (delete 'a f))) ;; dyadic
	    (t (a l '(w))))                           ;; monadic
      (cond ((null f) `(lambda ,l ,@x)) ;; no loc, no rec
	    ((memq 'd f) ;; recursion ?
	     (if (onep (len f)) `(label d lambda ,l ,@x) ;; no loc, rec
	       `(label d lambda ,l (let ,(delete 'd f) ,@x)))) ;; loc & rec
	    (t `(lambda ,l (let ,f ,@x)))))) ;; loc, no rec

;; Examples 
;;
;; (mapcar (dff { "Hello " w}) '(Fred Susi Mike))
;; => ("Hello Fred" "Hello Susi" "Hello Mike")

;; Monadic applications
;;(dff (* w 2)) ;; no local vars, no recursion
;; => (lambda (w) (* w 2))
;; (dff (a G [1 3] H [4 5]) (list w G H)) ;; local vars, no recursion
;; => (lambda (w) (let (G H) (a G [1 3] H [4 5]) (list w G H)))
;; (dff (cond ((zerop w) nil) (t (cons w (d (- w 1)))))) ;; no locals,recursion
;; => (label d lambda (w) (cond ((zerop w) nil) (t (cons w (d (- w 1))))))
;; local vars and recursion
;; (pp(dff (cond ((zerop w) nil) (t (a P (* w 2)) (cons P (d(- w 1)))))))
;; => (label d lambda (w) 
;;      (let (P) (cond ((zerop w) nil) (t (a P (* w 2)) (cons P (d (- w 1)))))))
;;
;; Dyadic applications
;; (dff (* w a))  ;; no local vars, no recursion
;; => (lambda (a w) (* w a))
;; (dff (a a 4 G [1 3] H [4 5]) (list a w G H)) ;; local vars, no recursion
;; => (lambda (a w) (let (G H) (a a 4 G [1 3] H [4 5]) (list a w G H)))
;;  (dff (cond ((zerop w) nil) (t (cons (* a w) (d a (- w 1)))))) ;; no locals,recursion
;; => (label d lambda (a w) (cond ((zerop w) nil)
;;                                (t (cons (* a w) (d a (- w 1))))))
;; dyadic with local vars and recursion
;; (pp(dff (cond ((zerop w) nil) (t (a P (* a w 2)) (cons P (d a (- w 1)))))))
;; => (label d lambda (a w) 
;;      (let (P) (cond ((zerop w) nil) (t (a P (* a w 2)) (cons P (d a (- w 1))))))

(a $ dff) ;; shorter name

;; Note: When FindFree annotates symbols with ref and set we can also do
;;       a better job

