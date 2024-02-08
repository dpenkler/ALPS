; quasiquote adventure

(a qq   'qquote)
(a qqc  ',)
(a qqca ',@)

;(defexpr qquote (X) (qqexp (car X))) ;causes macro expansion of initial qquote
(de qqexp (X)
    (cond ((atom X) (list 'quote X))
	  ((eq (car X) qqc ) (cadr X))
	  ((eq (car X) qqca) (error (fmt "Can't have " qqca " here")))
	  ((eq (car X) qq  ) (qqexp (qqexp (cadr X))))
	  (t (list 'append (qqexpl (car X)) (qqexp (cdr X))))))

(de qqexpl (X)
    (cond ((atom X) (list 'quote (list X)))
	((eq (car X) qqc ) (list 'list (cadr X)))
	((eq (car X) qqca) (cadr X))
	((eq (car X) qq  ) (qqexpl (qqexp (cadr X))))
	(t (list 'list (list 'append (qqexpl (car X)) (qqexp (cdr X)))))))

;; CLM thanks to Guy Steele 

(a *bq-quote-nil*   (list 'quote nil))
(a *bq-simplify* t)

; causes macro expansion of initial qquote
(de qquote (X) (bq-completely-process X))

(de bq-completely-process (x) 
  (let ((raw-result (bq-process x)))
    (cond (*bq-simplify* (bq-simplify raw-result))
	  (t raw-result))))

(de bq-process (x) 
  (cond ((atom x)          (list 'quote x)) 
        ((eq (car x) qq)   (bq-process (bq-completely-process (cadr x)))) 
        ((eq (car x) qqc)  (cadr x))
        ((eq (car x) qqca) (error (fmt ",@" (cadr x) " after `"))) 
        (t (prog 
	    ((p x) q)
	    (while (consp p)
	      (cond ((eq (car p) qqc) 
		     (cond ((cddr p) (error (fmt "Malformed ," p))))
		     (return (cons 'append (nconc (nrev q) (list (cadr p))))))
		    ((eq (car p) qqca) (error (fmt "Dotted ,@" p))))
	      (a q (cons (bracket (car p)) q) p (cdr p)))
	    (cons 'append (nconc (nrev q) (list (list 'quote p))))))))

;;; This implements the bracket operator of the formal rules.
(de bracket (x) 
  (cond ((atom x)          (list 'list (bq-process x))) 
        ((eq (car x) qqc)  (list 'list (cadr x))) 
        ((eq (car x) qqca) (cadr x)) 
        (t                 (list 'list (bq-process x)))))

;;; This auxiliary function is like MAPCAR but has two extra 
;;; purposes: (1) it handles dotted lists; (2) it tries to make 
;;; the result share with the argument x as much as possible.
(de maptree (fn x) 
  (cond ((atom x) (fn x))
	(t (let ((a (fn (car x))) 
		 (d (maptree fn (cdr x)))) 
	     (cond ((and (equal a (car x)) (equal d (cdr x))) x)
		   (t (cons a d)))))))

;;; can't have dependency on cl since it contains quasiquotes
;;; so defining this here
(de notany (F L)  (genc '(lambda (X) (not (F X))) L))

;;; This predicate is true of a form that when read looked like ,@foo 
(de bq-splicing-frob (x) (and (consp x) (eq (car x) qqca)))
 
;;; This predicate is true of a form that when read looked like ,foo or ,@foo
(de bq-frob (x) (and (consp x) (memq (car x) '(qqc qqca))))

;;; The simplifier essentially looks for calls to 'append and 
;;; tries to simplify them.  The arguments to 'append are 
;;; processed from right to left, building up a replacement form. 
;;; At each step a number of special cases are handled that, 
;;; loosely speaking, look like this: 
;;; 
;;;  (append (list a b c) foo) => (list* a b c foo) 
;;;       provided a, b, c are not splicing frobs 
;;;  (append (list* a b c) foo) => (list* a b (append c foo)) 
;;;       provided a, b, c are not splicing frobs 
;;;  (append (quote (x)) foo) => (list* (quote x) foo) 
(de bq-simplify (x) 
  (cond ((atom x) x)
	(t (let ((x (cond ((eq (car x) 'quote) x) (t (maptree bq-simplify x)))))
	     (cond ((not (eq (car x) 'append)) x) (t (bq-simplify-args x)))))))

(de bq-simplify-args (x) 
  (let ((args (revl (cdr x))) result)
    (while args
      (a result 
         (cond ((atom (car args)) (bq-attach-append 'append (car args) result)) 
               ((and (eq (caar args) 'list) 
                     (notany bq-splicing-frob (cdar args))) 
                (bq-attach-conses (cdar args) result)) 
               ((and (eq (caar args) 'list*) 
		     (notany bq-splicing-frob (cdar args))) 
                (bq-attach-conses 
                  (revl (cdr (revl (cdar args)))) 
                  (bq-attach-append 'append (car (last (car args))) result))) 
               ((and (eq (caar args) 'quote) 
                     (consp (cadar args)) 
                     (not (bq-frob (cadar args))) 
                     (null (cddar args))) 
                (bq-attach-conses (list (list 'quote (car(cadar args))))
				  result)) 
	       (t (bq-attach-append 'append (car args) result))))
	 (a args (cdr args)))
    result))

(de null-or-quoted (x) (or (null x) (and (consp x) (eq (car x) 'quote))))

;;; When bq-attach-append is called, op should be append 
;;; or nconc.  This produces a form (op item result) but 
;;; some simplifications are done on the fly: 
;;; 
;;;  (op '(a b c) '(d e f g)) => '(a b c d e f g) 
;;;  (op item 'nil) => item, provided item is not a splicable frob 
;;;  (op item 'nil) => (op item), if item is a splicable frob 
;;;  (op item (op a b c)) => (op item a b c)
(de bq-attach-append (op item result) 
  (cond ((and (null-or-quoted item) (null-or-quoted result)) 
         (list 'quote (append (cadr item) (cadr result)))) 
        ((or (null result) (equal result *bq-quote-nil*)) 
         (cond ((bq-splicing-frob item) (list op item)) (t item)))
        ((and (consp result) (eq (car result) op)) 
         (list* (car result) item (cdr result))) 
        (t (list op item result))))

;;; The effect of bq-attach-conses is to produce a form as if by 
;;; `(list* ,@items ,result) but some simplifications are done 
;;; on the fly. 
;;; 
;;;  (list* 'a 'b 'c 'd) => '(a b c . d) 
;;;  (list* a b c 'nil) => (list a b c) 
;;;  (list* a b c (list* d e f g)) => (list* a b c d e f g) 
;;;  (list* a b c (list d e f g)) => (list a b c d e f g)
(de bq-attach-conses (items result) 
  (cond ((and (genc null-or-quoted items) (null-or-quoted result)) 
         (list 'quote (append (mapcar 'cadr items) (cadr result)))) 
        ((or (null result) (equal result *bq-quote-nil*)) (cons 'list items)) 
        ((and (consp result) (memq (car result) '(list list*)))
         (cons (car result) (append items (cdr result)))) 
        (t (cons 'list* (append items (list result))))))
