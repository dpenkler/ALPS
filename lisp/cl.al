;;;-*- mode: emacs-lisp -*-
;;; Some Common Lisp compatibility routines
(a second       'cadr
   third        'caddr
   rest         'cdr
   every        'genc
   some         'ngenc
   reverse      'rev
   length       'len
   defconstant  'Defcon
   defparameter 'Defcon
   symbol-name  'arg
   numberp      'nump
   expt         'exp
   make-symbol  'intern)

(defmacro first (L N)
  (cond ((null L) nil)
	((null N) `(car ,L))
	(t `(cond ((not (nump ,(car N))) (error "first bad argument"))
		  ((ge ,(car N) (len ,L)) ,L)
		  (t (revl (nthcdr (- (len ,L) ,(car N)) (revl ,L))))))))

(defexpr Defun (Name Parameters Body) ;; Defun for variable length args
  (set Name
    `(macro (Args)
       `(let ,((label mlet lambda (X Y)
	  	 (cond ((null X) nil)
		       ((null Y) (cons (car X) (mlet (cdr X) Y)))
		       (t (cons (list (car X) (car Y))
				(mlet (cdr X) (cdr Y))))))
	       ',Parameters Args)
	  ,',@Body)))
  Name)

;; (Defun List3 (X Y Z) (list X Y Z))
;; (List3)                   => (nil nil nil)
;; (List3 'hello)            => (hello nil nil)
;; (List3 'hello 'polly)     => (hello polly nil)
;; (List3 'hello 'polly  '!) => (hello polly !)
;;

(defun caadar        (X)   (car (cadar X)))
(defun remove-if-not (F L) (mapcan '(lambda (X) (if     (F X) (list X))) L))
(defun remove-if     (F L) (mapcan '(lambda (X) (unless (F X) (list X))) L))
(defun notany        (F L) (genc '(lambda (X) (not (F X))) L))
(defun nreconc       (x y) (nconc (nrev x) y))
(defun integerp      (N)   (onep (= N (f N))))
(defun position      (Pat Item Start Test)
  (let ((R Start)) 
	(if 
	    (findc '(lambda (X) 
		     (if (Test Pat X) t (incr R))) 
		  (nthcdr Start Item) t)
	    R nil)))

(defun sublis (Alist Tree) 
  (cond ((null Tree) nil)
	((ngenc '(lambda (X) (if (eql Tree (car X)) (cdr X))) Alist))
	((atom Tree) Tree)
	(t (cons (sublis Alist (car Tree))  (sublis Alist (cdr Tree))))))
 
(defmacro subseq     (L S E)
  `(cond ((null ',E) (nthcdr ,S ,L)) 
	 (t (let ((K ,S) (J (+ 1 ,@E)))
	      (mapcan '(lambda (X) (if (lt (incr K) J) (list X)))
		      (nthcdr K ,L))))))
(defmacro defvar (S V R) `(let () (unless (boundp ',S) (setq ,S ,V)) 
			       (if ,(car R) (put ',S '#DOC ',(car R)))))

(defmacro Defcon (S V R) `(let () (setq ,S ,V) (put ',S '#DOC ',R)))
(defmacro When   (C E)   `(cond (,C        ,@E)))
(defmacro Unless (C E)   `(cond ((not ,C)  ,@E)))
(defmacro If     (C T E) `(cond (,C ,T)   (,@E)))

(defmacro dolist (Head Body) 
  `(prog () 
	 (mapc '(lambda (,(car Head)) ,@Body) ,(cadr Head))
	 ,(caddr Head)))

(defmacro do (Vlist EndTest Body) 
  `(let ,(mapcar '(lambda (X) (list (car X) (cadr X))) Vlist)
     (while (not ,(car EndTest)) ,@Body
	    (mapc '(lambda (X Y) (set X Y))
		  ',(mapcar 'car Vlist)
		  (mapcar '(lambda (X) (eval X)) ',(mapcar 'caddr Vlist))))
     ,@(cdr EndTest)))

(defmacro do* (Vlist EndTest Body) 
  `(let* ,(mapcar '(lambda (X) (list (car X) (cadr X))) Vlist)
     (while (not ,(car EndTest)) ,@Body
	    (mapc '(lambda (X Y) (set X (eval Y)))
		  ',(mapcar 'car   Vlist)
		  ',(mapcar 'caddr Vlist)))
     ,@(cdr EndTest)))


(defun getEven (L) 
  (let ((B t)) (mapcan '(lambda (X) (if (a B (not B)) (list X))) L)))

(defun getOdd (L) 
  (let ((B nil)) (mapcan '(lambda (X) (if (a B (not B)) (list X))) L)))

(defmacro psetq (X) 
  `(mapcar '(lambda (X Y) (set X Y))
	 ',(getOdd X)
	 (mapcar '(lambda (X) (eval X)) ',(getEven X))))

(defun list-reverse (L)
  (do ((x L (cdr x)) (y nil (cons (car x) y))) ((null x) y)))
