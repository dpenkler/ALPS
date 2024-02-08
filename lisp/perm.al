;; -*- emacs-lisp -*-
;;
;; Various lisp functions related to permutations

(require 'dfns)

(de nperm (n) "number of permutations of a set of cardinality n" (! n))

(defun GP (S) "Generate permutations of S (based on GENTUPLES from genset.al)"
  ((dfd
    (when w
      (if (memq (car w) a) (d a (cdr w))
	(nconc ((lambda (X) (if (eq (len X) (len S)) (list (revl X)) (d X S)))
		(cons (car w) a))
	       (d a (cdr w))))))
   nil S))

(de M (X A)
    (if (null X) nil
      (cons (append (list '* (car X)) A (cdr X))
	    (M (cdr X) (append A (list (car X)))))))

;; (M  '(A B C) nil) ;=>
;; ((* A B C) (* B A C) (* C A B))

(de M1 (S A)
    (print (list S A))
    (if (null S) (list nil)
      (mapn '(lambda (X)
		 (mapcar '(lambda (Y) (cons (car X) (cons '* Y)))
			 (M1 (append (cdr X) A) (list (car X)))))
	    S)))

(de M2 (X) "Based on A&SS (perm) sans auxiliary functions except (lin)"
    (if (null X) (list nil)
      (mapcan '(lambda (x) (mapcar lin x))
	      (mapcar '(lambda (x)
			 (mapcar '(lambda (p) (cons x p))
				 (M2 (mapcan '(lambda (y)
						(if (eq y x) nil (list y)))
					     X))))
	  X))))

;; From Abelson and Sussmans'
(defun perm (S)
  (if (null S) (list S)
    (flatmap '(lambda (x) (mapcar '(lambda (p) (cons x p)) (perm (remove x S))))
	     S)))

(defun perm2 (S) ;; this one crashes (len (perm2 '(a b c))) => 4
  ;;
  ;;  (perm2 '(a b c)) =>
  ;; ((a b c) (a c b) (b a) 
  ;; 8hXý÷øxFault address: 0x00007F0008000000
  ;; Stack address: 0x00007F0002EECB50
  ;;    0  10         1         9         3   0:00:00.0 print

  (cond ((null S) (list S))
	((atom S) (error "WTF"))
	(t (flatmap '(lambda (x) (mapcar '(lambda (p) (cons x p))
				  (perm2 (delete x S))))
	     S))))

(defun perm3 (S) ;; this one does not crash, makes copy of delete arg2
  (if (null S) (list S)
    (flatmap '(lambda (x) (mapcar '(lambda (p) (cons x p))
				  (perm3 (delete x (append S)))))
	     S)))


(defun flatmap (f s) (flatten (mapcar f s))) 

(defun remove (x l) (mapcan '(lambda (y) (if (eq x y) nil (list y))) l))

(defun flatten (S) (accumulate 'append-streams nil S))

(defun accumulate (C I S) (if (null S) I (C (car S) (accumulate C I (cdr S)))))

(defun append-streams (a b)
       (cond ((null a) b)
	     ((atom a) (cons a b))
	     (t (append a b))))

(defun TPerm (M)
  (FOR I 1 M
       (let ((N (explod (i I))))
	 (print (list (nperm I) (len (M2 N)) (equal (perm N) (M2 N)))))))

;; (TPerm 7)
;; (1 1 t)
;; (2 2 t)
;; (6 6 t)
;; (24 24 t)
;; (120 120 t)
;; (720 720 t)
;; (5040 5040 t)
;; alpsf
;;       (TIME (p(mf(rr 9))))
;; [9 362880]
;; [.369502134 .268613 .100887 0 0]
;;       (TIME (len (GP '(a b c d e f g h i))))
;; 362880
;; [1.0620169 1.042988 .019028 0 0]
;;       (TIME (len (GP (explod (i 9)))))
;; 362880
;; [1.190748308 1.190748 0 0 0]
;;       (TIME (len (M2 (explod (i 9)))))
;; 362880
;; [2.073840215 2.008411 .065429 0 0]
;;       (TIME (len (perm (explod (i 9)))))
;; 362880
;; [.961829747 .897744 .064085 0 0]


;; From Section 3.3 of Iverson's Notation as a tool for thought

(a rr (dfm (+ 1 (enc (rev (i w))  (- (i (! w)) 1)))))
;;       (rr 4)
;; [1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4
;;  1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3 1 1 2 2 3 3
;;  1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2
;;  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]

(a mf (dfm (if (zerop (tk 1 (p w))) w
	     (let ((x (d (dp [1 0] w))))
	       (cat 1 (aref w (rav 1) ())
		    (+ x (<= (aref w (p (tk 1 (p x)) 1) ()) x)))))))
;;
;;	(mf (rr 4))
;; [1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4
;;  2 2 3 3 4 4 1 1 3 3 4 4 1 1 2 2 4 4 1 1 2 2 3 3
;;  3 4 2 4 2 3 3 4 1 4 1 3 2 4 1 4 1 2 2 3 1 3 1 2
;;  4 3 4 2 3 2 4 3 4 1 3 1 4 2 4 1 2 1 3 2 3 1 2 1]
	       
(equal (GP '(a b c d))
       (mapcar 'explod (explod 2 (aref $[a b c d] (mf (rr 4)))))) ;=> t

