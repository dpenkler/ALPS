;; -*- mode: emacs-lisp; -*-
;;
;; Set operators in LISP and APL
;; A little corner of Cantor's paradise
;;
;; note (subset) (member) (memq) are builtins most functions use memq
;; which will not work for comparing sets of tuples for example
;;

(defun SETEQ (X Y) ; set equivalence
        (and (SUBSET X Y) (SUBSET Y X)))

(defun SETEQUAL (X Y) ; set equivalence
        (and (genc '(lambda (X) (member X Y)) X)
	     (genc '(lambda (Y) (member Y X)) Y)))

(defun SUBSET (X Y) ; is X a subset of Y 
        (cond ((null X) t) 
              ((memq (car X) Y) (SUBSET (cdr X) Y)) 
              (t nil)))

(defun subsetp (list1 list2) ; stolen from cl.el
  (cond ((null list1) t) ((null list2) nil)
	((equal list1 list2) t)
	(t (while (and list1 (memq (car list1) list2)) (pop 'list1))
	   (null list1))))

(defun subsetq (A B) (genc '(lambda (X) (memq X B)) A)) ; alps style

(defun subsetg (A B) (genc '(lambda (X) (member X B)) A)) ; alps style

(defun SCOMP (A B) (remove-if '(lambda (X) (member X B)) A))

(defun scomp (A B) (mapcan '(lambda (X) (unless (memq X B) (list X))) A))

(defun UNION (X Y) (append (scomp X Y) Y))

(defun SUNION (A B) 
  (cond ((null A) B)
	((memq (car A) B) (SUNION (cdr A) B))
	(t (SUNION (cdr A) (cons (car A) B)))))

(defun sunion (A B) 
  (append (mapcan '(lambda (X) (unless (memq X B) (list X))) A) B))

(defun SETINT (A B)  ; Set intersection of set A with set B
  (cond ((null A) nil)
	((memq (car A) B) (cons (car A) (SETINT (cdr A) B)))
	(t (SETINT (cdr A) B))))

(defun setint (A B) (mapcan '(lambda (X) (if (member X B) (list X))) A))

(defun setintq (A B) (mapcan '(lambda (X) (if (memq X B) (list X))) A))

(defun SUNIQ (S) ;; remove duplicate elements, recursive version
  ((label suny lambda (X Y)
	  (cond ((null X) Y)
		((memq (car X) Y) (suny (cdr X) Y))
		(t (suny (cdr X) (append Y (list (car X)))))))
   S nil))

(defun suniq (S) ;; remove duplicate elements, iterative version
  (mapn '(lambda (X) (unless (memq (car X) (cdr X)) (list (car X)))) S))

; Array complement: returns vector of elements in A not in B
(defun ACOMP (A B) (k (> (- (ind B A) #IO) (- (p B) 1)) A))

; Array intersection: returns vector of elements in A that are also in B
(defun AINT (A B) (k (< (- (ind B A) #IO) (p B)) A))

; Array union

(defun AUNION (A B) (cat (ACOMP A B) B))

(defun N2 (N)
  ;; Total number of partitions of set S with n elements into
  ;; S1 and S2 with n1 and n2 elements respectively
  ;; (r '+ (/ (! n) (* (! n1) (! n2)))) over n+1 decompositions of n=n1+n2
  (let* ((#IO 0) (!N1 (! (i (+ N 1))))) (r '+ (/ (! N) (* !N1 (rev !N1))))))

(defun TN2 (N) (eq (N2 N) (exp 2 N)))


; (FOR I 1 200 (print (list I (TN2 I) (- (N2 I) (exp 2 I)))))

;; Hermann Weyl Phil of Math & Nat Science p23
(a M (p [7 7]
	[  0 1 1 1 0 0 0
	   1 0 1 0 1 0 0
	   1 1 0 0 0 1 0
	   1 0 0 1 0 0 1
	   0 1 0 0 1 0 1
	   0 0 1 0 0 1 1
	   0 0 0 1 1 1 0
	   ]))
;;; Showing that any two rows of M contain exactly one pair of 1's
;;; in the same column.
;;;(r '+ (^ (aref M (aref (CUUApairs 7) 1 ()) ())
;;;         (aref M (aref (CUUApairs 7) 2 ()) ())))
;;;=>
;[1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1]
;;;(r '+ (^ (aref M (aref (UApairs 7 7) 1 ()) ())
;;;         (aref M (aref (UApairs 7 7) 2 ()) ())))
;;;=>
;[1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
;    1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1]
;;;(r '+ (^ (aref M (aref (Apairs 7 7) 1 ()) ())
;;;         (aref M (aref (Apairs 7 7) 2 ()) ())))
;;;=> 
;[3  1  1  1  1  1  1  1  3  1  1  1  1  1  1  1  3  1  1  1  1  1  1  1  3  1
;    1  1  1  1  1  1  3  1  1  1  1  1  1  1  3  1  1  1  1  1  1  1  3]
;;; Showing that any two distinct lines intersect at only one point
;;;(r '+ 1 (^ (aref M () (aref (Apairs 7 7) 1 ()))
;;;           (aref M () (aref (Apairs 7 7) 2 ()) )))
;;;=>
;[3  1  1  1  1  1  1  1  3  1  1  1  1  1  1  1  3  1  1  1  1  1  1  1  3  1
;    1  1  1  1  1  1  3  1  1  1  1  1  1  1  3  1  1  1  1  1  1  1  3]
;;;... obvious by symmetry of M: (eql M (tr M)) => t



