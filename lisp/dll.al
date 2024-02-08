;;-*- mode: emacs-lisp; -*-
;; doubly linked list package dmp
;;

;;selectors
(defun dll-next (X) (cdr X))
(defun dll-prev (X) (cdar X))
(defun dll-val  (X) (caar X))
(defun dll-head (L) (cddr L))
(defun dll-tail (L) (caar L))

;;predicates
(defun dll-p    (L) (and (consp (car L)) (eq (car L) (cdr L))))
(defun dll-null (L) (and (dll-p L) (eq L (caar L)) (eq L (cddr L))))

;;constructor
(defun mkdll ()
  (let* ((L (cons nil nil)) (M (cons L L)))
    (rplaca L M) (rplacd L M) L))
;
;   +----------------------------------------------+
;   |  	       	       	       	                   |
;   |  L       	       	                           v
;   | +-------+      +-------+      +-------+      +-------+
;   | | * | * |  +-->| * | *-+--+-->| * | *-|----->| * | L |
;   | +-|---|-+  |   +-|-----+  |   +-|-----+      +-|-----+
;   |   v   v    |     v        |     v              v
;   | +-------+  |   +-------+  |   +-------+      +-------+
;   +-+-* | *-+--+   | A | L |  |   | B | * |      | C | * |
;     +-------+  |   +-------+  |   +-----|-+      +-----|-+
;	T    H 	 |              |         |              |
;      		 |	        +---------O--------------+
;      	       	 |      	       	  |
;	      	 +------------------------+
;

;;methods
(defun puthead (L X)
  (let ((E (acons X L (dll-head L))))
    (cond ((dll-null L) (rplaca (car L) E) (rplacd (car L) E))
	  (t (rplacd (caddr L) E) (rplacd (car L) E)))) X)

(defun gethead (L)
  (cond ((dll-null L) nil)
	(t (let ((E (dll-head L)))
	      	(cond ((eq E (dll-tail L)) ; singleton
	      	       (rplaca (car L) L) (rplacd (car L) L))
	      	      (t (rplacd (car L) (dll-next E))
	      	   	 (rplacd (car (dll-next E)) L)))
	      	(dll-val E)))))

(defun peekhead (L) (cond ((dll-null L) nil) (t (dll-val (dll-head L)))))

(defun puttail (L X)
  (let ((E (acons X (dll-tail L) L)))
    (cond ((dll-null L)  (rplaca (car L) E) (rplacd (car L) E))
	  (t (rplacd (dll-tail L) E) (rplaca (car L) E)))) X)

(defun gettail (L)
  (cond ((dll-null L) nil)
	(t (let ((E (dll-tail L)))
		 (cond ((eq E (dll-head L)) ; singleton
			(rplaca (car L) L) (rplacd (car L) L))
		       (t (rplacd (dll-prev E) L)
			  (rplaca (car L) (dll-prev E))))
		 (dll-val E)))))

(defun peektail (L) (cond ((dll-null L) nil) (t (dll-val (dll-tail L)))))

(defun dll-hlist (L) ; list from head
  (cond ((dll-null L) nil)
	(t ((label mapdaux lambda (X) 
		   (cond ((eq X L) nil)
			 (t (cons (dll-val X)
				  (mapdaux (dll-next X))))))
	   (dll-head L)))))

(defun dll-tlist (L) ; list from tail
  (cond ((dll-null L) nil)
	(t ((label mapdaux lambda (X) 
		   (cond ((eq X L) nil)
			 (t (cons (dll-val X)
				  (mapdaux (dll-prev X))))))
	   (dll-tail L)))))

(defun dll-len (L) ; length of list
  (let ((N 0)
	(R L)) 
	(cond ((not (dll-null L)) (setq R (dll-head L))))
        (while (ne R L) (incr N) (setq R (dll-next R))) N))
;;EOF


