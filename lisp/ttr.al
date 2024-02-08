
(defun GenTr (S) 
  "(GenTr <number of elements in dimension> <number of dimensions>)"
  (let ((M (p S (i (r '* S))))
        (D (p S)))
  (mapcar '(lambda (X) (eql (TR X M) (tr X M)))
          (remove-if 'notcomplete (mapcar 'implod (UTUP (explod (i D)) D))))))

(defun notcomplete (X) ((lambda (X) (or (zerop (tk 1 X)) (neql X (s '^ X))))
			(r 'c (o '= (i (p X)) X))))

(defun TR (P M)  ; attempt at generic
  (let* ((#GS "I0")
         (PR (SR P M))  ; shape of R
	 ;; can't use fsubr gensym as function in map functions (for now...)
         (VL (mapcar 'eval (explod (p (p P) $[(gensym)])))) ;; var for each dim
         (VM (implod nil t VL)) ; list of variables
	 (MSubs (if (lt (p PR) (p P)) (aref VM P) (aref VM (InvPerm P))))
         (RSubs (explod (aref VM (i (p PR)))))
         (R (p PR 0))
	 ;; make nested loops with vars in L ending at I around body B
	 (ML '(lambda (L I B) 
		(cond (L (append `(FOR ,(car L) 0 ,(car I)) 
				 (list (ML (cdr L) (cdr I) B))))
		      (t B))))
	 (Body `(aset R (aref M ,@(explod MSubs)) ,@RSubs))
	 (Fun  `(lambda () (let ((#IO 0)),(ML RSubs (explod (- PR 1)) Body)))))
    (Fun) R))

(defun SR (P M) ; shape of (tr M P)
  (let* ((RM (rank M))
	 (PM (shape M))
	 (RR (+ (= #IO) (r 'c P)))
	 (SR (p RR 0)))
    (if (neql (shape P) RM)
	(error "Require: (eq (shape P) (rank M)) => t)"))
    (if (zerop (r '^ (elt P (i RM)))) (error "Index vector out of bounds"))
    (if (onep  (r '^ (elt (i RM) P))) 
	(aset SR (aref PM P) (i RR)) ; Case 1 permute
      (FOR I #IO (- RR (= #IO)) ; Case 2 diagonal
	   (aset SR (r 'f (k (= P I) PM)) I)))
    SR))
