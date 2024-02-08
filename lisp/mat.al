(de cpxm/ (M) "Inverse of a complex matrix M"
    (if (nump M) (m/ M)
      (let* ((A (re M)) (B (im M)) (A-1 (m/ A)) (B-1 (m/ B))
	     (C (m/ (+ A (m* B (m* A-1 B)))))
	     (D (m/ (+ B (m* A (m* B-1 A))))))
	(- C (* 0j1 D)))))

(de TCPXM/ () ;; from dyalog APL
    (let ((X (p [5 5]
	       [-37j-41  25j015  -5j-09   3j020 -29j041
		-46j026  17j-24  17j-46  43j023 -12j-18
		 1j013  33j025 -47j049 -45j-14   2j-26
		 17j048 -50j022 -12j025 -44j015  -9j-43
		 18j013   8j038  43j-23  34j-07   2j026]))
	  (ID (o '= (i 5) (i 5))))
      (r 'c (rav (| (- ID (. '+ '* X (cpxm/ X))))))))
