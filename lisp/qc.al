;-*- mode: emacs-lisp -*-
;;
;; QC simulation
;;
(require 'gcd)
(require 'prime)
(require 'dfns)

;;
;; Basis States assigned to their bra-ket symbols
;;
(a |0> (vtr [1j0 0]) |1> (vtr [0 1j0])) ; bootstrap standard basis

(defun qcs () "A quantum computing simulator"
       (let* ((#IO 1)
	      (basis '(lambda (N) (mapcar 'car (se `(aref BAL ,N) braket)))) ; get basis out of braket closure
	      (IsUnitary '(lambda (G) ;; All gate matrices must be unitary (only constraint)
			    (let ((N (tally G)))
			      (onep (r '^ (r '^ (= (. '+ '* (conj (tr G)) G) (Id N))))))))
	      ;; Helper functions
	      (EQL (dfd (onep (r '^ (r '^ (= a w)))))) ; lisp matrix equality
	      (mmm '(macro (L)  `(let ((R ,(car L)) (L (list ,@(cdr L))))
				   (while L (a R (. '+ '* R (car L)) L (cdr L))) R)))
	      (dotp (dfd (. '+ '* (tr (conj q)) w))) ;; dot product           <a||w>
	      (dyad (dfd (o '* (rav a) (rav w))))    ;; outer product         |a><w|
	      (tp (dfd (vtr (rav (o '* a w)))))      ;; vector tensor product |a>|w>
	      (tpn (dfd (let ((R [1])) (FOR I 1 w (a R (tp R a))) R))) ; vector tensor exponent
	      (tpm (dfd (p (* (p a) (p w)) (tr (+ [0 2 1 3] #IO) (o '* a w))))) ;; matrix tensor product
	      (tpmm '(macro (L)  ; matrix tensor product variable number of arguments
			    `(let ((R ,(car L)) (L (list ,@(cdr L))))
			       (while L (a R (tpm R (car L)) L (cdr L))) R)))
	      (tpmn (dfd (let ((R (vtr [1]))) (FOR I 1 w (a R (tpm R a))) R))) ; matrix tensor exponent
	      (Id ($(p {w w} {1j0 (p w 0)})))  ; Generate an NxN identity matrix
	      (I (Id 2)) 	;; Identity
	      ;; Pauli Operators
	      (X (p [2 2] [0 1 1 0]))
	      (Y (p [2 2] [0 0j-1 0j1 0]))
	      (Z (p [2 2] [1 0 0 -1]))
		;; Superposition Operators
	      (H (p [2 2] (/ [1 1 1 -1] (sqrt 2j0)))) ; Hadamard gate
	      (S (p [2 2] [1 0 0 0j1])) ; Phase gate
	      (S+ (conj (tr S))) ; Transposed conjugate Phase gate
	      ;; Conditional Gate
	      (CNOT (rot 2 [0 0 -1 1] (Id 4))) ;; Controlled not
	      (SWAP (rot 2 [0 -1 1 0] (Id 4))) ;; Swap
	      (CZ (p [4 4] {(p 15 [1 0 0 0 0]) -1})) ;; Controlled Z
	      ;; Non-Clifford Gates
	      (T  (p [2 2] {1 0 0 (exp (/ (% 0j1) 4))}))
	      (T+ (conj (tr T)))
	      (meas (dfm (sqr (| w)))) ; measure whole register
	      ;; Measurement in standard basis
	      (measQ '(lambda (Q R) "measure qubit Q with index from 0 in register R"
			(let* ((#IO 0) (S (l 2 (tally R))) (C (vtr [1 1]))
			       (Pre (tpn C Q)) (Post (tpn C (- S Q 1)))
			       P0 P1) ; sum of prob0/1 basis vectors
			  (if (ge Q S) (error "invalid QuBit index"))
			  (a P0 (tp (tp Pre |0>) Post) P1 (tp (tp Pre |1>) Post))
			  {(r '+ 1 (meas (* P0 R))) (r '+ 1 (meas (* P1 R)))})))
	      (measV '(lambda (V M) "Measure V with measurement operator M"
			(dotp (. '+ '* M V) (. '+ '* M V))))
	      (MM '(lambda (m n) "Make measurement operator with index m on n qubit reg"
		     (let* ((#IO 0) (N (exp 2 n)) (M (p {N N} 0j0))) (iset M 1j0 m) M)))
	      (TMV '(lambda (N) "Test all possible measurements on an N qubit reg"
		      (let* ((#IO 0) (L (exp 2 N)) (L1 (- L 1)) (B (basis N))
			     (M (mapcar '(lambda (X) (MM X N)) (explod (i L)))))
			(FOR I 0 L1
			     (FOR J 0 L1
				  (if (neql (= I J) (measV (nth I B) (nth J M))) (error "measV failed"))))
			t)))
	      (qsb '(lambda (qr n) "subset of first/last n qubits in qr"
		      )) ;; not feasible with entangled state in qr
	      ;; helpful fairies
	      (|0><0| (dyad |0> |0>))
	      (|1><1| (dyad |1> |1>))
	      (|1><0| (dyad |1> |0>))
	      (|0><1| (dyad |0> |1>))
	      (P0 |0><0|) (P1 |1><1|) 	   ;; Projectors
	      (L0 |0><1|) (L1 |1><0|)	   ;; Ladder operators
	      (CNOTR (mmm SWAP CNOT SWAP)) ;; Controlled not (target,control)
	      (CNOT12 (tpm I CNOT))
	      (CNOT02 (mmm (tpm SWAP I) (tpm I CNOT) (tpm SWAP I)))
	      SWAPFL SWAPXY CNOTOX TOFFOX PHASE CPHASEX G
	      (SWAPFL '(lambda (N) "Swap first and last bits in N bit qreg"
			 ;; Ref Quantum circuit optimization for unitary operators over non-adjacent qudits
			 (if (onep N) I
			   (let ((ID (Id (exp 2 (- N 2)))))
			     (cat 1 (cat (tpm ID P0) (tpm ID L1))
				  (cat (tpm ID L0) (tpm ID P1)))))))
	      (SWAPXY '(lambda (X Y N) "Swap bits X and Y in an N bit qreg"
			 (if (or (ge X N) (ge Y N)) (error "swap bits out of range"))
			 (if (gt X Y) (let ((T X)) (a X Y Y T))) ;; swap indexes
			 (tpmm (tpmn I X) (SWAPFL (- Y X -1)) (tpmn I (- N Y 1)))))
	      (CNOTOX '(lambda (O A N) "Create CNOT with control bit O affecting A in N qbit reg"
			 (if (ge O A) (error "CNOTOX: O must be less than A"))
			 (+ (tpmm (tpmn I O) (dyad |0> |0>) (tpmn I (- N O 1)))
			    (tpmm (tpmn I O) (dyad |1> |1>) (tpmn I (- A O 1)) X (tpmn I (- N A 1))))))
	      (TOFFOX '(lambda (O1 O2 A N) "Create Toffoli with control bit O1 O2 affecting A in N qbit reg"
			 (if (or (ge O1 O2) (ge O2 A)) (error "TOFFOX: O1 < O2 < A"))
			 (+ (tpmm (tpmn I O1) |0><0| (tpmn I (- N O1 1)))
			    (tpmm (tpmn I O1) |1><1| (tpmn I (- O2 O1 1)) |0><0| (tpmn I (- N O2 1)))
			    (tpmm (tpmn I O1) |1><1| (tpmn I (- O2 O1 1)) |1><1| (tpmn I (- A O2 1))
				  X (tpmn I (- N A 1))))))
	      (PHASE '(lambda (k) (p [2 2] {1 0 0 (exp (/ (% 0j2) (exp 2 k)))})))
	      (CPHASEX '(lambda (O A N X) "Create controlled phase with control bit O affecting A in N bit qreg"
			  (if (eql O A) (error "CPHASEX: O cannot be equal to A"))
			  (if (or (ge A N) (ge O N)) (error "bit arguments out of range"))
			  (if (gt O A)
			      (+ (tpmm (tpmn I O) |0><0| (tpmn I (- N O 1)))
				 (tpmm (tpmn I A) (PHASE X) (tpmn I (- O A 1)) |1><1| (tpmn I (- N O 1))))
			    (+ (tpmm (tpmn I O) |0><0| (tpmn I (- N O 1)))
			       (tpmm (tpmn I O) |1><1| (tpmn I (- A O 1)) (PHASE X) (tpmn I (- N A 1)))))))
	      (G '(lambda (O X) ; apply sequence of gates O to state X
		    (cond ((null O) X) ((atom O) (. '+ '* O X))	(t (G (cdr O) (. '+ '* (car O) X))))))
	      (qbr '(lambda (L)
		      "Generate an N qubit reg with list of coefficients (qbr ([a1 b1] [a2 b2] ... [aN bN]))"
		      (mapcar '(lambda (X)
				 (let (alpha beta gamma)
				   (cond ((lt (p X) 2) (a alpha (aref (rav X) 1) beta 0j0))
					 (t (a alpha (aref X 1) beta (aref X 2))))
				   (a gamma {alpha beta})
				   (unless (onep (r '+ (sqr (| gamma)))) ; normalisation condition
				     (print gamma)
				     (error "sum of the squares of the absolute values is not equal to 1"))
				   (+ (* alpha |0>) (* beta |1>))))
			      L))))

	      (fun G)
	      ))

(a QCS (qcs))
(de QC (X) (se X QCS))

(defun mkbra (N) ;; Construct braket symbols |xxx>
  (let* ((L1 '(|0> |1>)) (L L1) (B (list (pairlis (list |0> |1>) L1 nil))) BAL H
	 (tp (dfd (vtr (rav (o '* a w)))))      ;; vector tensor product |a>|w>
	 (mkb '(lambda (B1 B2) ; make basis assoc list from basis symbol lists
		 (let (X Y P S)
		   (mapcar '(lambda (Z)
			      (a X (car Z) Y (cadr Z)
				 P (* 1j0 (tp (eval X) (eval Y))) ;;  complex for compare
				 S (intern {(dp -1 X) (dp 1 Y) }));;  braket symbol
			      (set S P) ; set braket symbol value
			      (cons P S)) ; assoc list for reverse lookup
			   (COMB B1 B2)))))
	 (braket '(lambda (X) ; get braket symbol for basis vector
		    (cdr (assoc (* 1j0 X) (aref BAL (l 2 (aref (p X) 1))))))))
    (FOR I 2 N
	 (a H (mkb L L1) L (mapcar 'cdr H))
	 (nconc B (list H)))
    (a BAL (implod nil t B))
    (fun braket)))
(a braket (mkbra 9)) ; braket closure

;; N&C measurement
;; (a Q (car (QC '(qbr (list (o '% [1 2] (% (/ 6)))))))
;; (QC `(dotp ,Q (mmm (MM 0 1) ,Q))) ;=> [.25]   (2.96) a=sin(30)
;; (QC `(dotp ,Q (mmm (MM 1 1) ,Q))) ;=> [.75]          b=cos(30)
;; (QC `(/(mmm (MM 0 1) ,Q) (p 1 (sqrt(dotp ,Q (mmm (MM 0 1) ,Q)))))) ;=> (vtr [1 0]) (2.93) m=0
;; (QC `(/(mmm (MM 1 1) ,Q) (p 1 (sqrt(dotp ,Q (mmm (MM 1 1) ,Q)))))) ;=> (vtr [0 1])        m=1
;; (QC '(eql (Id 16) (MM (i [0 15]) 4))) ;=> t  (2.94)

;; Truth table for CNOT
;; (QC '(mapcar '(lambda (X) (braket (G CNOT X))) (basis 2)))  ;=> (|00> |01> |11> |10>)

;; (QC '(braket (meas (G (list  (tpm I I) CNOT) |00>)))) ;=> |00>
;; (QC '(braket (meas (G (list  (tpm I       X) CNOT) |00>)))) ;=> |01>
;; (QC '(braket (meas (G (list  (tpm X           X) CNOT) |00>)))) ;=> |10>
;; (QC '(braket (meas (G (list  (tpm X      I) CNOT) |00>)))) ;=> |11>
;; (QC '(meas (G (list (tpmn H 5) (tpm (tpm I CNOT) (Id 4))) |00000>))) ;=> (p [32 1] .03125)
;; (QC '(meas (G (list H T H T H T S H T H T S H) |0>))) ;=> (vtr  [.89 .11])
;; Bell state ZZ measurement
;; (QC '(meas (G (list (tpm H I) CNOT) |00>))) ;=> (vtr [ .5 0.0 0.0  .5])
;; Bell state ZW measurement
;; (QC '(meas (G (list (tpm H I) CNOT (tpm I S) (tpm I H) (tpm I T ) (tpm I H)) |00>))) ;=> (vtr [.4267766953 .0732233047 .0732233047 .4267766953])
;; Bell state ZV measurement
;; (QC '(meas (G (list (tpm H I) CNOT (tpm I S) (tpm I H) (tpm I T+) (tpm I H)) |00>))) ;=> (vtr [.4267766953 .0732233047 .0732233047 .4267766953])
;; Bell state XW measurement
;; (QC '(meas (G (list (tpm H I) CNOT (tpm   H S) (tpm I H) (tpm I T ) (tpm I H)) |00>))) ;=> (vtr [.4267766953 .0732233047 .0732233047 .4267766953])
;; Bell state XV measurement
;; (QC '(meas (G (list (tpm H I) CNOT (tpm   H S) (tpm I H) (tpm I T+) (tpm I H)) |00>))) ;=> (vtr [.0732233047 .4267766953 .4267766953 .0732233047])
;; GHZ State
;; (QC '(meas (G (list (tpmm H H X) CNOT12 CNOT02) |000>))) ;=> [0 .25 .25 0 .25 0 0 .25]
;; GHZ State YYX Measurement 
;; (QC '(meas (G (list (tpmm H H X) CNOT12 CNOT02 (tpmn H 3) (tpmm S+ S+ H) (tpmm H H I)) |000>))) ;=> (vtr [ .25 0 0 .25 0 .25 .25 0])
;; GHZ State YXY Measurement 
;; (QC '(meas (G (list (tpmm H H X) CNOT12 CNOT02 (tpmn H 3) (tpmm S+ H S+) (tpmm H I H)) |000>))) ;=> (vtr [ .25 0 0 .25 0 .25 .25 0])
;; GHZ State XYY Measurement 
;; (QC '(meas (G (list (tpmm H H X) CNOT12 CNOT02 (tpmn H 3) (tpmm H S+ S+) (tpmm I H H)) |000>))) ;=> (vtr [ .25 0 0 .25 0 .25 .25 0])
;; GHZ State XXX Measurement 
;; (QC '(meas (G (list (tpmm H H X) CNOT12 CNOT02 (tpmn H 3) (tpmn H 3)) |000>))) ;=> (vtr [0 .25 .25 0 .25 0 0 .25])
;; CNOT (reverse)
;; (QC '(meas (G (list (tpm I X) (tpm H H) CNOT (tpm H H)) |00>))) ;=> (vtr [0 0 0 1]))
;; swap
;; (QC '(meas (G (list (tpm I X) CNOT (tpm H H) CNOT (tpm H H) CNOT) |00>))) ;=> (vtr [0 0 1 0])
;; Using our swap operator
;; (QC '(meas (G (list (tpm I X) SWAP) |00>))) ;=> (vtr [0 0 1 0])
;; (QC '(EQL SWAP (mmm CNOT CNOTR CNOT))) ;=> t
;; (QC '(EQL CNOTR (mmm SWAP CNOT SWAP))) ;=> t
;;
;; Swap Q0 and Q1
;; (QC '(meas (G (list (tpmm H I I) CNOT02 (tpmm H I H) CNOT02 (tpmm H I H) CNOT02 CNOT12 (tpmm I H H) CNOT12 (tpmm I H H) CNOT12 CNOT02 (tpmm H I H) CNOT02 (tpmm H I H) CNOT02) |000>))) ;=> (vtr [.5 0 .5 0 0 0 0 0])
;; Controlled Hadamard
;; (QC '(meas (G (list (tpm H I) (tpm S H) (tpm I S+) CNOT (tpm I H) (tpm I T) CNOT (tpm I T) (tpm I H) (tpm I S) (tpm I X)) |00>))) ;=> (vtr [.5 0 .25 .25])
;; approximate sqrt(T)
;; (QC '(meas (G (list (tpmn H 5) (tpmm I H T S Z) (tpmm I (mmm T H T H T S H T H T S H T H T H) I I I)) |00000>))) ;=>
;;(vtr  [.032 .032 .032 .032 .032 .032 .032 .032 \
;;      .031 .031 .031 .031 .031 .031 .031 .031 \
;;      .032 .032 .032 .032 .032 .032 .032 .032 \
;;      .031 .031 .031 .031 .031 .031 .031 .031])
;; Toffoli state
;; (QC '(meas (G (list (tpmn H 3) (tpm I CNOT) (tpmm I I T+) CNOT02 (tpmm I I T) (tpm I CNOT) (tpmm I I T+) CNOT02 (tpmm I T T) (tpm I CNOT) (tpmm I H H)(tpm I CNOT) (tpmm I H H) (tpm I CNOT) CNOT02 (tpmm T H T+) CNOT02) |000>))) ;=> (vtr [.25 .25 0 0 .25 0 0 .25])
;; Toffoli with flips
;; (QC '(meas (G (list (tpmm X X I) (tpmm I I H)  (tpm I CNOT) (tpmm I I T+) CNOT02 (tpmm I I T) (tpm I CNOT) (tpmm I I T+) CNOT02 (tpmm I T T) (tpmm I I H) (tpm I CNOT) (tpmm I H H)(tpm I CNOT) (tpmm I H H) (tpm I CNOT) CNOT02 (tpmm T I T+) CNOT02) |000>))) ;=> (vtr [0 0 0 0 0 0 0 1])
;;
;; Grover N2 A00
;; the sequences ... (tpm I H) CNOT (tpm I H) ... are replaced by CZ, controlled Z
;; (braket (QC '(meas (G (list (tpm H H) (tpm S S) CZ (tpm S S) (tpm H H) (tpm X X) CZ (tpm X X) (tpm H H)) |00>)))) ;=> |00>
;; Grover N2 A01
;; (braket (QC '(meas (G (list (tpm H H) (tpm I S) CZ (tpm I S) (tpm H H) (tpm X X) CZ (tpm X X) (tpm H H)) |00>)))) ;=> |01>
;; Grover N2 A10
;; (braket (QC '(meas (G (list (tpm H H) (tpm S I) CZ (tpm S I) (tpm H H) (tpm X X) CZ (tpm X X) (tpm H H)) |00>)))) ;=> |10>
;; Grover N2 A11
;; (braket (QC '(meas (G (list (tpm H H)  CZ  (tpm H H) (tpm X X) CZ (tpm X X) (tpm H H)) |00>)))) ;=> |11>
;;
;; Deutsch-Josa N=3
;; (QC '(meas (G (list (tpmn H 3) (tpm Z CZ) (tpmn H 3)) |000>))) ;=> (vtr [0 0 0 0 .25 .25 .25 .25])
;; DJ N=3 Constant
;; (QC '(meas (G (list (tpmn H 3) (tpmn H 3)) |000>))) => (vtr [1 0 0 0 0 0 0 0])
;;
;; Phase estimation -
;; (QC '(measQ 1 (G (list (tpm H H) (tpm Z I) CNOTR (tpm I H)) |00>))) ;=> [0 1])
;; Phase estimation +
;; (QC '(measQ 1 (G (list (tpm H I) (tpm I H) CNOTR (tpm I H)) |00>))) ;=> [1 0])

;; N&G
;; 4.17
;; (QC '(EQL CNOT (mmm (tpm I H) CZ (tpm I H))))
;; me
;; (QC '(EQL CZ (mmm (tpm I H) CNOT (tpm I H))))
;; (QC '(EQL CNOT (CNOTOX 0 1 2)))
;; (QC '(EQL CNOT02 (CNOTOX 0 2 3)))
;; (QC '(EQL (tpmm I I I CNOT I)  (CNOTOX 3 4 6)))
;; (QC '(EQL (tpmm I I I CNOT02 I)  (CNOTOX 3 5 7)))
;;
;; Rieffel & Polak
;; (QC '(EQL CNOT (+ (tpm (dyad |0> |0>) I) (tpm (dyad |1> |1>) X))))
(defun adder (V) "|incoming carry, X input, Y input, sum(X,Y) mod 2, outgoing carry>" 
   (QC `(braket (G (list (TOFFOX 1 2 4 5) (TOFFOX 0 1 4 5) (TOFFOX 0 2 4 5) 
                     (CNOTOX 0 3 5)   (CNOTOX 1 3 5)   (CNOTOX 2 3 5)) ,V))))
;; (adder |00000>) ;=> |00000>
;; (adder |01000>) ;=> |01010>
;; (adder |00100>) ;=> |00110>
;; (adder |01100>) ;=> |01101>
;; (adder |10000>) ;=> |10010>
;; (adder |11000>) ;=> |11001>
;; (adder |10100>) ;=> |10101>
;; (adder |11100>) ;=> |11111>


;;
;; Quantum Fourier transform
;;
;; Ref: Mike and Ike

(defun QFT3 ()
  (mmm (tpm H (tpmn I 2))
       (CPHASEX 1 0 3 2)
       (CPHASEX 2 0 3 3)
       (tpmm I H I)
       (CPHASEX 2 1 3 2)
       (tpmm I I H)
       (SWAPFL 3)
       )
  )

(defun QSWAP (N)
  (let* ((#IO 0) (NH (f (/ N 2))) (NL (explod (i NH))))
    (mapcar '(lambda (X) (list 'SWAPXY X (- N X 1) N)) NL)))

(defun QTT (N)
  (let ((#IO 0))
   (append (mapcan '(lambda (X) (cons `(tpmm (tpmn I ,X) H (tpmn I ,(- N X 1)))
			       (mapcar '(lambda (Y) `(R ,X ,(+ Y X -1) ,Y))
				       (explod (+ 2 (i (- N X 1)))))))
		   (explod (i N)))
	   (QSWAP N))))

(defun QFT (N)
  (let ((R '(lambda (O A P) (CPHASEX O A N P))))
    (apply mmm (QTT N))))

(defun DQFT (N)  "Generate explicit QFT matrix"
       (let* ((#IO 0) (C (exp 2 N)) (IC (i C)))
	 (* (/ (sqrt C)) (exp (exp (/ (% 0j2) C)) (| C (o '* IC IC))))))

(defun PDQFT (N P)  "Generate explicit QFT matrix with phase error P%"
       (let* ((#IO 0) (C (exp 2 N)) (IC (i C)))
	 (* (/ (sqrt C)) (exp (Perturb (p {C C} (exp (/ (% 0j2) C))) P)
			      (| C (o '* IC IC))))))

(defun TQFT (N) (if (eql (| (QFT N)) (| (DQFT N))) 'OK 'Fail))

;; (eql (| (G (list (DQFT 3)) |110>)) (| (vtr (*  (/ (sqrt 8)) (w (rav |110>))))))

(defun PM (N P)  "Generate matrix of dim NxN with pertubations within +-P%"
       (let ((#IO 0))
	 (+ 1 (* (- (* 2 (? (p {N N} 0))) 1) (/ P 100)))))

(defun Perturb (X P) "Perturb imaginary part of X by +-P%"
  (let ((SP (p X)) (N (tally X)))
    (. '+ '% [-9 -11] (* (cat .5 (p SP 1) (PM N P)) (o '% [9 11] X)))))

(defun TU (G Epsilon) "Unitariness test of G within Epsilon"
  (> (|(. '+ '* (conj (tr G)) G)) Epsilon))

(defun MKV (n v) "Make state vector for v on n qbit reg"
       (let* ((#IO 0) (R (p (exp 2 n) 0j0))) (aset R 1j0 v) R))

(defun QCL (V) ;; length of cycle in V cycle must start at first element
  (- (+ 1 (ind (dp 1 V) (tk 1 V))) #IO))

(defun QFAC (N)
  (let ((N2 0) (NS 1) F)
    (while (evenp N) (a N (/ N 2) N2 (+ N2 1))) ;; reduce N to odd
    (cond ((onep N) (princl (fmt N2 "th power of 2")) (a F 2))
	  ((onep (isPrime N)) (princl (fmt N " is prime"
				    (if (zerop N2) "" (fmt " * (exp 2 " N2 ")"))))
			       (a F N))
	  (t (while (null (a F (SHOR N))) (incr NS))
	     (princl (fmt "Found result after " NS " iterations of SHOR"))))
    F))

(defun TSHOR (N) "Test Shor on products of primes less than N"
  (let* ((P (explod (Primes N)))
	 (Q (sort 'lt (mapcar '(lambda (x) (apply '* x)) (UCOMB P P))))
	 (selct '(lambda (x y) (mapcan '(lambda (x) (if (a y (not y)) (list x)))x)))
	 (evens '(lambda (x)   (selct x t))))
    (mapcar QFAC (print (evens Q)))))

;; (a #WD 0)
;; (QC '(TSHOR 10))

(defun PSHOR (N PertPerc) "Shor with +-PertPerc% perturbation on rotations"
       (let ((DQFT '(lambda (N) (PDQFT N PertPerc))))
	 (SHOR N)))

;;; 
;;; Refs: [1] Nielsen&Chuang ed 2010
;;;       [2] Shor’s Algorithm for Factoring Large Integers Lavor (et al) 
;;;           arXiv:quant-ph/0303175v1
(require 'graf)
(defun SHOR (N) "First attempt at Shor's algorithm on odd N"
  (prog* ((#IO 0)
	  (n (c (l 2 N)))          ;; n bits for R2
	  (T (c (l 2 (sqr N))))    ;; Skimping with T bits for R1
	  (T2 (exp 2 T))           ;; number of entries in R1 state vector
	  (R1 (p {T2 1} 0))        ;; initialise R1 to 0 over T qubits
	  (x (+ 1 (? (- N 2))))    ;; guess an x where 1 < x < N 
	  (F1 (v x N))             ;; first factor candidate is gcd x N
	  R2 VR2 MR2 F2 V1 V2 Y S R MX RR P
	  (NR 0))                  ;; normalisation count
    (princl (fmt "(SHOR " N ") -> R1 has " T " bits, R2 has " n "; chose x=" x))
    (unless (onep F1) (return F1)) ;; F1 works, no stinkin quannum needed here
    ;;(a R1 (G (list (tpmn H T)) R1))  ;; normally we would apply hadamards on R1
    ;; But we are cheating by "measuring" R2 before taking product so we can
    ;; collapse the state to R1 only to avoid T+n bit product matrices
    (a R2 (modexp 2 (i (exp 2 n)) N)) ;; state vector of R2
    ;; (print (a VR2 (tk (QCL R2) R2))) ;; VR2 unique values in R2 
    (a R2 (aref R2 (? (p R2))))   ;; "measure" reg2 pick random value in cycle
    (princl (fmt "Measured R2 -> " R2))
    (FOR I 0 (- T2 1)
	 ;; (a V1 (MKV T I) V2 (MKV n (modexp 2 I N)))
	 ;; (print (list I (braket V1) (braket V2)))
	 (when (eql (modexp 2 I N) R2) ;; collect terms corresponding to measured R2
	   (a NR (+ NR 1) R1 (+ R1 (vtr (MKV T I)))))) ;; From (21) and (22) of [2]
    (a R1 (/ R1 (sqrt NR)))              ;; normalise
    (a R1 (G (list (conj (DQFT T))) R1)) ;; apply inverse fft to top T bits aka R1
    (a RR (rav (meas R1)))               ;; measure R1
    (plot RR)
    (a MX (r 'c (dp 1 RR))) ;; find max peak excluding at 0
    (a P (dp 1 (k (> RR (/ MX 2)) (i T2))))  ;; peaks at P dropping first peak
    (print (list 'peaks P 'Max MX))
    (a S (p [] (aref P (? (p P)))))          ;; pick a peak
    (print (list 'picked 'peak S))
    (print (list (list S T2) (fc (convg (cf S T2)))))
    ;; Find period R in first convergent in the continued fraction S/T2 less than N
    (a R (ngenc '(lambda (X) (if (and (neql (car X) 1) (lt (car X) N)) (car X)))
		     (fc(convg (cf S T2))))) ;; See [1] Theorem 5.1
    (print (list 'R R))
    (if (or (null R) (oddp R)) (return nil)) ;; Try again
    (a Y (modexp 2 (/ R 2) N))               ;; See [2] page 10
    (cond ((and (gt Y 1) (lt Y (- N 1)))
	   (print (list (a F1 (gcd (- Y 1) N)) (a F2 (gcd (+ Y 1) N))))
	   (cond ((zerop (elt F1 {1 N})) F1)
		 ((zerop (elt F2 {1 N})) F2))))
    ))
   
;;; From Quantum Error Correction for Beginners; Devitt, Munro et al, 2013
;;; https://arxiv.org/abs/0905.2794

(de Fig2 (x error)
    (G (list (tpmm H I I)    ;; tensor product multiple matrices
	     (CNOTOX 0 1 3)  ;; CNOT on qubit 0 acting on qbuit 1 over 3 qubits
	     error           ;; error operator argument (tpmn I 3) for no error
	     (CNOTOX 0 2 3)) ;; CNOT on qubit 0 acting on qbuit 2 over 3 qubits
       x))

;;; Basis state |Phi> = |0> no error 
;; (Sbrak (QC '(Fig2 |000> (tpmn I 3))))   ;=> .7071067812*(|000> + |111>)
;;; Basis state |Phi> = |1> no error 
;; (Sbrak (QC '(Fig2 |100> (tpmn I 3))))   ;=> .7071067812*(|000> - |111>)
;;; Basis state |Phi> = |0>  with bit flip on Phi
;; (Sbrak (QC '(Fig2 |000> (tpmm X I I)))) ;=> .7071067812*(|010> + |101>)
;;; Basis state |Phi> = |1>  with bit flip on Phi
;; (Sbrak (QC '(Fig2 |100> (tpmm X I I)))) ;=> .7071067812*(|010> + |101>)
;;; Basis state |Phi> = |0>  with bit flip on first ancilla qubit
;; (Sbrak (QC '(Fig2 |000> (tpmm I X I)))) ;=> .7071067812*(|010> + |101>)
;;; Basis state |Phi> = |0>  with bit flip on second ancilla qubit 
;; (Sbrak (QC '(Fig2 |000> (tpmm I I X)))) ;=> .7071067812*(|001> + |110>) 


(de Fig3 (x error)
    (G (list (CNOTOX 0 1 5)
	     (CNOTOX 1 2 5)
	     error
	     (CNOTOX 0 3 5)
	     (CNOTOX 1 3 5)
	     (CNOTOX 1 4 5)
	     (CNOTOX 2 4 5))
       x))

;;; Basis state  |Phi> = |0>
;; (Sbrak (QC '(Fig3 |00000> (tpmn I 5))))        ;=> |00000>
;;; Basis state |Phi> = |1> no error 
;; (Sbrak (QC '(Fig3 |10000> (tpmn I 5))))        ;=> |11100> 
;;; Basis state  |Phi> = |0> bit flip on qubit 1
;; (Sbrak (QC '(Fig3 |00000> (tpmm X I I I I))))  ;=> |10010> 
;;; Basis state  |Phi> = |0> bit flip on qubit 2
;; (Sbrak (QC '(Fig3 |00000> (tpmm I X I I I))))  ;=> |01011> 
;;; Basis state  |Phi> = |0> bit flip on qubit 3
;; (Sbrak (QC '(Fig3 |00000> (tpmm I I X I I))))  ;=> |00101> 

(de Fig4 (x error) ;;Circuit to encode a qubit with Shor's 9 qubit code.
    (G (list (CNOTOX 0 3 9)
	     (CNOTOX 0 6 9)
	     error
	     (tpmm H I I H I I H I I)
	     (tpmm CNOT I CNOT I CNOT I)
	     (tpmn (CNOTOX 0 2 3) 3)) ;; tensor product of matrix 3 times
       x))

;;; Basis state  |Phi> = |0>
;; (Sbrak (QC '(Fig4 |000000000> (tpmn I 9)))) ;=>
;;.3535533906*(|000000000> + |000000111> + |000111000> + |000111111> +
;;             |111000000> + |111000111> + |111111000> + |111111111>)
;;; Basis state  |Phi> = |1>
;; (Sbrak (QC '(Fig4 |100000000> (tpmn I 9)))) ;=>
;;.3535533906*(|000000000> - |000000111> - |000111000> + |000111111> -
;;             |111000000> + |111000111> + |111111000> - |111111111>) 
;;; Basis state  |Phi> = |0> bit flip on qubit 1
;; (Sbrak (QC '(Fig4 |000000000> (tpmm X (tpmn I 8)))))
;;.3535533906*(|000000000> + |000000111> + |000111000> + |000111111> -
;;             |111000000> - |111000111> - |111111000> - |111111111>) 

(de Sbrak (v) ; Smarter reverse braket 
    (let* ((#IO 1)(v (rav v)) (S (* (re v))) ; signs of v
	   (V (* S v)) ;; change -'s to +
	   (Vals (RemDup (k (<> 0j0 V) V))) (IDM (Id (p V)))
	   RR BB PP)
      (FOR I 1 (p Vals)
	   (a RR (aref Vals I) PP (Posns RR V))
	   (if (and (gt (re RR) 0)(gt I 1)) (princ "+"))
	   (unless (eq RR 1j0) (princ RR) (princ "*"))
	   (a BB (mapcar braket (explod 2 (aref IDM () PP))))
	   (if (eq (len BB) 1) (prin0 (car BB))
	     (prin0 (mapn '(lambda (X S) 
			     (cond ((null (cdr X)) X)
				   (t (list (car X)
					    (if (onep (cadr S)) '+ '-)))))
			  BB (explod (aref S PP))))))
      (terpri)
      ))


(de CKME (x) (gt 1e-10 (| (- (K x)  (eval (I2P (TM (K x))))))))


(de TM (R) (let* ((B (buf (p 1024 " "))) (#PF B)) (princ "(") (Sbrak R) (princ ")")  (read B)))


(de F4p (X) (QC `(G (list (tpmm H I I) (CNOTOX 0 1 3) (CNOTOX 0 2 3)) ,X)))

(de TSWP (N)
    (let (S B R (N1 (- N 1)))
    (FOR II 0 N1
	 (FOR J 0 N1
	      (a B (nth (exp 2 (- N1 II)) (basis N)) ;; input
		 S (G (list (SWAPXY II J N)) B)      ;; output
		 R (nth (exp 2 (- N1 J)) (basis N))  ;; expected result
		 )
	      (prin0 (list 'SWAPXY II J N (braket B)))
	      (princl (fmt "=> " (braket S) "  "
			   (aref (p [2 3] "BadOK ") (+ #IO (r '^ (rav (= R S)))) ())))))))
;; (QC '(TSWP 5))
