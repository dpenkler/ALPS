;;;-*- mode: emacs-lisp -*-
(require 'idioms)

(defun Hex2N (S) "Convert Hex string to singleton num"
  (let ((#IO 0)) (dec 16 (ind #hexdigs S))))

(defun N2Hex (N) "Convert singleton num to hex string"
  (let ((#IO 0)) (aref #hexdigs (enc (p (c (l 16 N)) 16) N))))

(defun S2Hex (S) "convert ASCII string to hex string"
  (let ((#IO 0)) (rav (aref #hexdigs (tr (enc [16 16] (num S)))))))

(defun Hex2S (H) "Convert Hex string to ASCII"
       (chr (dec 16 (tr (p {(/ (p H) 2) 2} (dechex H))))))

(defun ascp (H) "Convert Hex string to printable ASCII"
       (RepElem '(lambda (X) (~ (elt X #a))) (hex "80") (asc H)))

(defun dechex (S) ; convert string of hex digits to vector of real decimal digs
 (let ((#IO 0)) (ind #hexdigs S)))

(defun dechexb (S) ; convert string of pairs of hex digits to vector of numss
  (let ((#IO 0))
    (. '+  '*  (p {(/ (p S) 2) 2} (- (ind #hexdigs S) #IO)) [16 1])))

(defun hexdec (S) ; convert array of real decimal to hex string
 (let ((#IO 0)) (aref #hexdigs S)))

(defun peek (A L) ; peek at starting at address A for L octets
  (p {L 8} (tr (S2Hex (mget (addr A) L))))) ; byte for byte

(defun sv (A) ; word for word
  (if (not(atom A)) (error "sv argument not atomic"))
  (let* ((RA (- (addr A) 8)) ; step back to bsiz field of alc'd block
	 (RL  (peep RA))     ; get length
	 RR RS 
	 (base (p 8 16))     ; base of 8 nibbles
	 (RR (rav (enc base RL))) ; encode into nibbles
	 (I 0))
    (if (ge (aref RR 1) 8) ; sign bit is set i.e. size is negative 
	(a RL (+ 1 (dec base (| (- RR 15))))) ; compute two's complement
      (error "Weird variable"))
    (princ (fmt "Show Variable @0x" RA:8::16 " length " RL:0::10"\n"))
    (a RL (/ RL 4))
    (while (lt I RL)
      (princ 
       (fmt I:3::10 (peep (+ (* I 4) RA) (a L (f 8 (- RL I)))):9::16 "\n"))
      (a I (+ I L)))
    )
)

(de sv64 (A) "show variable 64 bit"
 (let* ((#PW 36) (#IO 0) (REV (if (zerop (num (aref (mget (addr nil) 8) 6))) 'rev 'arg)) ;; little/big endian
       (Z (num (mget (addr A) (- (+ 1 (dec 2 (~(rav(tr(enc (p 8 2)(REV (num (mget (- (addr A) 16) 8))))))))) 24)))))
   (princl (fmt "Addr 0x" (addr A):-16::16))
   (FOR I 0 (- (/ (p Z) 8) 1) (prhex (aref Z (+ (* I 8) (i 8)))))))

(setq C "10039000A6CF27A9A50000000000004E45440680")
(setq D "101EF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
(setq E "101F0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")

(defun Bin (C) (let ((#IO 0)) (ind #hexdigs C)))

(defun HD (H) (let* ((A (Bin H)) (B (p H)) (C (p B 16))) (dec C A)))

(defun HT (N) (FOR I 1 N (print (HD (cat "1" (p I "0"))))))

(defun THEX (N) ; test conversion to hex and back of first N integers
  (let ((A (buf "hex")) (Base (p 8 16)) (#IO 0)) 
    (FOR I 0 N  
	 (buf (cat "0x" (aref #hexdigs (enc Base I))) A) ; encode to hex
	 (cond ((eq I (read A))) ((error "hex fails")))  ; read back and check
	 (cond ((zerop (| 8192 I)) (print I))))))      ; show progress

(defun HDB (V) ; show string of hex in bytes of hex dec <char> bin
  (prog ((D (. '+  '*  (p {(/ (p V) 2) 2} (- (ind #hexdigs V) #IO)) [16 1])))
	(mapc '(lambda (A)
		 (princl (fmt A:3::16 A:4 " <"
			      (aref {" " (chr A)} (+ 1 (elt (chr A) #p))) "> "
			      A:-8::2))) (explod D))
	(princ "Checksum: ")
	(print (- 256 (| 256 (r '+ D)))))) ; return Checksum

;; (HDB (S2Hex "hello this is the end my friend"))
;;  68 104 <h> 01101000
;;  65 101 <e> 01100101
;;  6C 108 <l> 01101100
;;  6C 108 <l> 01101100
;;  6F 111 <o> 01101111
;;  20  32 < > 00100000
;;  74 116 <t> 01110100
;;  68 104 <h> 01101000
;;  69 105 <i> 01101001
;;  73 115 <s> 01110011
;;  20  32 < > 00100000
;;  69 105 <i> 01101001
;;  73 115 <s> 01110011
;;  20  32 < > 00100000
;;  74 116 <t> 01110100
;;  68 104 <h> 01101000
;;  65 101 <e> 01100101
;;  20  32 < > 00100000
;;  65 101 <e> 01100101
;;  6E 110 <n> 01101110
;;  64 100 <d> 01100100
;;  20  32 < > 00100000
;;  6D 109 <m> 01101101
;;  79 121 <y> 01111001
;;  20  32 < > 00100000
;;  66 102 <f> 01100110
;;  72 114 <r> 01110010
;;  69 105 <i> 01101001
;;  65 101 <e> 01100101
;;  6E 110 <n> 01101110
;;  64 100 <d> 01100100
;; Checksum: 194
;; => 194

(defun prhex (A) "Print A in Hex and ascii by word size in bytes of max A"
  (let* ((#IO 1) (S (* 2 (c (l 256 (+ 1 (max {1 A})))))) (I 1) (L (p A)) R
	 (N (f (/ (- #PW 2) (+ S (/ S 2) 1))))
	 (PrAsc '(lambda (Start Num)
		   (Printable
		    " " (chr (rav (tr (enc (p (/ S 2) 256)
					   (aref A (+ Start (i Num)))))))))))
    (if (gt S 12) (error "Max value exceeds 12 hex digits"))
    (while (le I L)
      (princ (fmt (aref A I):(- S)::16 " "))
      (when (zerop (| N I)) (princl (fmt "* " (PrAsc (- I N) N))))
      (incr I))
    (unless (zerop (a R (| N (- I 1))))
      (princl (fmt "* ":(+ 2 (* (- N R) (+ S 1))):1 (PrAsc (- I R 1) R))))
    ))

(de Tprhex ()
    (let ((#IO 0) (Q (num "Hello this is the end my friend!")))
      (FOR I 0 6
	   (prhex (apply '+ (mapcar '(lambda (X) (* (exp 256 X) Q))
				    (explod (i I))))))))

(de CPHex (X) "Generate C code hex bytes from asci hex digit string"
    (let* ((#IO 0) (D (dechexb X)) (L (p D)))
      (princ "{")
      (FOR I 0 (- L 2)
	   (princ(low (fmt "0x" (aref D I):-2::16 ","
			   (if (zerop (| 12 (+ I 1))) "\n " " " )))))
      (princl (low (fmt "0x" (aref D (- L 1)):::16 "};")))))
