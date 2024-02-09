;;;-*- mode: emacs-lisp -*-
;; Base 64 encode and decode routines

(a eb64_ET "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(de eb64 (R) "encode binary character vector R to base 64"
    (let* ((#IO 0) (P (p R)) (N (c (/ P 3))) (Q (* 3 N)) (Pad (- Q P))
	   (T (p {N 3} (tk Q (num R))))
	   (L (dec 256 (tr T)))  ;; decode each 3 bytes into a 24bit value
	   (E (enc (p 4 64) L))  ;; encode each 24 bit into 4 six bit values 
	   (F (aref eb64_ET (rav (tr E))))) ;; convert to b64 encoded chars
      (aset F (p Pad "=") (- (i Pad))) F)) ;; padding
      
(de db64 (R) "Decode b64 encoded character vector R to binary character vector"
    (let* ((#IO 0) (N (c (/ (p R) 4))) (S (ind eb64_ET R)) (T (p {N 4} S))
	   (Pad (r '+ (= (tk -2 S) 64))) ;; number of padding chars
	   (L (dec 64 (tr T)))    ;; decode each 4 six bit values to 24bits
	   (E (enc (p 3 256) L))) ;; encode each 24 bit value into 3 bytes
      (chr (dp (- Pad) (rav (tr E)))))) ;; convert to 8 bit encoded chars

(de tb64 (N)
    (let ((#IO 0))
      (FOR I 1 N
	   (a T (chr (? (p (? 256) 256))))
	   (if (neql T (db64 (eb64 T)))
	       (error (fmt "Failed at" I:6 (p [] (p T)):5 (num T)))))))
    
