;;;-*- mode: emacs-lisp -*-
(require 'idioms)

(defun ReadPPM (N) ;; Read ppm file with name N
  "Returns an WxHx3 array where the last dimension contains the R G B values"
  (let ((F (open N)) Line (Buf (buf "")) W H Image
	(ReadH '(lambda ()
		  (while (eql (tk 1 (a Line (getl F))) "#") nil)
		  (buf Line Buf))))
    (ReadH) ; read magic number
    (if (neql Line "P6") (error {"Unsupported file type: " Line}))
    (ReadH) ; read width height
    (a W (read Buf) H (read Buf))
    (ReadH) ; read max pixel value
    (a P (read Buf))
    (princl (fmt "Image format for image File: \"" N "\""))
    (princl (fmt "Width Height Max pixel value:" W:5 H:5 P:5))
    (if (neql P 255) (error "At present can only deal with one byte pixels"))
    (a Image (num (p {H W 3} (getb F (* H W 3)))))
    (close F)
    Image))

(defun WritePPM (N A) ;; Writes image array A to N.ppm
  (let* ((#IO 1) (F (open {N ".ppm"} "w")) (W (aref (p A) 2)) (H (aref (p A) 1)))
    (princ "P6\n" F)
    (princ "# Creator: alps WritePPM\n" F)
    (princ (fmt W:5 H:5 "\n255\n") F)
    (putb (rav (chr A)) F)
    (close F)))

(defun RepColour (C1 C2 A)  "Replace colour C1 0xRRGGBB with C2 in image A"
       (tr (enc (p 3 256) (RepElem '(lambda (V) (= C1 V)) C2 (dec 256 (tr A))))))

(defun ShowPPM (F) "Show PPM file in graphics window"
       (let (ppmCanvas (S (rev (tk 2 (p (ReadPPM F))))))
	 (require 'graf)
	 ;; Note since ppmCanvas is a local variable the window will disappear
	 ;; when the gc runs
	 (a ppmCanvas (mkwdgt EZ_WIDGET_3D_CANVAS nil
			      (list (list  EZ_ORIENTATION EZ_VERTICAL_BOTTOM)
				    (list  EZ_SIZE (+ S 2))
				    (list  EZ_BACKING_STORE 1))))
	 (gcanvas ppmCanvas)
	 (dispwdgt ppmCanvas)
	 (glinit)
	 (gdisable EZ_DEPTH_TEST EZ_LIGHTING EZ_CULL_FACE )
	 (proj nil)
;;	 (gclear)
	 (gwar ppmCanvas)
	 (gload F)
	 ))

(defun Patch3 (R C A)
  (let* ((SD (cat R C))         ;; dimensions of the submatrix
	 (Fit (f (/ (tk 2 (p A)) SD))) ;; how many submats in A [down across]
	 (S (r '* Fit))         ;; number of submats
	 (T (cat (* SD Fit) 3)) ;; Chunk to take out of A
         (LR (aref T -2)))        ;; Length of row in chunk and last dim
    (tr [1 3 2 4]
	(p {S C R 3}
	   (tr [1 3 2 4]
	       (p {(* (/ S LR) C) R LR 3}
		 (tk T A)))))))

(defun TestPPM ()
  (let ((P (Patch3 180 180 (ReadPPM "data/Alps.ppm"))))
    (FOR I 1 (tk 1 (p P)) (WritePPM (fmt "Patch" I:-3) (aref P I () () ())))))

(defun UlamPPM (N)
  (require 'prime)
  (let* ((#IO 0) (NN (sqr N)) (G (f (/ N 2))) (F (vtr {G (- G (~ (| 2 N)))}))
	 (L (p [2 8]
	       [-1  0  1  1  0  0 -1 -1    ;; Y coordinate moves (dim 1)
	         0  1  0  0 -1 -1  0  0])) ;; X coordinate moves (dim 2)
	 (SI [0 2  1  1  1  1  1  1])     ;; move multiplier increments
	 (K (rav (+ 1 (o '* (i G) SI))))  ;; compress selector for moves
	 (SPIND (+ F  (s '+ 2 (k 2 K (aref L () (| 8 (i (* 8 G)))))))) ;; spiral
 	 (A (p {N N} 1))   ;; Array with ones along the sprial of primes
	 (P (p NN 0)))     ;; Vector of zeros to receive one at prime indexes
    (iset P 1 (Primes NN)) ;; (aset P I 1) for I in primes less or equal to NN
    (iset A (cat (dp 1 P) 0) (tk {2 NN} (cat F SPIND)))
    (aref (p [2 3] [0 0 0 255 0 0 ]) A ())))
