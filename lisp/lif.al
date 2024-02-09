;;;-*- mode: emacs-lisp -*-
;; LIF utilities
(de wd16 (B O);  ; 16 bit number from 2 chrs of B at offset O
    (dec 256 (num (aref B (+ O [0 1])))))
(de wds16 (N O B) (dec 256 (tr (p {N 2} (num (aref B (+ O (i (* 2 N))))))))) ; N wd16 from  O
(de wd32 (B O) ;; 32bit number from 4 chrs of B at offset O
    (dec 256 (num (aref B (+ O [0 1 2 3])))))
(de b16 (N) (chr (enc (p 2 256) (p [] N)))) ;; convert number N to 2 bytes
(de b32 (N) (chr (enc (p 4 256) (p [] N)))) ;; convert number N to 4 bytes
(dm wl (Buf Off) `(aref ,Buf (+ ,(car Off) (i 4))))
(de bcd (S)  (dec 10 (enc [16 16] (num S)))) ;; bcd string to digs vector
(de lifbin16 (N) (enc (p 16 2) N)) ;; 16 bit number to bit vector
(de lifbin   (B) (enc (p 8 2) B))  ;;  8 bit number to bit vector
(de lifand16 (W M) (dec 2 (^ (lifbin16 W) (lifbin16 M))))
(de lifand   (W M) (dec 2 (^ (lifbin W) (lifbin M))))
(de lifdt (B) (fmtdt (bcd B)))
(de int16  (N) (if (lt N (exp 2 15)) N (- (twoc N 16)))); signed 16 bit from num
(de uint16 (N) (if (lt N 0) (twoc N 16) N)); unsigned 16 from num
(de prt (B) (let ((A (elt B #p))) (ex A (k A B)))) ; non printable-> blank
(de mkdate (D) (chr (. '+ '* [16 1] ;; paws month starts from 1, D an alps (date)
		       (enc [10 10] (+ [0 1 0 0 0 0] (aref D (i [5 0])))))))
 
(de fmtdt (D) ;; Format date in workstation style BaseYear free
    (let ((#IO 0) (Months (p [12 3] "JanFebMarAprMayJunJulAugSepOctNovDec")))
      (condition-case Err 
	  (fmt (aref D 2):2 "-" (aref Months (- (aref D 1) 1) ())
	       "-" (mod 100 (+ BaseYear (aref D 0))):-2 " "   
	       (aref D 3):-2 ":" (aref D 4):-2 ":" (aref D 5):-2)
	(dom_range (p 18 " ")))))

;; LIF System Block fields - assume #IO 0
(de VolumeLabel (B)        (aref B (+ 2 (i 6))))
(de DirStart    (B)        (wd32 B  8))
(de DirSize     (B)        (wd32 B 16))
(de nTracks     (B)        (wd32 B 24))
(de nHeads      (B)        (wd32 B 28))
(de nSectors    (B)        (wd32 B 32))
(de Date/Time   (B) (lifdt (aref B (+ 36 (i 6)))))
(de DirStartB   (B) (* 256 (DirStart B)))  ;; byte offset of directories
(de DirSizeB    (B) (* 256 (DirSize B)))   ;; size of directories in bytes
(de NumDirs     (B) (* 8   (DirSize B)))   ;; Number of directory entries
(de VolSize     (B) (* (nTracks B) (nHeads B) (nSectors B))) ;; vol size sectors

(de SetVolLabel    (L B)   (aset B L       (+  2 (i 6))))
(de SetVolDirStart (S B)   (aset B (b32 S) (+  8 (i 4))))
(de SetVolDirSize  (S B)   (aset B (b32 S) (+ 16 (i 4))))
(de SetVolnTracks  (S B)   (aset B (b32 S) (+ 24 (i 4))))
(de SetVolnHeads   (S B)   (aset B (b32 S) (+ 28 (i 4))))
(de SetVolnSectors (S B)   (aset B (b32 S) (+ 32 (i 4))))
(de SetVolDate     (D B)   (aset B (mkdate D) (+ 36 (i 6))))

(de SysBlk (B) ;; Print sysblock details
    (let ((#IO 0) (BaseYear 1970))
      (mapc '(lambda (F) (prin0 F) (tab 16) (print (F B)))
	    '(VolumeLabel DirStart DirSize nTracks nHeads nSectors Date/Time VolSize))))

(de GetDirs (B) ;; get the array of Nx32 of all directory entries
    (p {(NumDirs B) 32}  (aref B (+ (DirStartB B) (i (DirSizeB B))))))

(de DirEnt (N B) ;; Make a list of directory entry fields for entry N
    (prog ((#IO 0) (BaseYear 1900) (DirEntNum '(lambda  (N B) N)))
	  (cond ((zerop (DirEntType N B)) (return t))
		((eq (DirEntType N B) -1) (return nil)))
	  (mapcar '(lambda (F) (F N B))
		  '(DirEntNum DirEntFileName DirEntType DirEntLocation  DirEntSize
			      DirDate        DirVol     DirLast         DirAttribute))))

(de SetDirEntField (N Buf O B) (aset B Buf (+ (DirStartB B) (* N 32) O (i (p Buf)))))

(de DirEntFileName (N B) ;; return unmangled file name (what a bloody kludge)
    (prog ((#IO 0) (Name (aref B (+ (DirEO N B DirEntNamOff) (i 10)))) C)
	  (if (eq " " (aref Name 9)) (return (k (rev (s 'v (rev (<> Name " ")))) Name)))
	  (a Name (k (rev (s 'v (rev (<> Name "_")))) Name)) ; remove trailing _'s
	  (a C (tk -1 Name)) ; last char
	  (cond ((eq C "T") {(dp -1 Name) ".TEXT"})
		((eq C "A") {(dp -1 Name) ".ASC"})
		((eq C "P") {(dp -1 Name) ".PDATA"})
		((eq C "C") {(dp -1 Name) ".CODE"})
		(t Name))))

(de MkDirFileName (N) ;; returns (list mangled_name type)
    (let* ((Dot (ind (rev N) ".")) (L (p N)) (Pref (tk (- L Dot) N)) (Suf (tk (- Dot) N)) Typ
	   (Pad '(lambda (N) (cat N (p (- 10 (p N)) "_")))))
      (if (or (gt Dot 10) (and (eql Dot 10) (onep (elt (tk -1 N) "TAPC")))) (error "File name too long"))
      (cond ((eql Suf "TEXT")  (aset Pref "T" -1) (list (Pad Pref) (hex "EA3E")))
	    ((eql Suf "ASC")   (aset Pref "A" -1) (list (Pad Pref) 1))
	    ((eql Suf "PDATA") (aset Pref "P" -1) (list (Pad Pref) (hex "EA0A")))
	    ((eql Suf "CODE")  (aset Pref "C" -1) (list (Pad Pref) (hex "EA32")))
	    (t  (list (tk 10 N) (hex "EA0A"))))))


(a DirEntNamOff 0 DirEntTypOff 10 DirEntLocOff 12 DirEntSizOff 16 DirEntDatOff 20
   DirEntVolOff 26 DirEntAttOff 28)
(de DirEO          (N B O) (+ (* N 32) (DirStartB B) O))
(de DirEntType     (N B) (int16 (wd16 B (DirEO N B DirEntTypOff))))
(de DirEntLocation (N B)        (wd32 B (DirEO N B DirEntLocOff)))
(de DirEntSize     (N B)        (wd32 B (DirEO N B DirEntSizOff)))
(de DirDate        (N B) (lifdt (aref B (+  (DirEO N B DirEntDatOff) (i 6)))))
(de DirVol         (N B) (r '^ (lifand16 (wd16 B  (DirEO N B DirEntVolOff)) (hex "7FFF"))))
(de DirLast        (N B) (not (zerop (lifand (p [] (num (aref B (DirEO N B DirEntVolOff))))
					     (hex "80")))))
(de DirAttribute   (N B)        (wd32 B  (DirEO N B DirEntAttOff)))
(de SetDirName     (N V B)   (SetDirEntField N (p 10 V)   DirEntNamOff B))
(de SetDirEntType  (N V B)   (SetDirEntField N (b16 V)    DirEntTypOff B))
(de SetDirEntLoc   (N V B)   (SetDirEntField N (b32 V)    DirEntLocOff B))
(de SetDirEntSize  (N V B)   (SetDirEntField N (b32 V)    DirEntSizOff B))
(de SetDirDate     (N D B)   (SetDirEntField N (mkdate D) DirEntDatOff B))
(de SetDirVolLast  (N V L B) (SetDirEntField N (b16 (+ V (* L (hex "8000")))) DirEntVolOff B))
(de SetDirAttr     (N V B)   (SetDirEntField N (b32 V)    DirEntAttOff B))
(de FilNum   (FL) (nth 0 FL))  ;; Directory entry number for file
(de FilName  (FL) (nth 1 FL))
(de FilType  (FL) (uint16 (nth 2 FL)))
(de FilStart (FL) (nth 3 FL))
(de FilSize  (FL) (nth 4 FL))
(de FilDate  (FL) (nth 5 FL))
(de FilVol   (FL) (nth 6 FL))
(de FilLast  (FL) (nth 7 FL))
(de FilAtt   (FL) (nth 8 FL))

(de DirList (B) ;; create list of directory entries of existing files
    (let* ((#IO 0) (DS (NumDirs B)) Tmp)
      ((label chain lambda (N)
	      (cond ((and (lt N DS) (a Tmp (DirEnt N B)) (consp Tmp)) (cons Tmp (chain (+ N 1))))
		    ((eq Tmp t) (chain (+ N 1)))))
       0)))

(de LDir (F B) ;; print directory contents sorted by field F (0 origin)
    (let ((FSSize (- (VolSize B) (DirStart B) (DirSize B))) (Busy 0))
      (princ " Fno Filename        Type   Start  Size  Date               VolE Attribute\n\n")
      (mapc '(lambda (F) (a Busy (+ Busy (FilSize F)))
	       (princl (fmt (FilNum F):4 " " (FilName F):16 (FilType F):4::16
			    (FilStart F):8 (FilSize F):6  "  "  (FilDate F)
			    (FilVol F):4 (if (FilLast F) "y" "n") (FilAtt F):10::16)))
	    (sort '(lambda (X Y) (lt (nth F X) (nth F Y))) (DirList B)))
      (fmt Busy " sectors used; " (- FSSize Busy) " remaining out of " FSSize )))
	  
(de FindFile (Name B) ;; Find file by Name
    (car (findc '(lambda (X) (eql (cadr X) Name)) (DirList B) t)))

(de LDT (B) ;; list text files with header - debug use only - throw away
    (mapc '(lambda (F)
	     (if (eq (FilType F) (hex "EA3E"))
		 (princl (fmt (FilName F):16 (FilSize F):6
			      (wds16 16 (hex "70") (GetFile(FilName F) B)):5::16))))
	  (DirList B)))

(de Gaps (B) 
    (let ((L (DirList B)) GL F FS (TFree 0) (TBusy 0) (Fno -1)
	  (FFS (+ (DirStart B) (DirSize B)))            ;; first free file space sector
	  (FL (* (nTracks B) (nHeads B) (nSectors B)))) ;; last sector
      (while L
	(a F (car L) FS (FilStart F) FZ (FilSize F) TBusy (+ FZ TBusy) Fno (FilNum F))
	;;(print (list FFS FS FZ (len GL) (car (last GL))))
	(cond ((eq FS FFS) (a FFS (+ FFS FZ)))
	      ((lt FS FFS) (error "non sequitur"))
	      (t (a GL (append GL (list(list (- FS FFS) FFS Fno)))
		    TFree (+ TFree (- FS FFS))   FFS (+ FS FZ))))
	(a   L (cdr L)))
      (cond ((lt FFS FL) (a GL (append GL (list (list (- FL FFS) FFS (+ 1 Fno)))))
	     (a TFree (+ TFree (- FL FFS))))
	    ((eq FFS FL) ())
	    (t (error "overflow")))
      (princl (fmt "Last Sect " FL ";   First Free " FFS))
      (princl (fmt "Tot Free" TFree:7 ";  Tot Busy" TBusy:7 ";  Sum" (+ TFree TBusy):7))
      GL))

(de KrunchDirs (B) 
    (let* ((D (GetDirs B)) (DK (dec 256 (num (tr (aref D () (+ DirEntTypOff (i 2)))))))
	   (LastDir (ind DK 65535)) (VDK (aref DK (i (+ 1 LastDir)))) VV
	   (D1 (rav (k 1 (<> VDK 0) (aref D (i (+ LastDir 1)) ())))))
      (aset B (cat D1 (p (- (DirSizeB B) (p D1)) (chr 0)))
	    (+ (DirStartB B) (i (DirSizeB B)))) t))

(de FindSpace (NS B) ;; Find space for NS sectors in B, returns dirno and first sector
    (prog* ((L (Gaps B)) (BestFit 1e18) T Target)
      (ngenc '(lambda (F)
	       (if (and (ge (car F) NS) (lt (a T (- (car F) NS)) BestFit)) (a BestFit T Target F))
	       (zerop BestFit)) L)
      (if (not Target) (error "No Room")  (print (list BestFit Target)) Target)))

(de AllocFile (Name File Type Attr B)
    (let*((Fnt (MkDirFileName Name))   ;; Get filename and type
	  (T (KrunchDirs B))        ;; Brute force to reduce cases to be handled
	  (Sectors (c (/ (p File) 256)))
	  (Target (FindSpace Sectors B))
	  (Free (car Target)) (FirstSector (cadr Target)) (DirNo (caddr Target))
	  (D (GetDirs B))
	  (DK (dec 256 (num (tr (aref D () (+ DirEntTypOff (i 2))))))) ;; dir kompressor
	  (LastDir (ind DK 65535)) ;; index of last dirent (type is -1)
	  (NDirs (NumDirs B)))
      (if (FindFile Name B) (error "File exists"))
      (princl (fmt "DirNo is " DirNo "   NumDirs is " (NumDirs B) "  LastDir is " LastDir))
      (if (ge LastDir NDirs) (error "no free directory slots"))
      (cond ((lt DirNo LastDir)
	     (princl (fmt "Inserting directory entry at " DirNo))
	     (a VV (cat (p (- NDirs 1) 1) 0))
	     (aset VV (+ 1 (aref VV DirNo)) DirNo)
	     (aset B (rav (k 1 VV D)) (+ (DirStartB B) (i (DirSizeB B)))))
	    (t (if (neql DirNo LastDir) (error "Directory list inconsistency"))
	       (SetDirEntType (+ DirNo 1) (hex "FFFF") B))) ;; new end of dir marker
      ;; Update directory entry for new file
      (SetDirName    DirNo (car Fnt) B)
      (SetDirEntType DirNo (if Type Type (cadr Fnt)) B) ;; use type if supplied
      (SetDirEntLoc  DirNo FirstSector B)
      (SetDirEntSize DirNo Sectors B)
      (SetDirDate    DirNo (date) B)
      (SetDirVolLast DirNo 1 1 B)
      (SetDirAttr    DirNo Attr B)
      (PutFile File DirNo B) ;; write the file contents
      t))

(de DeleteFile (Name B) 
    (let* ((DE (FindFile Name B)) (LastDir (NumDirs B)) (DirNo (car DE)) DelFlag)
      (if (not DE) (error "No such file"))
      (a DelFlag  (if (or (eql DirNo LastDir) (eql (DirEntType (+ DirNo 1) B) -1))
		      (hex "FFFF") 0))
      (SetDirEntType DirNo DelFlag B)
      t))

(de GetFile (N B) 
    (let ((#IO 0) (DE (FindFile N B)) FS)
      (unless DE (error "File not found"))
      (a FS (if (eq (FilType DE) (hex "EA0A")) (FilAtt DE) (* 256 (FilSize DE))))
      (aref B (+ (* 256 (FilStart DE)) (i FS)))))

;; PutFile assumes that the directory entry N exists and has been set up properly
(de PutFile (Buf N B) "Put file contents Buf dirent N into image buffer B"
    (let ((#IO 0) (DE (DirEnt N B)))
      (cond ((consp DE) ;; have an actual directory entry
	     (a Fsize (* 256 (FilSize DE)))
	     (if (le (p Buf) Fsize) (aset B Buf (+ (* 256 (FilStart DE)) (i (p Buf))))
	       (error "File does not fit")))
	(error "Invalid directory entry"))))

(defun TextReader (F) ;; Return Getline with F and local variables bound
  (let*
      ((#IO 0) (Fpos 1024) N (Flen (p F)) Eol C R Len
       (ChowZeros '(lambda ()
		     (while (and (lt Fpos Flen) (zerop (num (aref F Fpos))))
		       (incr Fpos))))
       (GetChar '(lambda ()
		   (when (lt Fpos Flen) ; C is free
			      (a C (aref F Fpos)) (incr Fpos) C)))
       (GetLine '(lambda ()
		   (ChowZeros)
		   (cond ((eq (GetChar) "\P")
			  (a N (- (num (GetChar)) 32)))
			 (t (a N 0 Fpos (- Fpos 1))))
		   (a Eol (ind F "\M") R nil)
		   (when (lt Eol Flen)
		     (a Len  (- Eol Fpos)
			R (cat (p N "") (aref F (+ Fpos (i Len))))
			Fpos (+ Eol 1)
			Flen (- Flen  Fpos)
			F (aref F (+ Fpos (i Flen)))
			Fpos 0))
		   R)))
    (fun GetLine)))

(defun ListTextFile (N B)
  (let ((GetLine (TextReader (GetFile N B))) R)
    (while (a R (GetLine)) (princl R))))

(defun ListAscFile (N B) "Type 1 file with 16bit length word before each record"
  (let* ((R (GetFile N B)) (Lim (p R)) (O 0) L)
    (while (lt O Lim)
      (a L (wd16 R O))
      (if (ne L 65535) (princl (aref R (+ 2 O (i L)))))
      (a O (+ O L 2 (| 2 L))))))
  
(defun ListTxtFile (N B) "ASCII file with carriage return delimited lines"
  (let ((Buf (buf (GetFile N B))) R)
    (while (a R (getl Buf "\M")) (princl R))))

(de SaveTextFile (N F B) " writes N from B to F on local filesysem"
    (let ((#PF (open F "w"))) (ListTextFile N B) (close #PF)))

(de SaveFile (N F B) "Writes bytes from file N in B to F on local filesystem"
    (if (null F) (a F N))
    (let ((F (open N "w"))) (putb (GetFile N B) F) (close F)))

(de LoadFile (F N B) "Read file F from host and put it into the image B as N as PDATA"
    (if (null F) (a F N))
    (let ((File (getfB F)))
      (if (eql (tk -4 N) ".ASC") (a File (RepChar "\n" "\M" File))) ;; DOSSIFY
      (AllocFile N File (hex "EA0A") (p File)  B)))

(de CopyFile (Name B1 B2) "Copy file from image B1 to B2"
    (let ((#IO 0) (DE (FindFile Name B1)) FS FT FA)
      (unless DE (error "File not found"))
      (a FS (* 256 (FilSize DE)) FT (FilType DE) FA (FilAtt DE))
      (AllocFile Name (aref B1 (+ (* 256 (FilStart DE)) (i FS))) FT FA B2)))
	
;; msys is a single volume
;; pas.hpi has 18 volumes of size 1048576 (pas00.hpi - pas17.hpi)
;; Volumes pas12.hpi to pas17.hpi are empty

;; Dump partitions to file NameNN from Byte array B size S N=num partitions
(de dumptn (Name B S N)
    (let ((#IO 0) Fname F)
      (FOR I 0 (- N 1)
	   (a Fname (fmt Name I:-2 ".hpi"))
	   (princl (fmt "Writing " Fname))
	   (a F (open Fname "w"))
	   (putb (aref B (+ (* I S) (i S))) F)
	   (close F)))
    'done)

(de getfB (Name) "get binary contents of file Name as chr"
    (let ((S (st Name)) F R)
      (cond ((equal (car S) Name)
	     (a F (open Name)
		R (getb F (cadr S))) ;; size of file in cadr S
	     (close F) R)
	    (t (error "No such file")))))

(defun LoadImage (ImageFile)  (getfB ImageFile))


(defun SaveImage (ImageFile B) 
 (let (F) 
    (cond ((a F (open ImageFile "w")) (putb B F) (close F)) 
     (t (error "Failed to write image file")))))

(defun LoadLif (N) (a B (LoadImage N)) (SysBlk B) (a #IO 0)  "B ready #IO=0")

(defun InitLif (Label Dirsz B)
  (aset B (chr 0) ())
  (SetVolLabel Label B)
  (SetVolDirStart 2 B)
  (SetVolDirSize  Dirsz B)
  (SetVolnTracks  1 B)
  (SetVolnHeads   1 B)
  (SetVolnSectors (f (/ (p B) 256)) B)
  (SetVolDate  (date) B)
  (aset B (chr 255) (+ (DirStartB B) (i (DirSizeB B))))
  (SysBlk B))
  
(defun LL () (LDir 1 B))
