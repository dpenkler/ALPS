;;-*- mode:emacs-lisp; -*-
;;
;; run a webserver at host I port P
;; example: (WS "localhost" 6666)
;; From the browser access it with http://localhost:6666/
;; To evaluate an expression  http://localhost:6666/hello/polly?(cons 'a 'b)
;; For browsers that don't allow port to be other than 80
;; # setcap cap_net_bind_service=ep alps
;;
(defun WS (I P) 
  (prog* (
	  (HOST (fmt I":" P))
	  (L (open HOST "l")) 
	  F #PF (#PW 132)
	  (#IO 1) HDR (Ind 1) 
;	  (Dummy 
;	   '("GET /hello/polly?(i%2010) HTTP/1.1\M" "Connection Yes\M" "\M"))
	  (GetL '(lambda () (a Ind 1) (dp -1 (getl F))))
;		   (prog1 (car Dummy) (a Dummy (cdr Dummy)))))
	  (GetWd '(lambda (L) 
		    (let* ((I (ss " " L 1 Ind)) (J Ind))
		      (incr Ind)
		      (cond ((zerop I) (incr Ind) (aref L J))
			    (t (a Ind (+ I 1)) (aref L (i {J (- I 1)})))))))
	  (GetRest '(lambda (L) (dp (- Ind 1) L)))
	  (ParseEXP
	   '(lambda ()
	      (prog (C)
		(rewind EXPB)
	      (FOR I (+ 1 Ind) (p URI)
		   (a C (aref URI I))
		   (cond
		    ((equal C "%") 
		     (a C (chr (dec 16 
				    (- (num (aref URI {(incr I) (incr I)}))
				       (num "0")))))))
		   (princ C EXPB))
	      (read EXPB))))
	  (printHead 
	   '(lambda ()
	      (princ (fmt Version " 200 OK\n\n"))
	      (princ "<html><head><title>Alps</title>\n")
	      (princ "<style>a:link {color: green; background-color: transparent; text-decoration: underline;}</style>\n")
	      (princ "<style>a:visited {color: pink; background-color: transparent; text-decoration: none;}</style>\n")
	      (princ "<body bgcolor=\"#000000\" text=\"#00FF00\"><pre>\n")
	      (princ (list Method URI Version Connection 
			   (fmt F))) ; need fmt cause print does #fob<dsfsd>
	      (terpri)))
	  (makeDirLine 
	   '(lambda (F) ; Free variable DIR contains directory name
	      (princ (fmt "<a href=\"http://" HOST "/" DIR "/" F "\">"
			  F "</a><br>"))))
	  (printTail '(lambda () (princ "</pre></body></html>\n\n")))
	  (doMp3 '(lambda (SONG) 
		    (princ (fmt "<h3>Playing <code>" SONG "</code></h3><br>"))
		    (sys "play -q " SONG "&")))
	  (print404  
	   '(lambda ()
	      (princ (fmt Version " 404 Not found.\n\n<html lang=en>"
			  "<title>Error 404 (Resource not found)</title>"
			  "<p><h1>Error 404: the URL <code>" URI  "</code> "))
	      (princ (fmt "is not known here.</h1></html>\n\n"))))
; Page variables
	  Method URI RES Version Connection EXP (EXPB (buf (p 512 (chr 0)))))
	 (LOG (a WS_RUN (fmt "Webserver listening on " HOST "\n")))
	 (while WS_RUN
	   (a F (open L))
	   (a #PF F)
	   (a RL (GetL) ; Get and log request line
	      Method (GetWd RL)
	      URI (GetWd RL)
	      Version (GetRest RL)
	      HDR nil)
	   (while (gt (p (a HL (GetL))) 1) ; get header lines
	     (push HL 'HDR)
	     (if (equal (GetWd HL) "Connection:") (a Connection (GetRest HL))))
	   (LOG (fmt Method " " URI " " Version " " Connection " " F))
	   (if (zerop (a Ind (ss "?" URI))) (a EXP nil)
	     (if (equal (aref URI (i (- Ind 1))) "/hello/polly")
		 (a EXP (ParseEXP))
	       (a EXP nil)))
	   (cond (EXP
		  (printHead)
		  (princ "\n\nEvaluating ")
		  (print EXP)
		  (princ "\n=>\n\n")
		  (print (eval EXP))
		  (princ "\nDone.\n")
		  (printTail))
		 (t 
		  (a DIR (dp 1 URI))              ; drop leading slash
		  (if (equal DIR "") 
		      (a DIR ".") ; some browsers drop leading dot
		    (if (equal (tk -1 DIR) "/")
			(a DIR (dp -1 DIR)))) ; drop trailing slash
		  (a RES (dl DIR ))
		  (cond ((null RES) (print404))
			((atom RES) ; it is a file
			 (printHead)
			 (if (equal (tk -4 RES) ".mp3") (doMp3 RES)
			   (du RES))   ; dump it
			 (printTail))
			(t          ; must be a directory
			 (a URI "index.html")
			 (printHead)
			 (princ (fmt "<h1>Directory list for <code>" DIR
				     "</code></h1><br><hl><p>"))
			 (mapc makeDirLine (sort 'lt RES))
			 (princ "</p><br><hl>\n")
			 (printTail)))))
	   (close F)
	   )
	 (close L)
	 )
  'done)
