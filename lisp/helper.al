(defun helper ()
  (sys "alps -d &")
  (wait 2)
  (a helperF (open "localhost:alps"))
  (monitor helperF)
  )

(defun run_monitor () (monitor helperF))

(defun monitor (F) ; print stuff coming from helper until a line "t" is printed
  (peval '(let ((RUN t))
	    (while (and RUN (not (eof F)))
	      (a RUN (ne "t" (princ (getl F))))
	      (terpri))
	    (terpri)
	    (if (eof F) (close F))
	    (princl "Helper monitor terminated..."))))

(defun shelp (S) (princ S helperF)) ; send command string to helper

(defun wait_helper () (while (nump (wait 1 (list helperF)))))
