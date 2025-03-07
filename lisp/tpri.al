;;;-*- mode: emacs-lisp -*-
(defun TPRI (N LIM) ; test priority scheduling
  "(TPRI N LIM) N=number of tasks LIM num iterations in task"
  (let ((#IO 1) L Res T
	(PL (explod (? (p N 15)))) ; N random priorities between 1 and 15
	(RUN '(lambda (P) (FOR I 1 LIM (len (oblis))) P))) ; task execution
    (freeze) ; hold them back untill all have been scheduled
    (condition-case err
	(a L (mapcar '(lambda (P) (setpri P (peval `(RUN ,P)))) PL))
      (user_err (print "HOWL"))
      (symt_ovfl (princl (fmt "Too many tasks - no space for stack: "
			      (car err) " : " (cdr err))))
      )
    (thaw) ; let slip the dogs of war
    (while L
      (a T (wait -1 L)) ; wait for a finisher
      (a Res (append Res (list (cdr T)))) ; add him to the completion list
      (a L (delete  (car T) L))) ; remove him from the wait list
    ; check that they finished in priority order
    (if (equal Res (sort 'lt PL)) "TPRI ok" (print Res) "TPRI failed")
    ))
;(FOR I 1 100 (print (cdr (wait -1 (list (setpri 0 (peval '(TPRI 15 1000))))))))
