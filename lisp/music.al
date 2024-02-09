;;;-*- mode: emacs-lisp -*-
(defconst music_A_frequency 440) ; Hz

(defconst music_speed_of_sound 345) ; m/s

(defconst music_12th_root_of_two (exp 2 (/ 12))) 

(defun Note (N) (* music_A_frequency (exp music_12th_root_of_two N)))

(defun WaveLen (Note) (/ music_speed_of_sound Note))

(a AME [0 2 4 5 7 7 9 9 9 9 7 9 9 9 9 9 7 7 7 7 5 5 4 4 4 4 0])

(defun Play (S)
  (FOR I 1 (p S)
       (play (* 10000 (GenSin (Note (aref S I)) #FS (* 0.5 #FS))))
       (play (p 0 (* .2    #FS)))))

