;;;-*- mode: emacs-lisp -*-
;;; adv.al --- A simple text-based adventure game.
;; Copyright (C) 2007 Conrad Barski

;; Author: Conrad Barski <drcode@gmail.com>
;; Editor: James A. Webb <uberkoder@gmail.com>
;; Created: 16 September 2007
;; Changed: 25 February  2012 Dave Penkler <dpenkler@gmail.com>
;; Version: 1.0
;; Keywords: alps lisp tutorial

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
        
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
        
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;; For the original source please see:
;; https://www.gnu.org/software/emacs/casting-spels-emacs/html/casting-spels-emacs-1.html

;; To play (load 'adv) (adv)
;; To learn about lisp and macros see the URL above from which you can
;; copy and paste the commands into the alps prompt

(require 'cl)

(setq objects '(whiskey-bottle bucket frog chain))

(setq map '((living-room (you are in the living room
              of a wizards house - there is a wizard
              snoring loudly on the couch -)
              (west door garden)
              (upstairs stairway attic))
            (garden (you are in a beautiful garden -
              there is a well in front of you -)
              (east door living-room))
            (attic (you are in the attic of the
              wizards house - there is a giant
              welding torch in the corner -)
              (downstairs stairway living-room))))

(setq object-locations '((whiskey-bottle living-room)
                         (bucket living-room)
                         (chain garden)
                         (frog garden)))

(setq location 'living-room)
(setq chain-welded nil)
(setq bucket-filled nil)

(defun describe-location (location map) (second (assoc location map)))

(defun describe-path (path)
  `(there is a ,(second path) going ,(first path) from here -))

(defun describe-paths (location map)
  (apply 'append (mapcar 'describe-path (cddr (assoc location map)))))

(defun is-at (obj loc obj-loc)  (eq (second (assoc obj obj-loc)) loc))

(defun describe-floor (loc objs obj-loc)
  (apply 'append 
	 (mapcar '(lambda (x) `(you see a ,x on the floor -))
		 (remove-if-not '(lambda (x) (is-at x loc obj-loc)) objs))))

(defun look ()
  (append (describe-location location map)
          (describe-paths location map)
          (describe-floor location objects object-locations)))

(defun walk-direction (direction)
  (let ((next (assoc direction (cddr (assoc location map)))))
    (cond (next (setq location (third next)) (look))
          (t '(you cannot go that way -)))))

(defmacro walk (direction) `(walk-direction ',(car direction)))

(defun pickup-object (object)
  (cond ((is-at object location object-locations)
         (push (list object 'body) 'object-locations)
         `(you are now carrying the ,object))
         (t '(you cannot get that))))

(defmacro pickup (object)  `(pickup-object ',(car object)))

(defun inventory ()
  (remove-if-not '(lambda (x) (is-at x 'body object-locations)) objects))

(defun have (object) (member object (inventory)))

(defmacro game-action (command subj obj place rest)
  `(defmacro ,command (subject object filler)
     `(cond ((and (eq location  ',',place)
                  (eq ',subject ',',subj)
                  (eq ',object  ',',obj)
                  (have ',',subj))
             ,@',rest)
            (t '(i cannot ,',command like that -)))))

(game-action weld chain bucket attic
  (cond ((and (have 'bucket) (setq chain-welded 't))
         '(the chain is now securely welded to the bucket -))
        (t '(you do not have a bucket -))))

(game-action dunk bucket well garden
  (cond (chain-welded (setq bucket-filled 't)
                      '(the bucket is now full of water))
        (t '(the water level is too low to reach -))))

(game-action splash bucket wizard living-room
  (cond ((not bucket-filled) '(the bucket has nothing in it -))
        ((have 'frog) (a end t)
	 '(the wizard awakens and sees that you stole
                        his frog - 
                        he is so upset he banishes you to the 
                        netherworlds - you lose! the end -))
        (t (a end t)
	   '(the wizard awakens from his slumber and greets you
             warmly - 
             he hands you the magic low-carb donut - you win!
             the end -))))

(defun adv ()
  (let (end
	(show '(lambda (X) (princl (subst "\n" '- X))))
	(help '(lambda ()
		 (let ((A '("Command character - action"
			    "h - help"
			    "l - look"
			    "w - walk west"
			    "e - walk east"
			    "u - walk upstairs"
			    "d - walk downstairs"
			    "i - inventory"
			    "p - pick up"
			    "S - splash bucket on wizard"
			    "D - dunk bucket in well"
			    "W - weld chain on bucket"
			    "q - end")))
		   (mapc 'princl A)))))
    (show (look))
    (princl "Press h for help")
    (while (not end)
      (princl (a C (getc)))
      (cond ((eq C "h") (help))
	    ((eq C "l") (show (look)))
	    ((eq C "w") (show (walk west)))
	    ((eq C "e") (show (walk east)))
	    ((eq C "u") (show (walk upstairs)))
	    ((eq C "d") (show (walk downstairs)))
	    ((eq C "i") (show (inventory)))
	    ((eq C "p") (princ "Pick up what ? ")
	     (show (pickup-object (read))) (rclr))
	    ((eq C "S") (show (splash bucket wizard)))
	    ((eq C "D") (show (dunk bucket well)))
	    ((eq C "W") (show (weld chain bucket)))
	    ((eq C "q") (a end t) 'Bye)
	    (t (princl "Huh ?"))))))
	 
