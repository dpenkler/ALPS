;;;-*- mode: emacs-lisp -*-

;;; From WINSTON

(setq FACTS (quote (
(ANIMAL HAS HAIR)
(ANIMAL IS MAMMAL)
(ANIMAL IS CHEETAH)
(ANIMAL HAS DARK SPOTS)
(ANIMAL HAS TAWNY COLOR)
(ANIMAL IS CARNIVORE) 
(ANIMAL EATS MEAT))))

(setq FACTS (quote (
(ANIMAL HAS HAIR)
(ANIMAL HAS DARK SPOTS)
(ANIMAL HAS TAWNY COLOR)
(ANIMAL EATS MEAT))))

(setq HYPOTHESES (quote (
(ANIMAL IS ALBATROSS)
(ANIMAL IS PENGUIN)
(ANIMAL IS OSTRICH)
(ANIMAL IS ZEBRA)
(ANIMAL IS GIRAFFE)
(ANIMAL IS TIGER)
(ANIMAL IS CHEETAH))))

(setq RULES (quote (
(RULE IDENTIFY1
     (IF (ANIMAL HAS HAIR))
     (THEN (ANIMAL IS MAMMAL)))
(RULE IDENTIFY2
     (IF (ANIMAL GIVES MILK))
     (THEN (ANIMAL IS MAMMAL)))
(RULE IDENTIFY3
     (IF (ANIMAL HAS FEATHERS))
     (THEN (ANIMAL IS BIRD)))
(RULE IDENTIFY4
     (IF (ANIMAL FLIES)
         (ANIMAL LAYS EGGS))
     (THEN (ANIMAL IS BIRD)))
(RULE IDENTIFY5
     (IF (ANIMAL EATS MEAT))
     (THEN (ANIMAL IS CARNIVORE)))
(RULE IDENTIFY6
     (IF (ANIMAL HAS POINTED TEETH)
         (ANIMAL HAS CLAWS)
         (ANIMAL HAS FORWARD EYES))
     (THEN (ANIMAL IS CARNIVORE)))
(RULE IDENTIFY7
     (IF (ANIMAL IS MAMMAL)
         (ANIMAL HAS HOOFS))
     (THEN (ANIMAL IS UNGULATE)))
(RULE IDENTIFY8
     (IF (ANIMAL IS MAMMAL)
         (ANIMAL CHEWS CUD))
     (THEN (ANIMAL IS UNGULATE)
           (EVEN TOED)))
(RULE IDENTIFY9
     (IF (ANIMAL IS MAMMAL)
         (ANIMAL IS CARNIVORE)
         (ANIMAL HAS TAWNY COLOR)
         (ANIMAL HAS DARK SPOTS))
     (THEN (ANIMAL IS CHEETAH)))
(RULE IDENTIFY10
     (IF (ANIMAL IS MAMMAL)
         (ANIMAL IS CARNIVORE)
         (ANIMAL HAS TAWNY COLOR)
         (ANIMAL HAS BLACK STRIPES))
     (THEN (ANIMAL IS TIGER)))
(RULE IDENTIFY11
     (IF (ANIMAL IS UNGULATE)
         (ANIMAL HAS LONG NECK)
         (ANIMAL HAS LONG LEGS)
         (ANIMAL HAS DARK SPOTS))
     (THEN (ANIMAL IS GIRAFFE)))
(RULE IDENTIFY12
     (IF (ANIMAL IS UNGULATE)
         (ANIMAL HAS BLACK STRIPES))
     (THEN (ANIMAL IS ZEBRA)))
(RULE IDENTIFY13
     (IF (ANIMAL IS BIRD)
         (ANIMAL DOES NOT FLY)
         (ANIMAL HAS LONG NECK)
         (ANIMAL HAS LONG LEGS)
         (ANIMAL IS BLACK AND WHITE))
     (THEN (ANIMAL IS OSTRICH)))
(RULE IDENTIFY14
     (IF (ANIMAL IS BIRD)
         (ANIMAL DOES NOT FLY)
         (ANIMAL SWIMS)
         (ANIMAL IS BLACK AND WHITE))
     (THEN (ANIMAL IS PENGUIN)))
(RULE IDENTIFY15
     (IF (ANIMAL IS BIRD)
         (ANIMAL FLYS WELL))
     (THEN (ANIMAL IS ALBATROSS))))))

(defun P (MESSAGE)
       (print (SQUASH MESSAGE)))

(defun SQUASH (S)
       (cond ((null S) nil)
             ((atom S) (list S))
             (t (append (SQUASH (car S))
                        (SQUASH (cdr S))))))

(defun MEMBER (A L) (cond ((null L) nil)
                          ((equal A (car L)) t)
                          (t (MEMBER A (cdr L)))))

(defun REMEMBER (NEW)
       (cond ((MEMBER NEW FACTS) nil)
             (t (setq FACTS (cons NEW FACTS))
                 NEW)))

(defun RECALL (FACT)
              (cond ((MEMBER FACT FACTS) FACT)
                    (t nil)))

(defun INIF (FFF)
  (mapcan '(lambda (RRR) (cond ((IFP FFF RRR)(list RRR))
			       (t nil)))
	  RULES))

(defun IFP (FF RR) (MEMBER FF (car (cdr (cdr RR)))))

(defun INTHEN (FFF)
       (mapcan (quote (lambda (RRR)
                       (cond ((THENP FFF RRR)
                              (list RRR))
                             (t nil))))
               RULES))

(defun THENP (FF RR)
       (MEMBER FF (car (cdr (cdr (cdr RR))))))

(defun FORWARD () (FORWARDaux nil) )

(defun FORWARDaux (PROGRESS)
       (cond ((STEPFORWARD) (FORWARDaux t))
             (t PROGRESS)))

(defun STEPFORWARD () (STEPFORWARDaux RULES) )

(defun STEPFORWARDaux (RULELIST)
       (cond ((null RULELIST) nil)
             ((TRYRULE (car RULELIST)) t)
             (t (STEPFORWARDaux (cdr RULELIST)))))

(defun BACKWARD () (BACKWARDaux HYPOTHESES nil) )

(defun BACKWARDaux (POSSIBILITIES ASKED)
       (cond ((null POSSIBILITIES)
              (princ "No hypothesis can be confirmed\n") nil)
             ((VERIFY (car POSSIBILITIES))
              (print (list 'Hypothesis
                           (car POSSIBILITIES)
                           "Is true"
                           (car POSSIBILITIES))))
             (t (BACKWARDaux (cdr POSSIBILITIES) ASKED))))
             
(defun TRYRULE (RULE)
  (cond ((TESTIF RULE) (USETHEN RULE))
	(t nil)))

(defun VERIFY (FACT) (VERIFYaux nil nil) )

(defun VERIFYaux (RELEVANT1 RELEVANT2)
       (cond ((RECALL FACT) t)
             (t (setq RELEVANT1 (INTHEN FACT))
                (setq RELEVANT2 RELEVANT1)
                (cond ((null RELEVANT1)
                       (cond ((MEMBER FACT ASKED) nil)
                             ((and (print (list "Is this fact true:"
						FACT
						"? [t or nil]" ))
                                   (read))
                              (REMEMBER FACT)
                              t)
                             (t (setq ASKED (cons FACT ASKED)) nil)))
                      (t (cond ((LOOP1 RELEVANT1) t)
                               (t (LOOP2 RELEVANT2))))))))

(defun LOOP1 (RELEVANT1)
       (cond ((null RELEVANT1) nil)
             ((TRYRULE (car RELEVANT1)) t)
             (t (LOOP1 (cdr RELEVANT1)))))

(defun LOOP2 (RELEVANT2)
       (cond ((null RELEVANT2) nil)
             ((TRYRULE+ (car RELEVANT2)) t)
             (t (LOOP2 (cdr RELEVANT2)))))

(defun TESTIF (RULE) (TESTIFaux (cdr (car (cdr (cdr RULE))))))

(defun TESTIFaux (IFS)
       (cond ((null IFS) t)
             ((RECALL (car IFS)) (TESTIFaux (cdr IFS)))
             (t nil)))

(defun TRYRULE+ (RULE)
       (cond ((TESTIF+ RULE) (USETHEN RULE))
             (t nil)))

(defun TESTIF+ (RULE) (TESTIF+aux (cdr (car (cdr (cdr RULE))))))

(defun TESTIF+aux (IFS)
       (cond ((null IFS) t)
             ((VERIFY (car IFS)) (TESTIF+aux (cdr IFS)))
             (t nil)))

(defun USETHEN (RULE) (USETHENaux (cdar (cdddr RULE)) nil))

(defun USETHENaux (THENS SUCCESS)
       (cond ((null THENS) SUCCESS)
             ((REMEMBER (car THENS))
              (print (list "Rule " 
                           (cadr RULE)
                           " Deduces "
                           (car THENS)))
              (setq SUCCESS t)
              (setq RULESUSED (cons RULE RULESUSED)))
             (t (USETHENaux (cdr THENS) SUCCESS))))

(defun USEDP (RULE) (USEDPaux RULE RULESUSED))

(defun USEDPaux (RULE POSSIBILITIES)
       (cond ((null POSSIBILITIES) nil)
             ((equal RULE (car (cdr (car POSSIBILITIES)))) t)
             (t (USEDPaux RULE (cdr POSSIBILITIES)))))

(defun HOW (FACT) (HOWaux RULESUSED nil))

(defun HOWaux (POSSIBILITIES SUCCESS)
       (cond ((null POSSIBILITIES)
              (cond (SUCCESS t)
                    ((RECALL FACT)
                     (print (list FACT " was given")) t)
                    (t (print (list FACT " is not established")) nil)))
             ((THENP FACT (car POSSIBILITIES))
              (setq SUCCESS t)
              (print (cons FACT (cons " demonstrated by: "
                     (mapcar '(lambda (A) (print A)))
                             (cdar (cddar POSSIBILITIES)))))
              t)
             (t (HOWaux (cdr POSSIBILITIES) SUCCESS))))

(defun WHY (FACT) (WHYaux RULESUSED nil))

(defun WHYaux (POSSIBILITIES SUCCESS)
       (cond ((null POSSIBILITIES)
              (cond (SUCCESS t)
                    ((RECALL FACT)
                     (print (list FACT " was hypothesis")) t)
                    (t (print (list FACT " is not established")) nil)))
              ((IFP FACT (car POSSIBILITIES)) (setq SUCCESS t)
               (print (cons FACT (cons " needed to show: "
				       (mapcar 'print
					       (cdadr (cddar POSSIBILITIES))))))
               t)
              (t (WHYaux (cdr POSSIBILITIES) SUCCESS))))
