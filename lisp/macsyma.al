;;;-*- mode: emacs-lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File macsyma.lisp: The implementation of MACSYMA in Chapter 8

(require 'patmat)

(a exp-p 'consp
   exp-args 'cdr)

(defun binary-exp-p (x)
  (and (exp-p x) (eq (length (exp-args x)) 2)))

(defun mkexp (x y z) (list y x z))
(a  exp-lhs 'cadr
    exp-op  'car
    exp-rhs 'caddr)

(a rule-pattern  'car
   rule-response 'cadr)

(defun prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
      (mapcar 'prefix->infix
              (if (binary-exp-p exp)
                  (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
                  exp))))

(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))

(setq *infix->prefix-rules*
  (mapcar 'expand-pat-match-abbrev
    '(((x+ = y+)    (= x y))
      ((- x+)       (- x))
      ((+ x+)       (+ x))
      ((x+ + y+)    (+ x y))
      ((x+ - y+)    (- x y))
      ((d y+ / d x) (d y x))        ;*** New rule
      ((Int y+ d x) (Int y x))      ;*** New rule
      ((x+ * y+)    (* x y))
      ((x+ / y+)    (/ x y))
      ((x+ expt y+)  (expt x y)))))

(defun infix->prefix (exp)
  "Translate an infix expression into prefix notation."
  ;; Note we cannot do implicit multiplication in this system
  (cond ((atom exp) exp)
        ((onep (length exp)) (infix->prefix (first exp)))
        ((rule-based-translator exp *infix->prefix-rules*
				rule-pattern  ; rule-if
				rule-response ; rule-then
				'(lambda (bindings response) ;action
				   (sublis (mapcar
					    '(lambda (pair)
					       (cons (first pair)
						     (infix->prefix 
						      (rest pair))))
					    bindings)
					   response))))
        ((symbolp (first exp))
         (list (first exp) (infix->prefix (rest exp))))
        (t (error "Illegal exp"))))

(defun variable-p (exp)
  "Variables are the symbols M through Z."
  ;; put x,y,z first to find them a little faster
  (member exp '(x y z m n o p q r s t u v w)))


(defparameter *simplification-rules* nil) ; Rules are in file macsyma-rules.lisp

(defun simplifier ()
  "Read a mathematical expression, simplify it, and print the result."
  (loop
    (print 'simplifier>)
    (print (simp (read)))))

(defun simp (inf) (prefix->infix (simplify (infix->prefix inf))))

(defun simplify (exp) 
  "Simplify an expression by first simplifying its components."
  (if (atom exp) exp
      (simplify-exp (mapcar 'simplify exp))))

(defun simplify-exp (exp)
  "Simplify using a rule, or by doing arithmetic."
  (cond ((rule-based-translator exp *simplification-rules*
				exp-lhs  ;rule-if 
				exp-rhs  ;rule-then
				'(lambda (bindings response)  ;action
				   (simplify (sublis bindings response)))))
        ((evaluable exp) (eval exp))
        (t exp)))

(defun evaluable (exp)
  "Is this an arithmetic expression that can be evaluated?"
  (and (every 'numberp (exp-args exp))
       (or (member (exp-op exp) '(+ - * /))
           (and (eq (exp-op exp) 'expt)
                (integerp (second (exp-args exp)))))))

;; Define n and m as numbers; s as a non-number:
(pat-match-abbrev 'n '(?is n numberp))
(pat-match-abbrev 'm '(?is m numberp))
(pat-match-abbrev 'p '(?is p negativep))
(pat-match-abbrev 's '(?is s not-numberp))

(defun not-numberp (x) (not (numberp x)))
(defun negativep (x) (and (nump x) (lt x 0)))

(defun simp-rule (rule)
  "Transform a rule into proper format."
  (let ((exp (infix->prefix rule)))
    (mkexp (expand-pat-match-abbrev (exp-lhs exp))
	   (exp-op exp) (exp-rhs exp))))

(defun simp-fn (op) (get op 'simp-fn))
(defun set-simp-fn (op fn) (set (get op 'simp-fn) fn))

(defun simplify-exp (exp)
  "Simplify using a rule, or by doing arithmetic,
  or by using the simp function supplied for this operator."
  (cond ((simplify-by-fn exp))                             ;***
        ((rule-based-translator exp *simplification-rules*
				exp-lhs ;rule-if 
				exp-rhs ;rule-then 
				'(lambda (bindings response) ;action
				   (simplify (sublis bindings response)))))
        ((evaluable exp) (eval exp))
        (t exp)))

(defun simplify-by-fn (exp)
  "If there is a simplification fn for this exp,
  and if applying it gives a non-null result,
  then simplify the result and return that."
  (let* ((fn (simp-fn (exp-op exp)))
         (result (if fn (funcall fn exp))))
    (if (null result)
        nil
        (simplify result))))

(defun factorize (exp)
  "Return a list of the factors of exp exp-operator n,
  where each factor is of the form (expt y n)."
  (let ((factors nil)
        (constant 1))
    (labels
      ((fac (x n)
         (cond
           ((numberp x)
            (setq constant (* constant (expt x n))))
           ((starts-with x '*)
            (fac (exp-lhs x) n)
            (fac (exp-rhs x) n))
           ((starts-with x '/)
            (fac (exp-lhs x) n)
            (fac (exp-rhs x) (- n)))
           ((and (starts-with x '-) (onep (len (exp-args x))))
            (a constant (- constant))
            (fac (exp-lhs x) n))
           ((and (starts-with x 'expt) (numberp (exp-rhs x)))
            (fac (exp-lhs x) (* n (exp-rhs x))))
           (t (let ((factor (find x factors :key #'exp-lhs
                                  :test #'equal)))
                (if factor
                    (incr (exp-rhs factor) n)
                    (push `(expt ,x ,n) factors)))))))
      ;; Body of factorize:
      (fac exp 1)
      (case constant
        (0 '((expt 0 1)))
        (1 factors)
        (t `((expt ,constant 1) .,factors))))))

(defun unfactorize (factors)
  "Convert a list of factors back into prefix form."
  (cond ((null factors) 1)
        ((onep (len factors)) (first factors))
        (t `(* ,(first factors) ,(unfactorize (rest factors))))))

(defun divide-factors (numer denom)
  "Divide a list of factors by another, producing a third."
  (let ((result (mapcar #'copy-list numer)))
    (dolist (d denom)
      (let ((factor (find (exp-lhs d) result :key #'exp-lhs
                          :test #'equal)))
        (if factor
            (decf (exp-rhs factor) (exp-rhs d))
            (push `(expt ,(exp-lhs d) ,(- (exp-rhs d))) result))))
    (delete 0 result :key #'exp-rhs)))

(defun free-of (exp var)
  "True if expression has no occurrence of var."
  (not (find-anywhere var exp)))

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree?  If so, return it."
  (cond ((eql item tree) tree)
        ((atom tree) nil)
        ((find-anywhere item (first tree)))
        ((find-anywhere item (rest tree)))))

(defun integrate (exp x)
  ;; First try some trivial cases
  (cond
    ((free-of exp x) `(* ,exp x))          ; Int c dx = c*x
    ((starts-with exp '+)                  ; Int f + g  = 
     `(+ ,(integrate (exp-lhs exp) x)      ;   Int f + Int g
         ,(integrate (exp-rhs exp) x)))
    ((starts-with exp '-)              
     (ecase (length (exp-args exp))        
       (1 (integrate (exp-lhs exp) x))     ; Int - f = - Int f
       (2 `(- ,(integrate (exp-lhs exp) x) ; Int f - g  =
              ,(integrate (exp-rhs exp) x)))))  ; Int f - Int g
    ;; Now move the constant factors to the left of the integral
    ((multiple-value-bind (const-factors x-factors)
         (partition-if '(lambda (factor) (free-of factor x))
                       (factorize exp))
       (identity ;simplify
         `(* ,(unfactorize const-factors)
             ;; And try to integrate:
             ,(cond ((null x-factors) x)
                    ((some '(lambda (factor)
			      (deriv-divides factor x-factors x))
                           x-factors))
                    ;; <other methods here>
                    (t `(int? ,(unfactorize x-factors) ,x)))))))))

(defun partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

(defun deriv-divides (factor factors x)
  (assert (starts-with factor 'expt))
  (let* ((u (exp-lhs factor))              ; factor = u exp n
         (n (exp-rhs factor))
         (k (divide-factors 
              factors (factorize `(* ,factor ,(deriv u x))))))
    (cond ((free-of k x)
           ;; Int k*u^n*du/dx dx = k*Int u^n du
           ;;                    = k*u^(n+1)/(n+1) for n/=1
           ;;                    = k*log(u) for n=1
           (if (eq n -1)
               `(* ,(unfactorize k) (log ,u))
               `(/ (* ,(unfactorize k) (expt ,u ,(+ n 1)))
                   ,(+ n 1))))
          ((and (eq n 1) (in-integral-table? u))
           ;; Int y'*f(y) dx = Int f(y) dy
           (let ((k2 (divide-factors
                       factors
                       (factorize `(* ,u ,(deriv (exp-lhs u) x))))))
             (if (free-of k2 x)
                 `(* ,(integrate-from-table (exp-op u) (exp-lhs u))
                     ,(unfactorize k2))))))))

(defun deriv (y x) (simplify `(d ,y ,x)))

(defun integration-table (rules)
  (dolist (i-rule rules)
    (let ((rule (infix->prefix i-rule)))
      (put (exp-op (exp-lhs (exp-lhs rule))) 'Int
	   rule))))

(defun in-integral-table? (exp)
  (and (exp-p exp) (get (exp-op exp) 'Int)))

(defun integrate-from-table (op arg)
  (let ((rule (get op 'Int)))
    (subst arg (exp-lhs (exp-lhs (exp-lhs rule))) (exp-rhs rule))))
