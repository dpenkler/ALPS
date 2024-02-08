(de nats (f x y) ; adapted from http://dfns.dyalog.com/c_nats.htm
    (let* ((#IO 0) (px 1) (rx (exp 10 px))
	   dfm dfd dlz dlzs zro ndn nat chk rep mix arith)
(de dlz  (w) (dp (= (tk 1 w) 0) w))        ; drop leading zero
(de dlzs (w) (k (s 'v -1 -1 (<> w 0)) w))  ; drop leading zeros
(de zro  (w) (tk (c 1 (p w)) w))           ; zero for []
(de ndn  (w)            ; normalise down:  (ndn [3 21]) -> [5 1] when px=1
    (FoF (dfm (r '+ 1 (rot [1 0] (enc {0 rx} w)))) w 'eql))
(de nup  (w)            ; normalise up: (nup [3 -1])) -> [2 9] when px=1
    (FoF (dfm (+ w (r '+ 1 (rot [0 1] (o '* {rx -1} (< w 0)))))) w 'eql))
(de nat  (w) (dlzs (rep (ind #d (chk (fmt w))))))

(de chk (w) (if (onep (r '^ (elt w #d))) w (error {"Bad number " w})))
(de dck (w) (k 1 (+ [2 1] (rot (cmp '>= w) [0 -1])) w))
(de rep (w)
    (dec 10 ((dfd (tr (p w (tk (- (r '* w)) a)))) w (cat (c (/ (p w) px)) px))))
(de form (w) (aref #d (zro (dlzs (rav (tr (enc (p px 10) w)))))))
(de cmp (f w) (r f -1 -1 (rav (k (s '< -1 -1 (r '<> 1 w)) w))))
(de mix (x y) (let ((m (- (c (p x) (p y))))) (cat .5 (tk m x) (tk m y))))

(de arith (f x y)
    (print (list f x y))
    (cond ((eq f '+) (ndn (cat 0  (r '+ 1 (mix x y)))))
	  ((eq f '-) (nup (r '- 1 (dck (mix x y)))))
	  (t (error (fmt "Can't do: " f)))))

;; (de mul (a w)
;;     (dlz (ndn (cat 0 (rav (cat .5
;; 			       ((dfd (let ((digit
(form (arith f (nat x) (nat y)))))
