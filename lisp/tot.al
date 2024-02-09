;;;-*- mode: emacs-lisp -*-

;;;                    notation as a Tool Of Thought

;;; Adapted for alps from Kenneth Iverson's paper of the same name at
;;; www.jsoftware.com/papers/tot.htm

;;; This file attempts to show that alps, despite the lisp syntax,
;;; still shares many of the same advantages as does APL. In addition
;;; the file itself is executable.

;;;   "The thesis of the present paper is that the advantages of
;;;   excecutability and universality found in programming languages can
;;;   be effectively combined, in a single coherent language, with the
;;;   advantages offered by mathematical notation."

;;; In the following are included some comments on computer
;;; representation which may detract a little from the heady terseness
;;; of the original but which are soley intended to make the
;;; material more readable for the casual peruser.

;; 0 Preliminary remarks on LISP (taken from McCarthy's History of Lisp)
;;   slightly adapted.


;; As a programming language, LISP is characterized by the following
;; ideas: computing with symbolic expressions rather than numbers,
;; representation of symbolic expressions and other information by
;; list structure in the memory of a computer, representation of
;; information in external media mostly by multi-level lists and
;; sometimes by S-expressions, a small set of selector and constructor
;; operations expressed as functions, composition of functions as a
;; tool for forming more complex functions (which it shares with APL),
;; the use of conditional expressions for getting branching into
;; function definitions, the recursive use of conditional expressions
;; as a sufficient tool for building computable functions, the use of
;; lambda-expressions for naming functions, the representation of LISP
;; programs as LISP data, the conditional expression interpretation of
;; Boolean connectives, the LISP function eval that serves both as a
;; formal definition of the language and as an interpreter, and
;; garbage collection as a means of handling the erasure problem. LISP
;; statements are also used as a command language when LISP is used
;; with an operating system.

;; 1 Important Characteristics of Notation (taken from the paper)


;; In addition to the executability and universality emphasized in the
;; introduction, a good notation should embody characteristics
;; familiar to any user of mathematical notation:

;; Ease of expressing constructs arising in problems.
;;  Suggestivity.
;;  Ability to subordinate detail.
;;  Economy.
;;  Amenability to formal proofs.

;; The foregoing is not intended as an exhaustive list, but will be
;; used to shape the subsequent discussion.

;; Unambiguous executability of the notation introduced remains
;; important, and will be emphasized by displaying the explicit
;; results of each expression separated by the characters ;=> This
;; avoids the result from being interpreted if the line is pasted into
;; the interpreter. For example, the integer function denoted by (i n)
;; produces a vector of the first n integers when applied to the
;; argument n , and the reduction wrt addition denoted by (r '+ v)
;; produces the sum of the elements of its vector argument v, and will
;; be shown as follows:

;; (i 5) ;=> [1 2 3 4 5]
;; (r '+ [1 2 3 4 5]) ;=> 15
;;
;; The semi-colon introduces a comment and all remaining text on the
;; line is ingored by the interpeter. In this way the one line
;; examples can be "cut" (after the initial ;;) from this file and
;; "pasted" directly into the alps interpreter.  For example here is an
;; excerpt of the dialog cutting and pasting the above two lines at the
;; alps prompt:

;; alps: (i 5) ;=> [1 2 3 4 5]
;; [1  2  3  4  5]
;; alps: (r '+ [1 2 3 4 5]) ;=> 15
;; 15
;; alps: 

;; To denote equivalence between two expressions the characters <=> are used

;; 1.1 Ease of Expressing Constructs Arising in Problems

;; If it is to be effective as a tool of thought, a notation must
;; allow convenient expression not only of notions arising directly
;; from a problem, but also of those arising in subsequent analysis,
;; generalization, and specialization.

;; Consider, for example, the crystal structure illustrated by Figure
;; 1, in which successive layers of atoms lie not directly on top of
;; one another, but lie "close-packed" between those below them. The
;; numbers of atoms in successive rows from the top in Figure 1 are
;; therefore given by (i 5) , and the total number is given by (r '+ (i 5))

;; The three-dimensional structure of such a crystal is also
;; close-packed; the atoms in the plane lying above Figure 1 would lie
;; between the atoms in the plane below it, and would have a base row
;; of four atoms. The complete three-dimensional structure
;; corresponding to Figure 1 is therefore a tetrahedron whose planes
;; have bases of lengths 1 , 2 , 3 , 4 , and 5 . The numbers in
;; successive planes are therefore the partial sums of the vector (i 5) ,
;; that is, the sum of the first element, the sum of the first two
;; elements, etc. Such partial sums of a vector v are denoted by (s '+ v),
;; the function (s '+ ) being called scan wrt addition. Thus:

;; (s '+ (i 5)) ;=> [1 3 6 10 15]
;; (r '+ (s '+ (i 5))) ;=> 35

;; The final expression gives the total number of atoms in the tetrahedron.

;; The sum (r '+ (i 5)) can be represented graphically in other ways, such as
;; shown on the left of Figure 2. Combined with the inverted pattern
;; on the right, this representation suggests that the sum may be
;; simply related to the number of units in a rectangle, that is, to a
;; product.

;; The lengths of the rows of the figure formed by pushing together
;; the two parts of Figure 2 are given by adding the vector (i 5) to
;; the same vector reversed. Thus:

;; (i 5)                ;=> [1 2 3 4 5]
;; (rev (i 5))          ;=> [5 4 3 2 1]
;; (+ (i 5) (rev (i 5)) ;=> [6 6 6 6 6]

;; Fig. 1.                      Fig. 2.
;;
;;     o                     #    #####
;;    o o                    ##    ####
;;   o o o                   ###    ###
;;  o o o o                  ####    ##
;; o o o o o                 #####    #
;;
;; This pattern of 5 repetitions of 6 may be expressed as (p 5 6) ,
;; and we have:
;; (p 5 6) ;=> [6 6 6 6  ]
;; (r '+ (p 5 6)) ;=> 30
;; (* 6 5)        ;=> 30

;; The fact that (r '+ (p 5 6)) <=> (* 6 5) follows from the
;; definition of multiplication (*) as repeated addition.

;; The foregoing suggests that (r '+ (i 5)) <=> (/ (* 6 5) 2) where
;; (/ a b) denotes division of a by b,  and , more generally, that:

;;       (r '+ (i n)) <=> (/ (* (+ n 1) n) 2)	A.1


;;1.2 Suggestivity

;; A notation will be said to be suggestive if the forms of the
;; expressions arising in one set of problems suggest related
;; expressions which find application in other problems. We will now
;; consider related uses of the functions introduced thus far, namely:

;; (i n)    integers from 1 to n
;; (rev v)  reverse the elements of a vector v
;; (p m n)  repeat m times the value n
;; (r '+ v) reduce by adding the elements of v
;; (s '+ v) scan adding the elements of v

;; The example:
;; (p 5 2) ;=> [2 2 2 2 2]
;; (r '* (p 5 2)) ;=> 32
;; suggests that (r '* (p m n)) <=> (exp n m) , where (exp n m)
;; represents the exponentiation function raising n to the power m.
;; The similiarity between the definitions of power in terms of
;; times, and of times in terms of plus may therefore be exhibited as
;; follows:
;;   (r '+ (p m n)) <=> (* n m)
;;   (r '* (p m n)) <=> (exp n m)

;; Similar expressions for partial sums and partial products may be
;; developed as follows:
;; (s '* (p 5 2))  ;=> [2 4 8 16 32]
;; (exp 2 (i 5))   ;=> [2 4 8 16 32]

;;  (s '+ (p m n)) <=> (* n (i m))
;;  (s '* (p m n)) <=> (exp n (i m))

;; Because they can be represented by a triangle as in Figure 1, the
;; sums (s '+ (i 5)) are called triangular numbers. They are a
;; special case of the figurate numbers obtained by repeated
;; applications of scan wrt addition, beginning either with (s '+ (i n)) ,
;; or with (s '+ (p n 1)) .
;; Thus:
;; (p 5 1)                      ;=> [1 1 1 1 1]
;; (s '+ (p 5 1))               ;=> [1 2 3 4 5]
;; (s '+ (s '+ (p 5 1)))        ;=> [1 3 6 10 15]
;; (s '+ (s '+ (s '+ (p 5 1)))) ;=> [1 4 10 20 35]

;; Replacing sums over the successive integers by products yields the
;; factorials (! n)  as follows:

;; (i 5)        ;=> [1 2 3 4 5]
;; (r '* (i 5)) ;=> 120
;; (s '* (i 5)) ;=> [1 2 6 4 20]
;; (! 5)        ;=> 120
;; (! (i  5))   ;=> [1 2 6 24 120]

;; Part of the suggestive power of a language resides in the ability
;; to represent identities in brief, general, and easily remembered
;; forms. We will illustrate this by expressing dualities between
;; functions in a form which embraces De Morgan's laws, multiplication
;; by the use of logarithms, and other less familiar identities.

;; If v is a vector of positive numbers, then the product (r '* v) may be
;; obtained by taking the natural logarithms of each element of v
;; denoted by (l v) , summing them (r '+ (l v)), and applying the exponential
;; function (exp (r '+ (l v))). Thus: (r '* v) <=> (exp (r '+ (l v)))

;; (exp (r '+ (l (i 5)))) ;=> 120

;; Since the exponential function (exp v) is the inverse of the
;; natural logarithm (l v) , the general form suggested by the right
;; side of the identity is: (ig (r f (g v))) where ig is the function
;; inverse to g .

;; Using (^) and (v) to denote the functions and and or, and (~) to
;; denote the self-inverse function of logical negation, we may
;; express De Morgan's laws for an arbitrary number of elements by:

;;        (r '^ b) <=> (~ (r 'v (~ b)))
;;        (r 'v b) <=> (~ (r '^ (~ b)))

;; The elements of b are, of course, restricted to the boolean values
;; 0 and 1 . Using the relation symbols to denote functions, for
;; example:
;; (< x y) yields 1 if x is less than y and 0 otherwise
;; (<> x y) yields 1 if x is not equal to y and 0 otherwise,
;; we can express further dualities, such as:

;;        (r '<> b) <=> (~ (r '=  (~ b)))
;;        (r '= b)  <=> (~ (r '<> (~ b)))

;; Finally, using (c) and (f) to denote the maximum (ceil) and minimum
;; (floor) functions, we can express dualities which involve
;; arithmetic negation for a vector v:

;;         (r 'c v) <=> (- (r 'f (- v)))
;;         (r 'f v) <=> (- (r 'c (- v)))

;; It may also be noted that scan (s fn) may replace reduction (r fn) in
;; any of the foregoing dualities.

;; 1.3 Subordination of Detail

;; As Babbage remarked in the passage cited by Cajori, brevity
;; facilitates reasoning. Brevity is achieved by subordinating detail,
;; and we will here consider three important ways of doing this: the
;; use of arrays, the assignment of names to functions and variables,
;; and the use of operators.

;; We have already seen examples of the brevity provided by
;; one-dimensional arrays (vectors) in the treatment of duality, and
;; further subordination is provided by matrices and other arrays of
;; higher rank, since functions defined on vectors are extended
;; systematically to arrays of higher rank.

;; In particular, one may specify the axis to which a function
;; applies. For example, (rev 1 m) acts along the first axis of a
;; matrix m to reverse each of the columns, and (rev 2 m) reverses
;; each row; (cat 1 m n) catenates columns (placing m above n), and
;; (cat 2 m n) catenates rows; and (r '+ 1 m) sums columns and
;; (r '+ 2 m) sums rows. If no axis is specified, the function applies along
;; the last axis. Thus (r '+ m) sums rows. For example assume that m
;; is the 4x5 rectangular matrix of the first 20 integers:

;; assign to m the 4 by 5 matrix of the first 20 integers:
;; (a m (p [4 5] (i 20)))
;; m ;=>
;; [ 1  2  3  4  5
;;   6  7  8  9 10
;;  11 12 13 14 15
;;  16 17 18 19 20]
;; (rev 1 m) ;=>
;; [16 17 18 19 20
;;  11 12 13 14 15
;;   6  7  8  9 10
;;   1  2  3  4  5]
;; (rev 2 m) ;=> 
;; [ 5  4  3  2  1
;;  10  9  8  7  6
;;  15 14 13 12 11
;;  20 19 18 17 16]
;; (r '+ 1 m) ;=>  [34 38 42 46 50]
;; (r '+ 2 m) ;=>  [15 40 65 90]

;; Two uses of names may be distinguished: constant names which have
;; fixed referents are used for entities of very general utility, and
;; ad hoc names are assigned,  by means of the assignment function (a) ,
;; to quantities of interest in a narrower context. For example, the
;; constant (name) 144 has a fixed referent, but the names crate ,
;; layer , and row assigned by the expressions

;;     (a crate 144)
;;     (a layer (/ crate 8)) 
;;     (a row (/ layer 3))

;; are ad hoc, or variable names. Constant names for vectors are also
;; provided, as in [2 3 5 7 11] for a numeric vector of five elements,
;; and in "abode" for a character vector of five elements.

;; Analogous distinctions are made in the names of
;; functions. Constant names such as + , * , and exp are assigned to
;; so-called primitive functions of general utility. The detailed
;; definitions, such as (r '+ (p m n)) for (* n m) and (r '* (p m n))
;; for (exp n m) , are subordinated by the constant names * and exp.

;; Less familiar examples of constant function names are provided by
;; the (cat) function which catenates its arguments as illustrated by:

;;      (cat (i 5) (rev (i 5))) ;=> [1 2 3 4 5 5 4 3 2 1]

;; and by the base-representation function (enc) , encode , which produces a
;; representation of its right argument in the radix specified by its
;; left argument. For example:

;;      (enc [2 2 2] 3) ;=> [0 1 1]
;;      (enc [2 2 2] 6) ;=> [1 1 0]

;; (a bn (enc [2 2 2] [0 1 2 3 4 5 6 7]))
;;  bn ;=>
;; [0 0 0 0 1 1 1 1
;;  0 0 1 1 0 0 1 1
;;  0 1 0 1 0 1 0 1]

;; (cat bn (rev bn)) ;=>
;; [0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0
;;  0 0 1 1 0 0 1 1 1 1 0 0 1 1 0 0
;;  0 1 0 1 0 1 0 1 1 0 1 0 1 0 1 0]

;; The matrix bn is an important one, since it can be viewed in
;; several ways. In addition to representing the binary numbers, the
;; columns represent all subsets of a set of three elements, as well
;; as the entries in a truth table for three boolean arguments. The
;; general expression for n elements is easily seen to be
;;     (enc (p n 2) (i (- (exp 2 n) 1)))
;; and we may wish to assign the ad hoc name T to this function using
;; the function definition function (de)

(de T (n) (enc (p n 2) (- (i (exp 2 n)) 1)))   ;=> T      A.2

;; The symbol n above represents the argument of the function. For
;; multi-argument functions as many symbols as arguments can be used
;; as long as they are unique.

;; Conditional execution can be specified by the
;; (if (Cond) (Then) (Else)) function

;; where (Cond) is a conditional expression which is evaluated
;; first. If its value is true the value of the function is the
;; evaluation of the expression (Then) otherwise it is the value of
;; the expression (Else).

;; For example, a function which produces binomial coefficients of an
;; order specified by its argument may be defined recursively as
;; follows:

(de bc (n) (let (x) (if (zerop n) 1
		      (+ (cat 0 (a x (bc (- n 1)))) (cat x 0)))))

;; Thus (bc 0) ;=> 1 and (bc 1) ;=> [1 1] and (bc 4) ;=> [1 4 6 4 1]

;; The term operator, used in the strict sense defined in mathematics
;; rather than loosely as a synonym for function, refers to an entity
;; which applies to functions to produce functions; an example is the
;; derivative operator.

;; We have already met two operators, reduction, and scan, denoted by
;; (r) and (s) , and seen how they contribute to brevity by applying to
;; different functions to produce families of related functions such
;; as (r '+)  and  (r '*) and  (r '^) .
;; We will now illustrate the notion further by
;; introducing the inner product operator denoted by a period. A
;; function (such as (r '+ ) produced by an operator will be called a
;; derived function.

;; If p and q are two vectors, then the inner product (. '+ '*) is
;; defined by:  (. '+ '* p q) <=> (r '+ (* p q))
;; and analogous definitions hold for function pairs other than + and *
;; For example:

      ;; (a p [2 3 5]  q [2 1 2])
      ;; (. '+ '* p q)   ;=> 17
      ;; (. '* 'exp p q) ;=> 300
      ;; (. 'f '+ p q)   ;=> 4

;; Each of the foregoing expressions has at least one useful
;; interpretation:
;; (. '+ '* p q) is the total cost of order quantities q for items
;; whose prices are given by p
;; because p is a vector of primes, (. '* 'exp p q) is the number
;; whose prime decomposition is given by the exponents q
;; and if p gives distances from a source to transshipment points and
;; q gives distances from the transshipment points to the destination,
;; then (. 'f '+ p q) gives the minimum distance possible.

;; The function (. '+ '*) is equivalent to the inner product or dot product
;; of mathematics, and is extended to matrices as in
;; mathematics. Other cases such as (. '* 'exp) are extended analogously. For
;; example, if T is the function defined by A.2, then:

;;       (T 3) ;=>
;; [0 0 0 0 1 1 1 1
;;  0 0 1 1 0 0 1 1
;;  0 1 0 1 0 1 0 1]

;; (. '+ '*   p (T 3)) ;=> [0 5 3 8 2 7 5 10]
;; (. '* 'exp p (T 3)) ;=> [1 5 3 15 2 10 6 30]


;; These examples bring out an important point: if b is boolean, then
;; (. '+ '* p b) produces sums over subsets of p specified by 1's in b , and
;; (. '* 'exp p b) produces products over subsets.

;; The phrase (o '* l  r) is a special use of the inner product operator to
;; produce a derived function which yields products of each element of
;; its left argument with each element of its right. For example:

;; (o '* [2 3 5] (i 5)) ;=> 
;; [2  4  6  8 10
;;  3  6  9 12 15
;;  5 10 15 20 25]

;; The function (o '*) is called outer product, as it is in tensor
;; analysis, and functions such as (o '+) and (o '*) and (o '<) are
;; defined analogously, producing "function tables" for the particular
;; functions. For example:

;; (a d [0 1 2 3])
;; (o 'c d d)  ;=> [0 1 2 3   
;;                  1 1 2 3   
;;                  2 2 2 3   
;;                  3 3 3 3]  
;;
;; (o '>= d d) ;=> [1 0 0 0              
;;                  1 1 0 0 
;;                  1 1 1 0 
;;                  1 1 1 1]
;;
;; (o '! d d)  ;=> [1 1 1 1  
;;                  0 1 2 3  
;;                  0 0 1 3  
;;                  0 0 0 1] 
;;
;; The symbol ! denotes the binomial coefficient function, and the
;; table (o '! d d) is seen to contain Pascal's triangle with its apex at
;; the left; if extended to negative arguments , as with
;; (a d [-3 -2 -1 0 1 2 3]) it will be seen to contain the triangular
;; and higher-order figurate numbers as well. This extension to
;; negative arguments is interesting for other functions as well.

;; (o '! d d)
;; [  1 -2  1 0 0 0 0
;;    0  1 -1 0 0 0 0
;;    0  0  1 0 0 0 0
;;    1  1  1 1 1 1 1
;;   -3 -2 -1 0 1 2 3
;;    6  3  1 0 0 1 3
;;  -10 -4 -1 0 0 0 1]

;; For example, the table (o '* d d) consists of four quadrants
;; separated by a row and a column of zeros, the quadrants showing
;; clearly the rule of signs for multiplication.

;; (o '* d d)
;; [ 9  6  3 -0 -3 -6 -9
;;   6  4  2 -0 -2 -4 -6
;;   3  2  1 -0 -1 -2 -3
;;  -0 -0 -0  0  0  0  0
;;  -3 -2 -1  0  1  2  3
;;  -6 -4 -2  0  2  4  6
;;  -9 -6 -3  0  3  6  9]


;; Patterns in these function tables exhibit other properties of the
;; functions, allowing brief statements of proofs by exhaustion. For
;; example, commutativity appears as a symmetry about the
;; diagonal. More precisely, if the result of the transpose function
;; (tr) (which reverses the order of the axes of its argument) applied
;; to a table (a t (o fun d d)) agrees with t , then the function fun
;; is commutative on the domain. For example,
;; (= t (tr (a t (o 'c d d))))
;; produces a table of 1's because c (ceil) is commutative.

;; Corresponding tests of associativity require rank 3 tables of the
;; form (o fun d (o fun d)) and (o fun (o fun d d) d). For example:

;; (a d [0 1])
;; (o '^ d (o '^ d d))    ;=>  [0  0
;;                              0  0
;;
;;                              0  0
;;                              0  1]

;; (o '^ (o '^ d d) d)    ;=>  [0  0
;;                              0  0
;; 
;;                              0  0
;;                              0  1]

;; (o '<= d (o '<= d d))  ;=>  [1  1
;;                              1  1
;; 
;;                              1  1
;;                              0  1]

;; (o '<= (o '<= d d) d)  ;=> [0  1
;;                             0  1
;; 
;;                             1  1
;;                             0  1]


;; 1.4 Economy

;; The utility of a language as a tool of thought increases with the
;; range of topics it can treat, but decreases with the amount of
;; vocabulary and the complexity of grammatical rules which the user
;; must keep in mind. Economy of notation is therefore important.

;; Economy requires that a large number of ideas be expressible in
;; terms of a relatively small vocabulary. A fundamental scheme for
;; achieving this is the introduction of grammatical rules by which
;; meaningful phrases and sentences can be constructed by combining
;; elements of the vocabulary.

;; This scheme may be illustrated by the first example treated - the
;; relatively simple and widely useful notion of the sum of the first
;; n integers was not introduced as a primitive, but as a phrase
;; constructed from two more generally useful notions, the function (i n)
;; for the production of a vector of integers, and the function (r '+ v) for
;; the summation of the elements of a vector. Moreover, the derived
;; function (r '+) is itself a phrase, summation being a derived function
;; constructed from the more general notion of the reduction operator
;; applied to a particular function.

;; Economy is also achieved by generality in the functions
;; introduced. For example, the definition of the factorial function
;; denoted by (!) is not restricted to integers, and the gamma
;; function of v may therefore be written as (! (- v 1)) including
;; complex values for v . Similarly, the relations defined on all
;; real arguments provide several important logical functions when
;; applied to boolean arguments: exclusive-or (x), material
;; implication (<=), and equivalence (=).

;; The economy achieved for the matters treated thus far can be
;; assessed by recalling the vocabulary introduced:

;;      i  p rev enc cat 
;;      r s  o  .
;;
;;      + - * / exp ln ! c f tr
;;      v ^ ~ < <= = <= < <>

;; The five functions and four operators listed in the first two rows
;; are of primary interest, the remaining familiar functions having
;; been introduced to illustrate the versatility of the operators.

;; A significant economy of symbols, as opposed to economy of
;; functions, is attained by allowing any symbol to represent both a
;; monadic function (i.e. a function of one argument) and a dyadic
;; function, in the same manner that the minus sign is commonly used
;; for both subtraction and negation. Because the two functions
;; represented may, as in the case of the minus sign, be related, the
;; burden of remembering symbols is eased.

;; For example, (exp x y) and (exp y) represent power and exponential,
;; (ln x y) and (ln y) represent base x logarithm and natural logarithm,
;; (/ x y) and (/ y) represent division and reciprocal,

;; and (! x y) and (! y) represent the binomial coefficient function
;; and the factorial that is:
;;         (! x y) <=> (/ (! y) (* (! x) (! (- y x)))

;; The symbol p used for the dyadic function of replication also
;; represents a monadic function which gives the shape of the argument
;; that is: x <=> (p (p x y),
;; In APL the symbol used for the monadic reversal
;; function also represents the dyadic rotate function
;; In alps these each have their own symbol for readability
;; (rot 2 (i 5)) ;=> [3 4 5 1 2]
;; (rot -2 (i 5));=> [4 5 1 2 3]
;; (rev (i 5))   ;=> [5 4 3 2 1]

;; Also in APL the comma represents not only catenation, but also the
;; monadic ravel, which produces a vector of the elements of its
;; argument in "row-major" order.
;; In alps they have their own symbols cat and rav respectively.

;; (T 2)  ;=>
;; [0 0 1 1                 
;;  0 1 0 1]

;; (rav (T 2)) => [0 0 1 1 0 1 0 1]

;; alps using LISP's prefix notation has a polyadic operation for
;; catenation {}:

;; {}      ;=> []
;; {(i 5)} ;=> [1 2 3 4 5]
;; {(i 5) (! (i 5))} ;=> [1 2 3 4 5 1 2 6 24 120]
;; {3 2 1 (i 5) (! (rev (i 5)))} ;=> [3 2 1 1 2 3 4 5 120 24 6 2 1]


;; Simplicity of the grammatical rules of a notation is also
;; important.

;; Syntactic rules are made as simple as possible by adopting a single
;; form for all expresions. Expressions are comprised of either a
;; simple name or literal or a function followed by its arguments all
;; enclosed in paretheses and separated by spaces as in LISP. The
;; function and arguments themselves are also expressions.
;; (function arg1 arg2 ...)

;; This contrasts with the variety of rules in mathematics. For
;; example, the symbols for the monadic functions of negation,
;; factorial, and magnitude precede, follow, and surround their
;; arguments, respectively. Dyadic functions show even more variety.

;; The extra symbols required for this LISP prefix syntax, parentheses
;; and space separators, make the notation somewhat more cumbersome
;; that of the very concise APL infix notation. However the
;; parentheses allow for improved readability with automatic
;; indentation. Further simplicity is afforded through the regular
;; form for nilladic, monadic, dyadic and polyadic expressions,
;; including system functions. This simple regular syntax also
;; facilitates the programmatic manipulation of expressions for
;; modifying them or generating expressions with macros.

;; A further simplification is that the order of evaluation of an
;; expression is strictly from left to right. This rule has certain
;; consequences in reduction and scan for non-commutative
;; functions. Since (r f v) is equivalent to placing the function f in
;; front of the elements of v the semantics are equivalent to applying
;; f to all the elements of v in order from left to right.

;; (r '- [1 2 3 4]) is equivalent to (- 1 2 3 4) => -8 which subtracts
;; each subsequent element from the first. This is different from how
;; APL handles reduction and scan as it uses right to left
;; evaluation. To enable the useful applications of right to left
;; evaluation in reduction and scan one cane use the direction
;; argument after the axis argument. For example the expression
;; (r '- -1 -1 v) gives the alternating sum of the elements of v.
;; (r '- -1 -1 [1 2 3 4]) => -2. This is equivalent to
;; (1 - (2 - (3 - 4))) => (1 - (2 - -1)) => (1 - 3) => - 2, or
;; 1 - 2 + 3 - 4 => -2. (r '/ -1 -1 v) gives the alternating
;; product. Moreover, if b is a boolean vector, then (s '< -1 -1 b)
;; "isolates" the first 1 in b , since all elements following it
;; become 0. For example.
;; (s '< 1 -1 [0 0 1 1 0 1 1]) ;=> [0 0 1 0 0 0 0]


;; 1.5 Amenability to Formal Proofs

;; The importance of formal proofs and derivations is clear from their
;; role in mathematics. Section 4 is largely devoted to formal proofs,
;; and we will limit the discussion here to the introduction of the
;; forms used.

;; Proof by exhaustion consists of exhaustively examining all of a
;; finite number of special cases. Such exhaustion can often be simply
;; expressed by applying some outer product to arguments which include
;; all elements of the relevant domain. For example, if (a d [0 1]) ,
;; then (o '^ d d) gives all cases of application of the and
;; function. Moreover, De Morgan's law can be proved exhaustively by
;; comparing each element of the matrix(o '^ d d) with each element of
;; (~ (o 'v (~ d) (~ d)))  as

;; (o '^ d d)             ;=> [0 0
;;                             0 1]

;; (~ (o 'v (~ d) (~ d))) ;=> [0 0
;;                             0 1]

;; 2 Polynomials

;; A polynomial can effectively be represented by the vector of its
;; coefficients. There are two ways in which this can be done. If one
;; considers the subscripts of a vector to increase from 1 to n from
;; left to right, where n is the length of the vector, the
;; correspondence of coefficients can be with increasing or decreasing
;; powers of the variable. If [1 2 3 4] for example is a vector of
;; coefficients in decreasing powers the polynomial thus represented
;; is: x^3 + 2x^2 + 3x + 4. For a coefficient vector [1 2 3 4] of
;; increasing powers it would be: 1 + 2x + 3x^2 + 4x^3

;; A similar choice exists also for the binary representation of
;; integers in computers. The discussions motivated by preferences for
;; one over the other have been compared to the Lilliputian dispute
;; over which end to start eating an egg from. The representation with
;; decreasing powers has been referred to as "big-endian" because the
;; vector starts with the coefficient of the highest exponent. Both
;; representations are useful.  Big-endian is the way we write
;; numbers, for example 4098 for 4 thousands, no hundreds, 9 tens and
;; 8 units. To evaluate a polynomial given as a coefficient vector of
;; decreasing powers at a single value one can use the decode
;; function:
(dec 10 [4 0 9 8]) ;=> 4098 with powers of 10
(dec 16 [1 0 0 2]) ;=> 4098 with powers of 16
;; Evaluating the polynomial x^3 + 2x^2 + 3x + 4 at the values 0 1 2 3 and 4:
(dec 0 [1 2 3 4]) ;=> 4
(dec 1 [1 2 3 4]) ;=> 10
(dec 2 [1 2 3 4]) ;=> 26
(dec 3 [1 2 3 4]) ;=> 58
(dec 4 [1 2 3 4]) ;=> 112

;; The big endian representation is also more intutive since it is the
;; representation which is used by humans to "manually" perform
;; polynimial long division where one typically starts at the left
;; with the highest powers working to the right to the lower order
;; terms.

;; In the following discussion a polynomial is represented by a
;; coefficient vector of increasing powers. This gives a more
;; intuitive correspondence between the exponent and the subscript of
;; the corresponding coefficient in the vector. For example the above
;; polynomial has exponents: (- (i 4) 1) ;=> [0 1 2 3].  So to
;; evaluate it at a single value say 3 the value of 3 raised to the
;; exponents [0 1 2 3] are:
;;                      (exp 3  (- (i 4) 1))  ;=> [1  3  9  27]
;; Multiplying by the respective coefficients:
;;        (* [4 3 2 1]  (exp 3  (- (i 4) 1))) ;=> [4  9  18  27]
;; and finally summing over the lot:
;; (r '+  (* [4 3 2 1]  (exp 3  (- (i 4) 1)))) ;=> 58

;; Thus by sacrificing the familiarity of the big-endian
;; representation one obtains a more unobtrusive correspondence
;; between the coefficients in a vector and their respective exponent
;; powers. If one counts the vector positions from 1 to n from the
;; left then the exponent corresponding to the coefficient i is simply
;; i-1.


;; Using the inner product to perform the summing and multiplication
;; and the outer product for the exponentiation the function to
;; perform the evaluation of a polynomial a at multiple values x
(de poly (a x)  (. '+ '* (o 'exp x (- (i (p a)) 1)) a))  
;; (poly [4 3 2 1] [0 1 2 3]) ;=> [4  10  26  58]
;; Observe that the evaluation at the last point 3 is 58 as above
;;
;; (poly [1 3 3 1] [0 1 2 3 4]) ;=> [1  8  27  64  125]
;; Remark: This gives the same result as raising the first 5 natural
;; numbers to the power of 3: (exp (i 5) 3) ;=> [1 8 27 64 125]
;; (x + 1)^3 => 1 + 3x + 3x^2 + x^3
;; substituting [0 1 2 3 4] for x in the left hand expression
;; clearly produces the first 5 cubes. Similarly for (x + 1)^4:
;; (poly [1 4 6 4 1] [0 1 2 3 4]) ;=> [1 16 81 256 625]
;; (exp (i 5) 4)                  ;=> [1 16 81 256 625]

;; For conciseness we define the matrix multiplication function  m*
;; as the inner product wrt to addition and multiplication:
(de m* (x y) (. '+ '* x y))

;; Curve fitting: Given points y resulting from the evaluation of a
;; polynomial of unknown coefficients c at points x where the lengths
;; of x and y are equal we can define a function C that will determine
;; the coefficients c of the unknown polynomial that fits the curve
;; described by the points. This is done by multiplying the inverse of
;; the matrix (m/) of outer product wrt to exponentiation of x and the
;; vector of its exponents powers (m/ (o 'exp x (+ -1 (i (p x)))))
;; with y.
(de C (x y) (m* (m/ (o 'exp x (+ -1 (i (p x))))) y))
;; Evaluating the polynomial [1 3 3 1] at the points [2 3 4 5]:
;; (poly [1 3 3 1] [ 2 3 4 5])  ;=> [27  64  125  216]
;; Feeding the points  [2 3 4 5] and the value of the unknown polynomial
;; at those points [27  64  125  216] to C:
;; (C [2 3 4 5] [27 64 125 216]) ;=> [1 3 3 1]

;; We find that the returned coefficients correspond to the original
;; polynomial. Trying it at different points say (a V [3 6 7 16])
;; still produced the correct result:
;; (C V (poly [1 3 3 1] V)) ;=> [1 3 3 1]

;; 2.0 Adding and subtracting polynomials

;; Adding or subtracting two polynomials can be accomplished by simply
;; adding or subtracting their respective coefficients. The only thing
;; that must be ensured is that their coefficient vectors have the
;; same length. In the two functions below l is the length of the
;; longer vector which is obtained by reducing wrt to ceil (maximum)
;; their catenated lengths. The take function (tk l v) takes l
;; elements from v filling with zeros if the length of v is less than
;; l.
(de p+ (a b) (let ((l (r 'c (cat (p a) (p b))))) (+ (tk l a) (tk l b))))
;; (p+ [1 2 3 4] [2 3]) ;=> [3  5  3  4]
(de p- (a b) (let ((l (r 'c (cat (p a) (p b))))) (- (tk l a) (tk l b))))
;; (p- [3 5 3 4] [2 3]) ;=> [1  2  3  4]
;; (p- [1 2 3 4] [1 2 3 3]) ;=> [0  0  0  1]
;; (poly [0 0 0 1] 10) ;=> 1000
;; (p- [2 2 3 4] [1 2 3 4]) ;=> [1  0  0  0]
;; (poly [1 0 0 0] 10) ;=> 1
;; Should it be required to eliminate the trailing zeros a variant of
;; the ElimTB idiom from idioms.al can be used:
(de ElimT0 (V) (k (rev (s 'v (<> (rev V)))) V))
;;  (ElimT0 [0 2 1 0 0 0 ]) ;=> [0  2  1]

;; 2.1 Products of Polynomials

;; Evaluate the product of polynimials b and c at x
(de poly* (b c x) (r '+ (rav (* (o '* b c)
			     (exp x (o '+
				       (- (i (p b)) 1)
				       (- (i (p c)) 1)))))))
;; (poly* [3 1 2 3] [2 0 3] 2)             ;=> 518
;; (* (poly [3 1 2 3] 2) (poly [2 0 3] 2)) ;=> 518
;; Taking it step by step:
;; Assume for the rest of the discussion that
;; the vector b has the value [3 1 2 3] and c => [2 0 3]
(a b [3 1 2 3] c [2 0 3]) ;; assign vectors b and c
;; the exponents of b (- (i (p b)) 1) ;=> [0 1 2 3]
;; the exponents of c (- (i (p c)) 1) ;=> [0 1 2]
;; the outer product wrt addition of these exponents:
;; (o '+ (- (i (p b)) 1) (- (i (p c)) 1)) ;=>
;; [0 1 2
;;  1 2 3
;;  2 3 4
;;  3 4 5]
;; evaluating the array of exponents at x=2
;; (exp 2 (o '+ (- (i (p b)) 1) (- (i (p c)) 1))) ;=>
;; [1  2  4
;;  2  4  8
;;  4  8 16
;;  8 16 32]
;; Now we must multiply these values by their corresponding coefficients.
;; These are obtained by performing the outer product wrt multiplication
;; of the coefficients of b and c:
;; (o '* b c)  ;=>
;; [6 0 9
;;  2 0 3
;;  4 0 6
;;  6 0 9]
;; multiplying the array of evaluated exponents by the coefficient
;; products:
;; (* (o '* b c) (exp 2 (o '+ (- (i (p b)) 1) (- (i (p c)) 1)))) ;=>
;; [ 6 0  36
;;   4 0  24
;;  16 0  96
;;  48 0 288]
;; (un)ravelling the result
;; (rav (* (o '* b c) (exp 2 (o '+ (- (i (p b)) 1) (- (i (p c)) 1))))) ;=>
;; [6 0 36 4 0 24 16 0 96 48 0 288]
;; and summing
;; (r '+ [6 0 36 4 0 24 16 0 96 48 0 288]) ;=> 518
;; Note how we use addition in the outer product of the exponents and
;; multiplication in the outer product of the coefficients.
;; Note that the exponents corresponding this product of coefficients is:
;; (rav (o '+ (- (i (p b)) 1) (- (i (p c)) 1))) ;=> [0 1 2 1 2 3 2 3 4 3 4 5]

;; To obtain the vector of the coefficients of the product in the
;; standard representation we need to sum coefficients of equal
;; exponents and put them in increasing order of the exponents
;; The array of exponents is
;; (o '+ (- (i (p b)) 1) (- (i (p c)) 1)) ;=>
;; [0 1 2
;;  1 2 3
;;  2 3 4
;;  3 4 5]
;; Let's call this array M: (a M (o '+ (- (i (p b)) 1) (- (i (p c)) 1)))
;; Examining M we see that each subsequent row is shifted left by one.
;; Padding on the right with 3 zeros
;; (cat M (p [4 3] 0) ;=>
;; [0 1 2 0 0 0
;;  1 2 3 0 0 0
;;  2 3 4 0 0 0
;;  3 4 5 0 0 0]
;; and rotating each row by 1 less than the row index:
;; (rot [0 -1 -2 -3] (cat M (p [4 3] 0)))
;; [0 1 2 0 0 0
;;  0 1 2 3 0 0
;;  0 0 2 3 4 0
;;  0 0 0 3 4 5]
;; we can get the exponents to line up by columns.

;; Therefore by performing the same transformation on the coefficients
;; and summing the columns we obtain the coefficients of the product
;; in increasing order of the exponents.

;; (o '*  b c) ;=>
;; [6  0  9
;;  2  0  3
;;  4  0  6
;;  6  0  9]
;; Padding c with 1 less zero than elements in b:
;; (o '* b (cat c (dp 1 (* b 0))))
;; [6  0  9  0  0  0 
;;  2  0  3  0  0  0 
;;  4  0  6  0  0  0 
;;  6  0  9  0  0  0]
;; Rotating individual rows by (- 1 (i (p b))) ;=> [0  -1  -2  -3]
;; (See ColDiag in idioms.al)
;; (rot (- 1 (i (p b))) (o '* b (cat c (dp 1 (* b 0))))) ;=>
;; [6  0  9  0  0  0
;;  0  2  0  3  0  0
;;  0  0  4  0  6  0
;;  0  0  0  6  0  9]
;; Summing the columns
;; (r '+ 1 (rot (- 1 (i (p b))) (o '* b (cat c (dp 1 (* b 0)))))) ;=>
;; [6  2  13  9  6  9]
;;
;; Thus we can define our function to compute coefficients of the
;; product of two polynomials:
(de p* (b c) (r '+ 1 (rot (- 1 (i (p b))) (o '* b (cat c (dp 1 (* 0 b)))))))    
;; (p* b c) ;=> [6  2  13  9  6  9]

;; 2.1.1 Polynomial division

;; We will now develop an alternative method for the product of 2
;; polynomials based upon the simple observation that if (p* b c)
;; produces the product of polynomials b and c, then p* is linear in
;; both its arguments.

;; Consequently we can define a function p*a: to multiply b and c
;; using an array a: (de p*a (b c) (m* b (m* a c)))
;; where a is an array to be determined.
;; The rank of a must be 3 and must depend on
;; 1) the exponents of b (+ -1 (i (p b))) i.e. 0 1 2 3 ...,
;; 2) the exponents of the result: (+ -1 (i (p (dp 1 (cat b c)))))
;; 3) the exponents of c  (+ -1 (i (p c)))

;; The "deficiencies" of the right exponents (c) are given by the
;; difference table:
;; (o '- (i (p (dp 1 (cat b c)))) (i (p c)))

;; Assume (a b [3 1 2 3] c [2 0 3]) , as before, then:
;; the exponents of b are: (+ -1 (i (p b)))       => [0 1 2 3]
;; of the result: (+ -1 (i (p (dp 1 (cat b c))))) => [0  1  2  3  4  5]
;; and of c:  (+ -1 (i (p c)))                    => [0  1  2  3]
;; The difference table (o '- (i (p (dp 1 (cat b c)))) (i (p c))) is then
;; [0  -1  -2
;;  1   0  -1
;;  2   1   0
;;  3   2   1
;;  4   3   2
;;  5   4   3]
;; and comparison of these values with the left exponents (b) yields
;; the array a with 1's where the exponents of b are equal to the
;; entries in the difference table.
(a a  (o '= (+ -1 (i (p b))) (o '- (i (p (dp 1 (cat b c)))) (i (p c)))))
;; So our rank 3 array a =>
;;    [1  0  0
;;     0  1  0
;;     0  0  1
;;     0  0  0
;;     0  0  0
;;     0  0  0
    
;;     0  0  0
;;     1  0  0
;;     0  1  0
;;     0  0  1
;;     0  0  0
;;     0  0  0
    
;;     0  0  0
;;     0  0  0
;;     1  0  0
;;     0  1  0
;;     0  0  1
;;     0  0  0
    
;;     0  0  0
;;     0  0  0
;;     0  0  0
;;     1  0  0
;;     0  1  0
;;     0  0  1]

;; Based on this we can now define an alternate function for the
;; polynomial product:
(de p*a (b c)
    (let ((a (o '= (+ -1 (i (p b))) (o '- (i (p (dp 1 (cat b c)))) (i (p c))))))
      (m* b (m* a c))))
;; and  (p*a b c) ;=> [6  2  13  9  6  9] as before

;; Since (m* b a) is a matrix, =>
;; [3  0  0
;;  1  3  0
;;  2  1  3
;;  3  2  1
;;  0  3  2
;;  0  0  3]

;; The formulation of the product suggests that if d is the product of
;; b and c: d <=> (m* b (m* a c))
(a d (p*a b c))
;; then c might be obtained by
;; premultiplying d by the inverse of (m* b a): (m* (m/ (m* b a)) d) ,
;; thus providing division of polynomials. The function m/ to invert or
;; divide matrices is a primitive of the alps language.

;; Since (m* b a) is not a square matrix, this will not work, but by
;; replacing the non-square matrix either by its leading or trailing
;; square parts one obtains two results, one corresponding to division
;; giving low-order remainder terms, and the other giving high-order
;; remainder terms.

;; For example let M be the matrix (m* b a): (a M (m* b a))
;; Then the leading square part
(a M1 (tk (p 2 (r 'f (p M))) M))
;; M1 ;=>
;; [3  0  0
;;  1  3  0
;;  2  1  3]
;; And the trailing square part:
(a M2 (tk (- (p 2 (r 'f (p M)))) M))
;; M2 ;=>
;; [3  2  1
;;  0  3  2
;;  0  0  3]

;; So to divide d by by premultiplying the leading conformant part of
;; d ,   (tk (dp 1 (p M1)) d) ;=> [6 2 13]
;; by the inverse of leading square part of (m* b a), i.e. (m/ M1):
;; (m* (m/ M1) (tk (dp 1 (p M1)) d)) ;=> [2 0 3] which is indeed equal to c

;; Using the  trailing part of d and (m* b a):
;; is (tk (- (dp 1 (p M2))) d) ;=> [9 6 9]
;; (m* (m/ M2) (tk (- (dp 1 (p M1))) d)) ;=> [2 0 3] as before

;; Trying it the other way round and dividing d by c we should obtain
;; b. We do this generalising the expressions a bit to work towards a
;; general division function. We use the fact that the dimension of
;; the square matrix will be one more than the difference between the
;; lengths of dividend d and the divisor c, which is effectively the
;; length of the result, which in this case is the length of b. 
(a L (- (p d) (p c) -1))
;; The exponent array can now be defined as
(a A (o '= (- (i (p c)) 1) (o '- (i (p d)) (i L))))
;; the leading square matrix as      (tk    (p 2 L)  (m* A c))
;; and the trailing square matrix as (tk (- (p 2 L)) (m* A c))

;; Premultiplying the leading conformant part of d by the inverse leading 
;; square matrix:
;; (m* (m/ (tk (- (p 2 L)) (m* c A))) (tk (- L) d)) ;=> [3 1 2 3]
;; which is equal to the vector b

;; So the function (p/hr d e) using the leading (low order) terms to
;; obtain the result of dividing d by e to obtain a result with the
;; high order remainder terms can be defined as
(de p/hr (d e)
    (let* ((L (- (p d) (p e) -1)) ;; length of result
	   (A (o '= (- (i (p e)) 1) (o '- (i (p d)) (i L)))));; exponent array
      (m* (m/ (tk (p 2 L) (m* e A))) (tk L d))))

;; Similarly using the trailing parts (high order terms) to obtain a
;; result with the low order remainder terms
(de p/lr (d e)
    (let* ((L (- (p d) (p e) -1)) ;; length of result
	   (A (o '= (- (i (p e)) 1) (o '- (i (p d)) (i L)))));; exponent array
      (m* (m/ (tk (- (p 2 L)) (m* e A))) (tk (- L) d))))

;; (p/lr d b) => [2 0 3]    ;; (p/lr d c) => [3 1 2 3]
;; (p/hr d b) => [2 0 3]    ;; (p/hr d c) => [3 1 2 3]

;; The results between p/lr and p/hr agree as there is no remainder
;; Before returning to the the division problem we make a brief
;; digression.

;; 2.1.2 Polynomial exponentiation

;; A function to perform the exponentiation of a polynomial p by by n
;; can be defined as a simple recursive function on n that multiplies
;; the polynimial p by itself n times using the polynomial product
;; function (p* x y) defined above
(de pexp (P N) (if (lt N 1) 1 (p* P (pexp P (- N 1)))))

;; Raising the polynomial (x + 1) to the power 5:
;; (pexp [1 1] 5) ;=> [1 5 10 10 5 1]
;; Dividing this by (x + 1)^2 should give (x + 1)^3:
;; (p/lr (pexp [1 1] 5) (pexp [1 1] 2)) ;=> [1 3 3 1]

;; The dyadic combination function (! m n) , which gives the number of
;; ways m objects can be selected out of a set of n , can be used to
;; generate the binomial coefficients of (x + 1)^n
;; (! [0 1 2 3 4 5] 5) ;=> [1 5 10 10 5 1]
(de Bin (N) (! (- (i (+ N 1)) 1) N))
;; (Bin 5) ;=> [1 5 10 10 5 1]
;; (Bin 3) ;=> [1 3 3 1]
;; (Bin 2) ;=> [1 2 1]

;; 2.1.3 General polynomial division

;; Coming back to the polynomial division problem we consider two last
;; examples to help derive a general function.
;; Firstly let's look at the following division:
;; (x - 1)^3 / (x + 1) where we have
(a D1 (pexp [1 -1] 3) E1 [1 1]) ;; dividend D1 and divisor E1
;; Calculate the quotients
(a Q1L (p/lr D1 E1))  ;=> [-7 4 -1]
(a Q1H (p/hr D1 E1))  ;=> [1 -4 7]
;; calculate the remainders by subracting the product of the quotient
;; and divisor (p* Q1x E1) from the dividend D1
(a R1L (p- D1 (p* Q1L E1)))  ;=> [8 0 0 0]  ;; Low order remainder
(a R1H (p- D1 (p* Q1H E1)))  ;=> [0 0 0 -8] ;; High order remainder

;; So with the low order remainder D1/E1 => -x^3 +4X -7 + (8/(x + 1))
;; Checking by multiplying the quotient Q1L with the divisor E1 and
;; adding the remainder R1L:
;; (p+ (p* Q1L E1) R1L)  ;=> [1 -3 3 -1]
;; D1 => [1 -3 3 -1]

;; The other result based on Q1H and R1H would be
;; D1/E1 = 7x^3 -4x +1 + (-8x^3/(x + 1))
;; (p+ (p* Q1H E1) R1H)  ;=> [1 -3 3 -1]
;; Which is also arithmetically correct but has the degree of the
;; numerator of the remainder larger than that of the divisor:
;; -8x^3/(x + 1)

;; Second example (X^5 - X^3 +3x -5)/(x^2 + 7)
(a D2 [-5 3 0 -1 0 1] E2 [7 0 1]) ;; dividend D2 , divisor E2
;; Calculate the quotients
(a Q2L (p/lr D2 E2))  ;=> [0 -8 0 1]
(a Q2H (p/hr D2 E2))  ;=> [-.7142857143 .4285714286 .1020408163 -.2040816327]

;; calculate the remainders by subracting the product of the quotient
;; and divisor from the dividend
(a R2L (p- D2 (p* Q2L E2))) ;=> [-5 59 0 0 0 0] ;; Low order
(a R2H (p- D2 (p* Q2H E2)))
;;   => [-8.881784197e-16 0 0 0 -.1020408163 1.204081633] ;;  high order terms

;; Thus D2/E2 with the low order remainder give: quotient Q2L => [0 -8 0 1]
;; and remainder R2L => [-5 59]

;; Checking whether the coefficients of D2 are equal to the
;; coefficients of the polynomial produced by summing over the product
;; of the quotient with the divisor (p* Q2L E2) and the remainder R2L:
;; (= D2 (p+ (p* Q2L E2) R2L) ;=> [1 1 1 1 1 1]

;; Defining the general function (p/ d e) which returns a list of the
;; quotient and the remainder that eliminates the trailing zeros of
;; the remainder:
(de p/ (d e) (let ((q (p/lr d e))) (list q (ElimT0 (p- d (p* q e))))))
;;  (p/ D2 E2) ;=> ([0 -8 0 1] [-5 59])

;; Verify polynomial division
(de vp/ (d e) (eql d (apply '(lambda (q r) (p+ (p* q e) r)) (p/ d e))))
;; (vp/ D2 E2) ;=> t 
;; (vp/ D1 E1) ;=> t
;; No remainder case 
;; (vp/ (pexp [1 1] 5) [1 1]) ;=> t
;; (p/ (pexp [1 1] 5) [1 1]) ;=> ([1 4 6 4 1] [])
;; (p+ [1 4 6 4 1] []) ;=> [1 4 6 4 1]

;; 2.2 Derivative and Integral of a Polynomial

;; Derivative

;; This is achieved succinctly simply using the definition of the
;; derivative d(N*x^M)/dx => (N*Mx^(M-1)).  To obtain the coefficients
;; of the derivative of a polynomial p we drop the leading 0 term of
;; the vector produced by multiplying the coefficients of p by their
;; corresponding exponents
(de dpdx (p) (dp 1 (* p (+ -1 (i (p p))))))
;; b ;=> [3 1 2 3]
;; (dpdx b) ;=> [1 4 9]
;; db/dx => d (3x^3 + 2x^2 + x + 3)/dx => 9x^2 + 4x + 1 => 9x^2 + 4x + 1

;; Integral

;; Similarly the vector of the coefficients of the integral of a
;; polynomial p is obtained by prefixing the integration constant c to
;; the coefficients of p divided by their respective exponent plus 1
(de spdx (p c) (cat c (/ p (i (p p)))))
;; Example using the polynomial x^5 + x^4 + x^3 + x^2 + x + 1
;; (p 6 1)                 ;=> [1 1 1 1 1 1] ;; Example polynomial
;; (dpdx (p 6 1))          ;=> [1 2 3 4 5]   ;; Derivative
;; (spdx (dpdx (p 6 1)) 1) ;=> [1 1 1 1 1 1] ;; Integral of derivative

;; Integrating the result of the derivative of b with the constant 3
;; we obtain b
;; (spdx (dpdx b) 3) ;=> [3 1 2 3]
;; (spdx [1 4  9] 3) ;=> [3 1 2 3]

;; 3. Representations

;; The subjects of mathematical analysis and computation can be
;; represented in a variety of ways, and each representation may
;; possess particular advantages. For example, a positive integer n
;; may be represented simply by n check-marks; less simply, but more
;; compactly, in Roman numerals; even less simply, but more
;; conveniently for the performance of addition and multiplication, in
;; the decimal system; and less familiarly, but more conveniently for
;; the computation of the least common multiple and the greatest
;; common divisor, in the prime decomposition scheme to be discussed
;; here.

;; Graphs, which concern connections among a collection of elements,
;; are an example of a more complex entity which possesses several
;; useful representations. For example, a simple directed graph of n
;; elements (usually called nodes) may be represented by an n by n
;; boolean matrix b (usually called an adjacency matrix) such that
;; (aref b i j) = 1 if there is a connection from node i to node
;; j. Each connection represented by a 1 in b is called an edge, and
;; the graph can also be represented by a (r '+ (rav b)) by n matrix
;; in which each row shows the nodes connected by a particular edge.

;; Functions also admit different useful representations. For example,
;; a permutation function, which yields a reordering of the elements
;; of its vector argument x, may be represented by a permutation
;; vector p such that the permutation function is simply (aref x p) ,
;; by a cycle representation which presents the structure of the
;; function more directly, by the boolean matrix
;; (a b (o '= p (i (p p)))) such that the permutation function is
;; (. '+ '* b x), or by a radix representation r which employs one of
;; the columns of the matrix
;; (+ 1 (enc (rev (i (a n (p x)))) (- (i (! n)) 1))),
;; and has the property that (| 2 (r '+ 1 (- r 1))) is the parity of
;; the permutation represented.
;; (note: The original paper has
;;        (+ 1 (enc (rev (i (a n (p x)))) (- (! n) 1)))
;;        and (| 2 (r '+ (- r 1))) which are incorrect.)

;; In order to use different representations conveniently, it is
;; important to be able to express the transformations between
;; representations clearly and precisely.  Conventional mathematical
;; notation is often deficient in this respect, and the present
;; section is devoted to developing expressions for the
;; transformations between representations useful in a variety of
;; topics: number systems, polynomials, permutations, graphs, and
;; boolean algebra.


;; 3.1 Number Systems

;; We will begin the discussion of representations with a familiar
;; example, the use of different representations of positive
;; integers and the transformations between them. Instead of the
;; positional or base-value representations commonly treated, we
;; will use prime decomposition, a representation whose interesting
;; properties make it useful in introducing the idea of logarithms
;; as well as that of number representation [6, Ch.16].

;; If p is a vector of the first (p p) primes and e is a vector of
;; non-negative integers, then e can be used to represent the number
;; (. '* 'exp p e) , and all of the integers (i (r 'c p)) can be so
;; represented. For example, (. '* 'exp [2 3 5 7] [0 0 0 0]) is 1 and
;; (. '* 'exp [2 3 5 7] [1 1 0 0]) is 6 and:

;; (a  p [ 2 3 5 7])
;; (a me (p [4 10]
;; [0 1 0 2 0 1 0 3 0 1
;;  0 0 1 0 0 1 0 0 2 0
;;  0 0 0 0 1 0 0 0 0 1
;;  0 0 0 0 0 0 1 0 0 0])
;; (. '* 'exp p  me) ;=> [1 2 3 4 5 6 7 8 9 10]

;; The similarity to logarithms can be seen in the identity:
;;     (r '* (. '* 'exp  p me)) <=> (. '* 'exp p (r '+ me))
;; which may be used to effect multiplication by addition.

;; Moreover, if we define gcd and lcm to give the greatest common
;; divisor and least common multiple of elements of vector arguments,
;; then:

;;       (gcd (. '* 'exp p me) <=> (. '* 'exp p (r 'f me))
;;       (lcm (. '* 'exp p me) <=> (. '* 'exp p (r 'c me))

;; (a  me  (p [4 3] [2 1 0 3 1 2 2 2 0 1 2 3]))
;; me => [2 1 0        
;;        3 1 2    
;;        2 2 0 
;;        1 2 3]
;; (a v (. '* 'exp p me))
;; v ;=> [18900 7350 3087]
;; (gcd v)                 ;=> 21
;; (. '* 'exp p (r 'f me)) ;=> 21
;; (lcm v)                 ;=> 926100		    
;; (. '* 'exp p (r 'c me)) ;=> 926100
 

;; In defining the function gcd , we will use the operator (k) with a
;; boolean argument b as in (k b v). It produces the compression
;; function which selects elements from its second argument v
;; according to the ones in b . For example, (k [1 0 1 0 1] (i 5)) is
;; [1 3 5]. Moreover, the function (k b m) applied to a matrix argument
;; compresses rows (thus selecting certain columns), and the function
;; (k 1 b m) compresses columns to select rows. Thus:

(de gcd (w) (let (m r) (if (ge 1 (p (a r (k (<> w 0) w))))
			   (r '+ r)
			 (gcd (cat (a m (r 'f r)) (| m r))))))

(de lcm (w) (let (x) (if (zerop (p w)) 1
		       (/ (r '* (a x (cat (tk 1 w) (lcm (dp 1 w))))) (gcd x)))))

;; The transformation to the value of a number from its prime
;; decomposition representation (vfr) and the inverse transformation
;; to the representation from the value (rpv) are given by:

(de vfr (a w) (. '* 'exp a w))
(de rfv (a w) (let (d) (if (onep (r '^ (~ (a d (= 0 (| a w)))))) d
			 (+ d (rfv a (/ w (. '* 'exp a d)))))))

;; For example:
;; (a p [2 3 5 7])
;; (vfr p [2 1 3 1]) ;=> 10500
;; (rfv p  10500)    ;=> [2 1 3 1]

;;3.2 Polynomials

;; Section 2 introduced two representations of a polynomial on a
;; scalar argument x , the first in terms of a vector of coefficients
;; c (that is, (r '+ (* c (exp x (+ -1 (i (p c)))))), and the second
;; in terms of its roots r (that is, (r '* (- x r)). The coefficient
;; representation is convenient for adding polynomials (+ c d) and for
;; obtaining derivatives (* (dp 1 c) (- (i (p c)) 1)). The root
;; representation is convenient for other purposes, including
;; multiplication which is given by (cat r1 r2)


;; We will now develop a function cfr (Coefficients from Roots) which
;; transforms a roots representation to an equivalent coefficient
;; representation, and an inverse function rfc . The development will
;; be informal; a formal derivation of cfr appears in Section 4.

;; The expression for cfr will be based on Newton's symmetric
;; functions, which yield the coefficients as sums over certain of the
;; products over all subsets of the arithmetic negation (that is, (- r))
;; of the roots r . For example, the coefficient of the constant term
;; is given by (r '* (- r)) , the product over the entire set, and the
;; coefficient of the next term is a sum of the products over the
;; elements of -r taken (- (p r) 1) at a time.

;; The function T defined by A.2 can be used to give the products over
;; all subsets as follows:

;; (a p (. '* 'exp (- r) (a m (T (p r)))))  

;; The elements of p summed to produce a given coefficient depend upon
;; the number of elements of r excluded from the particular product,
;; that is, upon (r '+ 1 (~ m)) , the sum of the columns of the complement of
;; the boolean "subset" matrix (a m (T (p  r))) .

;; The summation over p may therefore be expressed as
;; (. '+ '* (o '= (cat 0 (i (p r))) (r '+ 1 (~ m))) p) , and the
;; complete expression for the coefficients c becomes:

;;  (a c  (. '+ '* (o '= (cat 0 (i (p r))) (r '+ 1 (~ (a m (T (p r))))))
;;       	   (. '* 'exp (- r)  m)))
 
;; For example, if (a r [2 3 5]) and (a m (T (p r))) , then
;;  m ;=>                   
;; [0 0 0 0 1 1 1 1         
;;  0 0 1 1 0 0 1 1         
;;  0 1 0 1 0 1 0 1]
;;
;; (. '* 'exp (- r) m) ;=> [1 -5 -3 15 -2 10 6 -30]
;;
;; (r '+ 1 (~ m)) ;=> [3 2 2 1 2 1 1 0]
;;
;; (o '= (cat 0 (i (p r))) (r '+ 1 (~ m))) ;=>
;; [0 0 0 0 0 0 0 1	     
;;  0 0 0 1 0 1 1 0	     
;;  0 1 1 0 1 0 0 0	     
;;  1 0 0 0 0 0 0 0]
;;
;; (. ' + '* (o '= (cat 0 (i (p r))) (r '+ 1 (~ (a m (T (p r))))))
;;           (. '* 'exp (- r)  m)) ;=> [-30 31 -10 1]

;; The function cfr which produces the coefficients from the roots may
;; therefore be defined and used as follows:
(de cfr (r) (let ((m (T (p r))))                                     ;; C.1
	      (. ' + '* (o '= (cat 0 (i (p r))) (r '+ 1 (~  m)))
		   (. '* 'exp (- r)  m))))
;; (cfr [2 3 5] ;=> [-30 31 -10 1]
;; (poly (cfr [2 3 5] (a x (i 8)))) ;=> [-8 0 0 -2 0 12 40 90] ;; coeff rep
;; (r '* (o '- x [2 3 5]))          ;=> [-8 0 0 -2 0 12 40 90] ;; root rep

;; The inverse transformation rfc is more difficult, but can be
;; expressed as a successive approximation scheme as follows:

(de rfc (w) (g (- (i (p (dp 1 w))) 1) w))
(de g (a w) (if (gt tol (r 'c (| (a z (step a w))))) (- a z) (g (- a z) w)))
(de step (a w) (. '+ '* (cpxm/ (. '* 'exp (o '- a a) (o '<> (a i (i (p a))) i)))
		  (. '+ '* (o 'exp a (- (i (p w)) 1)) w)))
(require 'mat) ;; for complex matrix inversion (cpxm/)
;; (a c (cfr [2 3 5 7]))
;; c ;=> [210 -247 101 -17 1]
;; (a tol 1e-8)
;; (rfc c) ;=> [7 5 2 3]

;; The order of the roots in the result is, of course, immaterial. The
;; final element of any argument of rfc must be 1 , since any
;; polynomial equivalent to (r '* (- x r)) must necessarily have a
;; coefficient of 1 for the high order term.

;; The foregoing definition of rfc applies only to coefficients of
;; polynomials whose roots are all real. The left argument of g in rfc
;; provides (usually satisfactory) initial approximations to the
;; roots, but in the general case some at least must be complex. The
;; following example, using the roots of unity as the initial
;; approximation, was executed on an alps system which handles complex
;; numbers:

;; (re (cfr [1j1 1j-1 1j2 1j-2])) ;=> [10 -14 11 -4 1]

(de rfc2 (w) (g (exp (% (* 0j1 (/ (- (i (a n (p (dp 1 w)))) 1) n)))) w)) ;;C.2

;;  (rfc2 (cfr [1j1 1j-1 1j2 1j-2])) ;=> [1j-1 1j2 1j1 1j-2]

;; The monadic function (%) in rfc2 multiplies its argument by pi.

;; In Newton’s method for the root of a scalar function f, the next
;; approximation is given by (a a (/ (- a (f a)) (df a))), where df is
;; the derivative of f. The function step is the generalization of
;; Newton’s method to the case where f is a vector function of a
;; vector. It is of the form (. '+ '* (m/ m) b), where b is the value
;; of the polynomial with coefficients w, the original argument of
;; rfc, evaluated at a , the current approximation to the roots;
;; analysis similar to that used to derive B.3 shows that m is the
;; matrix of derivatives of a polynomial with roots a , the
;; derivatives being evaluated at a .  Examination of the expression
;; for m shows that its off-diagonal elements are all zero, and the
;; expression (. '+ '* (m/ m) b) may therefore be replaced by (/ b d),
;; where d is the vector of diagonal elements of m. Since (dp {i j} n)
;; drops i rows and j columns from a matrix n, the vector d may be
;; expressed as (r '* (dp [0 1] (rot (- (i (p a)) 1) (o '- a a)))) the
;; definition of the function step may therefore be replaced by the
;; more efficient definition:

(de step2 (a w) (/ (. '+ '* (o 'exp a (- (i (p w)) 1)) w)    ;; C.3
		  (r '* (dp [0 1] (rot (- (i (p a)) 1) (o '- a a))))))

;; This last is the elegant method of Kerner [7]. Using starting
;; values given by the left argument of g in C.2, it converges in
;; seven steps (with a tolerance (a tol 1e08)) for the sixth-order
;; example given by Kerner.



;; 3.4 Directed Graphs

;; A simple directed graph is defined by a set of k nodes and a set
;; of directed connections from one to another of pairs of the
;; nodes. The directed connections may be conveniently represented
;; by a k by k boolean connection matrix c in which
;; (onep (aref c ij)) denotes a connection from the ith node to the jth.

;; For example, if the four nodes of a graph are represented by
;; (a n "qrst") , and if there are connections from node s to node q ,
;; from r to t , and from t to q , then the corresponding connection
;; matrix is given by:

;;       0 0 0 0
;;       0 0 0 1
;;       1 0 0 0
;;       1 0 0 0

;; This matrix can be generated with (a m " tqq") specifying no
;; connection from q , connections from r to t, s to q and t to q
;; (o '= m n) ;=>
;; [0 0 0 0
;;  0 0 0 1
;;  1 0 0 0
;;  1 0 0 0]

;; A connection from a node to itself (called a self-loop) is not
;; permitted, and the diagonal of a connection matrix must therefore
;; be zero.

;; If p is any permutation vector of order (p n) ,
;; then (a n1 (aref n p)) is a reordering of the nodes, and the corresponding
;; connection matrix is given by (aref c p p) . We may (and will)
;; without loss of generality use the numeric labels (i (p n)) for the
;; nodes, because if n is any arbitrary vector of names for the
;; nodes and l is any list of numeric labels, then the expression
;; (a q (aref n l)) gives the corresponding list of names and, conversely,
;; (ind n q) gives the list l of numeric labels.

;; The connection matrix c is convenient for expressing many useful
;; functions on a graph. For example, (r '+ c) gives the out-degrees
;; of the nodes, (r '+ 1 c) gives the in-degrees, (r '+ (rav c)) gives
;; the number of connections or edges, (tr c) gives a related graph
;; with the directions of edges reversed, and (v c (tr c)) gives a
;; related "symmetric" or "undirected" graph. Moreover, if we use the
;; boolean vector (a b (r 'v (o ' = (i (tk 1 (p c)) 1)))) to represent
;; the list of nodes l , then (. 'v '^ b c) gives the boolean vector
;; which represents the set of nodes directly reachable from the set
;; b.  Consequently, (. 'v '^ c c) gives the connections for paths of
;; length two in the graph c , and (v c (. 'v '^ c c)) gives
;; connections for paths of length one or two. This leads to the
;; following function for the transitive closure of a graph, which
;; gives all connections through paths of any length:

(de tc (w) (let (z) (if (onep (r '^ (rav (= w (a z (v w (. 'v '^ w w))))))) z
		      (tc z))))
