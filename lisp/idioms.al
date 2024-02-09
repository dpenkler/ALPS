;;;-*- mode: emacs-lisp -*-
;;; An alps rendition of a selection of APL Idioms from Perliss &
;;; Rugaber APL Idiom List Research Report #87 (Yale) April 1977
;;; with various supplements.
;;
;; By studying the idioms one quickly acquires a good understanding of
;; alps programming techniques. Many of the idioms are given here as
;; defined functions in a loadable file so that they can be tried
;; immediately without setting variables. In the report most are
;; presented as code fragments which correspond to the body of the
;; defined functions. In general, when programming, the fragments
;; should be combined using functional composition as opposed to using
;; the named functions defined here. The idioms, being short, are
;; almost always clearer than invoking the named function (see
;; Perlism 94 below).
;;

;;
;; Perliss is one of the few early APL'ers that also worked with LISP.
;; He has left us with some precious Perlisms: 
;;
;; 3.  Syntactic sugar causes cancer of the semicolon
;;
;; 49. Giving up on assembly language was the apple in our Garden of
;;     Eden: Languages whose use squanders machine cycles are
;;     sinful. The LISP machine now permits LISP programmers to
;;     abandon bra and fig-leaf.
;;
;; 55. A LISP programmer knows the value of everything, but the cost
;;     of nothing.
;;
;; 94. Interfaces keep things tidy, but don't accelerate growth:
;;     Functions do.

;;
;; Section 5.0 Annotated examples
;;

;; 0. Remove consecutive repetitions in a vector V
;; (a W (k (cat (dp -1 (<> V (rot 1 V))) 1) V))
;;
;; As a gentle introduction to the idiom list this "initiation" idiom
;; is presented.
;;
;; Commentary: W is to be set to a copy of the vector V with all
;; repetitions of consecutive elements removed.  The function compress
;; (k) will do the removal job: (k S V).  Compress selects only those
;; elements in V for which S has a 1 in the corresponding position,
;; compressing out the elements in V for which S has the value 0. S
;; must have as many elements as the vector it compresses.  For
;; example: (k [1 0 1 0 1] [1 2 3 4 5]) => [1 3 5] In order to produce
;; the selector that will have 0's in the place of the repetitions,
;; that is where adjacent elements are equal, compare a shifted
;; version of the vector with itself: (<> V (rot 1 V)). (<>) is the
;; predicate not equal which will produce 1's wherever adjacent
;; elements are not equal and 0's elsewhere.  The (rot 1 V) function
;; rotates the elements of V to the left by one (1) position putting
;; the first element of V at the end.
;;
;; Assigning to V the string "Harry": (a V "Harry")
;; then (rot 1 V) => "arryH" and  (<> V (rot 1 V)) => [1 1 0 1 1].
;; So using this as the selector:
;; (k (<> V (rot 1 V)) V) => (k (<> "Harry" "arryH") "Harry")
;;                        => (k [1 1 0 1 1] "Harry")
;;                        => "Hary".
;; Looks OK, but what about trying this with "rambbler" ?
;; Assigning to the symbol V the value "rambbler": (a V "rambbler"),
;; now (k (<> V (rot 1 V)) V) => (k (<> "rambbler" "ambblerr") "rambbler")
;;                            => (k [1 1 1 0 1 1 1 0] "rambbler")
;;                            => "ramble", which is incorrect.
;; It has dropped the last "r" which is not a repetition of the adjacent
;; character "e". So, to ignore the last comparison, drop the last
;; element of the selector with (dp -1 (<> V (rot 1 V))) and catenate
;; a 1 on at the end with  (cat (dp -1 (<> V (rot 1 V))) 1) so that the
;; selector has the same number of elements as the vector V.
;; Now (k (cat (dp -1 (<> V          (rot 1 V) )) 1) V)
;; =>  (k (cat (dp -1 (<> "rambbler" "ambblerr")) 1) "rambbler")
;; =>  (k (cat (dp -1 [1 1 1 0 1 1 1 0]) 1) "rambbler")
;; =>  (k (cat        [1 1 1 0 1 1 1 ] 1) "rambbler")
;; =>  (k             [1 1 1 0 1 1 1 1] "rambbler")
;; =>  "rambler"
;; To define this idiom as a function with the name RemRep:
(defun RemRep (V) (k (cat (dp -1 (<> V (rot 1 V))) 1) V))
;; Applying the newly defined function to the string "rrrambblerr":
;; (RemRep "rrrambblerr")  ;=> "rambler"
;; See also RedBlank in 5.c below.

;; 1. Remove duplicates from vector V and assign the result to W:
;; (a W (k (= (ind V V) (i (p V))) V))
;;
;; Commentary: W is to be set to a copy of V with duplicates removed.
;; To demonstrate this, consider the Kth element of V: (aref V K).
;; There are two possibilities.
;; If this is the first occurrence of the Kth element, that is,
;; if there is no J less than K such that the Jth element of V
;; is equal to the Kth element, (= (aref V J) (aref V K)) => 1,
;; then (ind V (aref V K)) will be K, where dyadic (ind V (aref V K))) is the
;; index function that returns the index of the first occurrence of
;; (aref V K) in V.
;; (aref (i (p V)) K) will of course be K as (i (p V)) is the sequence
;; of integers from 1 to the number of elements in V. Monadic (i N)
;; generates the integers from 1 to N and the monadic shape function (p V)
;; returns the number of elements in the vector V. Thus in this case the
;; Kth element of (= (ind V V) (i (p V))) will be 1.
;; This is the selector argument S for the compress function (k S V). 
;; Now if, on the other hand, there exists a J less than K such that
;; (= (aref V J) (aref V K)) => 1, then (ind V (aref V K)) will be J.
;; (aref (i (p V)) K) will, of course, still be K.
;; Because J does not equal K, the Kth element of the selector will be 0
;; and (aref V K) will not be included in W, the result of the compression.
;; Hence for each K, 1 <= K <= (p V), (aref V K) is included in W
;; if and only if it is the first (leftmost) occurrence of its value in V.
;; W will contain each of the different elements of V and it will
;; contain no duplicates. Therefore, W is V with duplicates removed.
;; Here we define the function RemDup that applies this idiom.
(defun RemDup (V) (k (= (ind V V) (i (p V))) V))
;; (RemDup "a quick brown fox jumped over the lazy dogs") =>
;; "a quickbrownfxjmpedvthlzygs"

;; Another variant of this is idiom is:
;; (a W (k (tr [1 1] (s '< -1 -1 (o '= V V))) V))
;;
;; This variant uses more advanced APL functions:
;; (tr [1 1] M) is the transpose function which given the tranposition
;; pattern [1 1] extracts the diagonal elements from the matrix M.
;; (s F Axis Direction A) scans the array A along the dimension specified
;; by Axis in the given Direction applying the function F to successive
;; elements. The -1 for the Axis argument selects the last axis and the
;; -1 for Direction indicates that the scan is to occur from right to left,
;; that is, starting from the last element along the axis and progressing
;; to the first.
;; (o F A1 A2) performs the outer product operation by applying the function
;; F to each pair of elements chosen from A1 and A2. Thus the result will have
;; as many elements as the product of the number of elements in A1 and A2.
;; So in the idiom (o '= V V) creates a matrix with a 1 everywhere where
;; elements in V match. Let, for example, (a V "tart"). Then
;; (o '= V V) =>
;; [1 0 0 1
;;  0 1 0 0
;;  0 0 1 0
;;  1 0 0 1]
;; The diagonal is of course all ones but the duplicated "t" places a 1 in
;; the top right hand and bottom lefthand corners.
;; Now (s '< -1 -1 (o '= V V)) leaves only the leftmost 1 in each row:
;; (s '< -1 -1 (o '= V V))
;; [1 0 0 0
;;  0 1 0 0
;;  0 0 1 0
;;  1 0 0 0]
;; The transpose function pulls out the diagonal:
;; (tr [1 1] (s '< -1 -1 (o '= V V))) => [1 1 1 0]
;; and the compressor function selects only elements with a corresponding 1
;; (k [1 1 1 0] V) => "tar"
;; This idiom directly generalises to higher dimensions such as the 
;; case for a rank 3 array A:
;; (a W (k 1 (tr [1 1] (s '< (r '^ (tr [1 3 3 2] (. '^ '= A (tr A)))))) A))
;; For example, let (a A (k 1 [2 1] (p [2 3 4] (i 24)))), 
;; where we duplicate the first plane of a 2 x 3 x 4 array with (k 1 [2 1] A)
;; to give a 3 x 3 x 4 array A:
;; (p A) => [3 3 4]
;; A =>
;; [ 1  2  3  4
;;   5  6  7  8
;;   9 10 11 12
;; 
;;   1  2  3  4
;;   5  6  7  8
;;   9 10 11 12
;; 
;;  13 14 15 16
;;  17 18 19 20
;;  21 22 23 24]
;;
;; (k 1 (tr [1 1] (s '< (r '^ (tr [1 3 3 2] (. '^ '= A (tr A)))))) A) =>
;; [ 1  2  3  4
;;   5  6  7  8
;;   9 10 11 12
;; 
;;  13 14 15 16
;;  17 18 19 20
;;  21 22 23 24]
;;
;; Notice how the duplicated plane has been compressed out in the result.
;; This generalisation uses the reduction (r) and inner product (.) operators.
;; We capture the simpler one dimensional version of this idiom in RemDupII
(defun RemDupII  (V) (k (tr [1 1] (s '< -1 -1 (o '= V V))) V))

;; A third variant can be used where the length of V is large compared
;; to its span, (the difference between its largest and smallest elements), 
;; i.e. (p V) is large compared to (- (r 'c V) (r 'f V)) which is the same
;; as simply saying that V has a lot of duplicates.
;; We define it as RemDupIII with out further comments.
(defun RemDupIII (V) ; Integers only
  (let ((R (i {(r 'f V) (r 'c V)}))) (k (elt R V) R)))

;; Here are all three variants in action:
;; (a S "a quick brown fox jumped over the lazy dogs")
;; (RemDup S) => "a quickbrownfxjmpedvthlzygs"
;; (RemDupII S) => "a quickbrownfxjmpedvthlzygs"
;; (RemDupIII [10 5 2 9 8 3 7 8 8 4]) ;=> [2 3 4 5 7 8 9 10]
;; Note the first two keep the elements in order, the last sorts them.
;; To use RemDupIII with character vectors convert the characters
;; to the numbers corresponding to their ASCII value and back again
;; using the functions (num) to convert characters to their ASCII value
;; and (chr) to convert the numbers back to characters.
;; (chr (RemDupIII (num S))) => " abcdefghijklmnopqrstuvwxyz"
(defun TRemDup () "Test RemDup"
       (let* ((S "a quick brown fox jumped over the lazy dogs")
	      (R (RemDup S))
	      (Q (chr (RemDupIII (num S)))))
	 (if (and (eql R (RemDupII S)) (eql Q (aref R (gup R))))
	     Q) ;; return Q
	 ))
;; (TRemDup) ;=> " abcdefghijklmnopqrstuvwxyz"
;; In the last test between Q and R we sort R into increasing order with
;; the grade up function (gup) which returns the indices of its argument
;; ordered by increasing value of the elements in the argument. To obtain
;; the vector the argument is subscripted by the ordered indeces:
;; (aref R (gup R)). See also 9.3

;; 2. Expand
(defun Expand (A B) "Expand A adding B"
  (let ((D (c (dp 1 (p A)) (p B)))) ; new size of array entry
    (cat 1 (tk (cat (tally  A) D) A) (tk D B))))
;; A is the array to be expanded and B is the new entry to be catenated 
;; along the first axis of A. B must be of rank one less than A, i.e.
;; (- (rank A) (rank B)) => 1
;; If either A or B is larger than the other along any axis, then the
;; length of that axis must be expanded to make them match.
;; D will be the shape vector containing the larger of the elements of the
;; shape vectors of A and B dropping the first element of A's shape vector.
;; This is accomplished with ceiling (c) and drop (dp): (c (dp 1 (p A)) (p B))
;; (cat (tally  A) D) The function (tally  A) returns length of the first
;; dimension of A and cat "catenates" it onto D to give the desired shape of the
;; result. Let (a PR (cat (tally  A) D)). Now (tk PR A) will
;; for each dimension K "take" (aref PR K) elements from A. If more elements
;; are taken than are in the dimension, fill elements of the appropriate type
;; are inserted. 0's for numbers and blanks for characters. For example:
;; (tk 10 "Hello") => "Hello     "
;; (tk 10 (i 5))   => [1 2 3 4 5 0 0 0 0 0]
;; Similarly with (tk D B) at the end of the idiom.
;; Finally the (cat (tk PR A) (tk D B) 1) catenates the take from B along the
;; first axis with the take from A.
;;  (a A (p [4 6] "Hello Polly Fred  Joe   ") B "Sebastien")
;; A =>
;; "Hello 
;;  Polly 
;;  Fred  
;;  Joe   "
;; B => "Sebastien"
;; (p A) => [4 6]
;; (p B) => [9]
;; (- (rank A) (rank B)) =>  1
;; (Expand A B) => 
;; "Hello    
;;  Polly    
;;  Fred     
;;  Joe      
;;  Sebastien"
;; (p (Expand (p [4 6] "Hello Polly Fred  Joe   ") "Sebastien")) ;=> [5 9]

;; 3. Bar Graph
;; (o '>= V (i (r 'c V)))
;; Given V a vector of integers, returns boolean matrix with V[I] ones in row I
;; (r 'c V) finds the highest value in V. Uses reduction with the ceiling function.
;; To draw an actual bar graph:
(defun BarGraph (V) (aref " #" (+ #IO (rev 1 (tr (o '>= V (i (r 'c V))))))))
;;  (BarGraph [1 2 3 4 6 4 3 2 1])
;; "    #    
;;      #    
;;     ###   
;;    #####  
;;   ####### 
;;  #########"


;; Another application 
(defun MakeArray (S V) "Makes an array from S separated subvectors in vector V"
       (let* ((#IO 1)
	      (A (k (= S (cat V S)) (i (+ 1 (p V))))); positions of S
	      (L (- A 1 (cat 0 (dp -1 A)))) ; lengths of subvectors
	      (Y (k (<> V S) V))            ; remove blanks idiom
	      (H (o '>= L (i (r 'c L)))))   ; Bar graph idiom
	 (p (p H) (ex (rav H) Y))))         ; bar graph used as expansion mask
;; (MakeArray " " "The boy stood upon the burning deck") ;; S is a blank here
;; "The    
;;  boy    
;;  stood  
;;  upon   
;;  the    
;;  burning
;;  deck   "

;; 4. Make a vector of integers from A to B
(defun TO (A B) (cat A (+ A (* (* (- B A)) (i (| (- B A)))))))
;; (TO 2 5)  ;=> [2 3 4 5]
;; (TO 10 5) ;=> [10 9 8 7 6 5]
;; We can also just use (i [2 5]) and (i [10 5])

;; 5. Blank removal
;;    a.  Eliminate leading blanks
(defun ElimLB (V) (k (s 'v (<> " " V)) V))
;; (ElimLB "   This is the end !   ") ;=> "This is the end !   "

;;    b. Eliminate trailing blanks
(defun ElimTB (V) (k (rev (s 'v (<> " " (rev V)))) V))
;; (ElimTB "   This is the end !   ") ;=> "   This is the end !"

;;    c. Multiple blanks reduced to a single blank
(defun RedBlank (V) (let ((A (<> V " "))) (k (v A (dp 1 (rot 1 (cat 1 A)))) V)))
;; (RedBlank "   This    is    the       end  !     ") ;=> " This is the end ! "

;; 6. Character-number conversions
;; a. Compute the number of digits required to express a number:
(defun NDigs (N) (f (+ 1 (l 10 (| (+ N (= N 0)))))))
;; (NDigs 12345) ;=> 5

;; b. Convert a positive numbers into a vector
(defun MkVec (N) (enc (p (NDigs (r 'c N)) 10) N))
;; (eql (MkVec 12345) (i 5)) ;=> t

;; c. Convert an integer into a character string
(defun MkStr (N) (aref "0123456789" (+ #IO (MkVec N))))
;; (MkStr 123450) ;=> "123450"

;; d. Convert a string of digits, represented as characters, into a vector
(defun StoV (S) (- (ind "0123456789" S) 1))
;; (StoV "4321")  ;=> [4 3 2 1]

;; e. Encode a vector of positive integers as an integer
(defun EncV (V) (dec (exp 10 (f (+ 1 (l 10 (r 'c V))))) V))
;; (EncV [4 3 2 1 1 2 3 4]) ;=> 43211234

;; f. Make an array in which each row contains the character
;; representation of one element of the input vector V
(defun Arep (V)
  (aref "0123456789" (+ 1 (tr (enc (p (f (+ 1 (l 10 (r 'c V)))) 10) V)))))
;;(Arep [1 2 19 123]) =>
;;"001
;; 002
;; 019
;; 123"
;; (eql (Arep [1 2 19 123]) (p [4 3] "001002019123")) ;=> t

;; g. Blank out leading zeros in a string w
(defun blzs (w) (let ((s (s 'v -1 -1 (<> w "0")))) (k (+ s (- s 1)) (k s w)))) 
;; (blzs "00010000") ;=> "   10000"

;; 7. AMTOAL and ALTOAM: Let A be a multi-dimensioned array, stored in
;; linear memory in row major order. If the memory addresses of some
;; entries are given in M, and if a list of subscript positions, S,
;; for those same elements is to be computed the following idiom
;; can be used: (a S (+ 1 (enc (p A) (- M 1)))) where the columns of S
;; contain the subscripts. Going the other way converts sbscripts into
;; ravel (linear) order positioins: (a M (+ 1 (dec (p A) (- S 1))))
;; Viewed from another perspective, this is similar to the problem of
;; converting a directed graph, AM, represented in adjacency matrix
;; form, into a list of its arcs. An adjacency matrix has a one at the
;; intersection of row I and column J wherever I is connected to J and
;; zeros elsewhere. (AMTOAL):
(defun AMTOAL (AM) (+ 1 (enc (p AM) (- (k (rav AM) (i (p (rav AM)))) 1))))
;; Going in the other direction is almost as easy (ALTOAM):
(defun ALTOAM (AL)
  (let ((A (r 'c (rav AL))))
    (p (cat A A) (elt (i (* A A)) (+ 1 (dec (cat A A) (- AL 1)))))))
;; (LTM 5)  => ;; Generate a 5x5 lower triangular matrix. See 14.19
;; [1 0 0 0 0
;;  1 1 0 0 0
;;  1 1 1 0 0
;;  1 1 1 1 0
;;  1 1 1 1 1]
;; (eql (ALTOAM (AMTOAL (LTM 5))) (LTM 5)) ;=> t
;;
;; (setq A (AMTOAL (LTM 5)))
;; [1 2 2 3 3 3 4 4 4 4 5 5 5 5 5
;;  1 1 2 1 2 3 1 2 3 4 1 2 3 4 5]
;; (ALTOAM A)
;; [1 0 0 0 0
;;  1 1 0 0 0
;;  1 1 1 0 0
;;  1 1 1 1 0
;;  1 1 1 1 1]
;;
;; An alternative approach for ALTOAM is to use the (iset) primitive
;; (a B (p (r 'c A) 0)) assigns to B an NxM matrix of zeros where N
;; is the highest column index and M the highest row index in A
;; where A is the list of subscripts for the one elements in the matrix.
;; To set the elements in B to 1 with subscripts in A: (iset B 1 A)
;; (iset) is an extension to the standard apl primitives.
;; (setq A (AMTOAL (LTM 5)))
;; [1 2 2 3 3 3 4 4 4 4 5 5 5 5 5
;;  1 1 2 1 2 3 1 2 3 4 1 2 3 4 5]
;; (setq B (p (r 'c A) 0))
;; [0 0 0 0 0
;;  0 0 0 0 0
;;  0 0 0 0 0
;;  0 0 0 0 0
;;  0 0 0 0 0]
;; (iset B 1 A) => 1
;; B =>
;; [1 0 0 0 0
;;  1 1 0 0 0
;;  1 1 1 0 0
;;  1 1 1 1 0
;;  1 1 1 1 1]

;;
;; Section 6.0 Predicates
;;

;; Alps can be used to make assertions about alps programs. The
;; inclusion of logical vectors and operators in the language from
;; APL, in effect, provides the expressibility of the first order
;; predicate calculus in addition to the lambda calculus of lisp. In
;; particular, "for all" in a boolean vector, B, is expressed by (r '^ B)
;; and "there exists" with (r 'v B) where ^ and v are the boolean
;; logical connectives "and" and "or" respectively. This section gives
;; further evidence of this power.

;; 1. Example of input specifications given in alps. This problem
;; comes originally from the article "An illustration of the current
;; ideas on the derivation of correctness proofs and correct
;; programs," D. Gries, IEEE Transactions on Software Engineering, Vol
;; SE-2, No. 4, Dec 1976. The problem is to justify a line of text given
;; the following input parameters.
;;
;; Z is the current line number
;; N is the number of words on the line, each word separated by a single blank.
;; S is the number of extra blanks at the end of the line.
;; B on input is the vector of starting positions of words on the line.
;; the output is a vector of starting positions of words on the justified line.
;;
;; Even numbered lines are to have extra blanks separating words toward the
;; left end of the line, odd numbered lines toward the right. This input
;; specification for this problem can be stated by the following alps
;; expressions:
;; (a T {B Z (- N 1) (- S 1)}) and  (r '^ (cat (> T 0) (= T (f T))))
;; where T is a vector containing column positions B, line number Z, number
;; of words on the line minus 1 (- N 1), and number of blanks terminating
;; the line minus 1 (- S 1).
;; The specifications stated in the second expression is that all the
;; numbers in T must be integers (= T (f T)), and greater than 0
;; (> T 0).
;;
;; A solution to this problem is given by the function annotated below:

(defun JUSTIFY (Z N S B)
  (prog (A M F G C)
	(if (or (zerop S) (ge 0 (a M (- N 1)))) (return B))
	(a A (+ S M)
	   C (f (/ A M))
	   F (tk M (cat (p (- A (* C M)) 1) (p M 0)))
	   G (+ (p M C) (* F (~ (| 2 Z))) (* (| 2 Z) (rev F))))
	(+ (+ 1 (- B (i N))) (s '+ (cat 0 G)))))

;; Commentary: The function is only executed if N and S are at least one,
;; otherwise the input line is already justified. A is the total number of
;; blanks on the line. C is the minimum number of blanks that will separate
;; any adjacent pair of words. F indicates how the unallocated blanks should
;; be distributed among the other  words, depending on the parity of Z.
;; Together they form G, the actual number of blanks separating adjacent words.
;; Finally, the result is computed by taking the value of B, subtracting
;; the number of blanks to the left of each word in the input string, and
;; adding the number to the left in the output string. The scan operator
;; (s '+ V) is used to compute this running count.
;;
;; Given an input string V the input values to (JUSTIFY Z N S B) can be
;; obtained as follows:
;; Assume Z, the line number is given.
;; V1, a temporary vector is the input string without the trailing blanks:
;; (a V1 (k (rev (s 'v (<> " " (rev V)))) V)) see ElimTB.
;; N, the number of words: (a N (+ 1 (r '+ (= " " V1)))) where
;; the number of words is the number of blanks in V1 plus 1.
;; S, the number of blanks at the end of the line:
;; (a S (r '+ (s '^ (= " " (rev V))))) which is the sum the number of
;; consecutive blanks at the end of the line.
;; B, the starting positions of the words on the line:
;; (a B (cat 1 (+ 1 (k (elt V1 " ") (i (p V1)))))) where (elt V1 " ") creates a
;; vector with 1's where there are blanks and 0's elsewhere. Using this to
;; compress the index vector of the string without trailing blanks gives
;; the position of each blank separating a word in the string.
;; By adding one to each of these one obtains the starting positions of
;; each word excepting the first, which has the starting position 1
;; prepended to the list.
;;
;; To simplify experimentation define a function to invoke JUSTIFY given
;; a string V to justify and a line number Z:

(require 'prims) ;; need this for the definitions< of some additional primitives.

(defun SJust (V Z)
  (let* ((V1 (k (rev (s 'v (<> " " (rev V)))) V))
	 (N (+ 1 (r '+ (= " " V1))))
	 (S (r '+ (s '^ (= " " (rev V)))))
	 (B (cat 1 (+ 1 (k (elt V1 " ") (i (p V1)))))))
    (princl (fmt "N= " N " S= " S " B= " B))
    (JUSTIFY Z N S B)))

;; Assign to V a string with 9 blanks at the end:
;; (a V "There are more things in heaven and earth, Horatio,         ")
;; (princl (fmt "JUSTIFIED => " (SJust V 1))) => t
;; N= 9 S= 9 B= [1 7 11 16 23 26 33 37 44]
;; JUSTIFIED => [1 8 13 19 27 31 39 44 53]
;;
;; In order to take a string with an arbitrary number of leading,
;; trailing, or inter-word blanks the idioms for eliminating leading,
;; trailing and redundant blanks can be used. To illustrate the
;; practical use of JUSTIFY using these and other idioms the following
;; session log is given.
;;
;; Assign an arbitrary string to be justified to V
;; (a V "  There are more things    in heaven and earth, Horatio,    ")
;; Use the defined idioms to remove the blanks to obtain an input string
;; that satisfies the specification for JUSTIFY:
;; (ElimLB (ElimTB (RedBlank V)))
;; => "There are more things in heaven and earth, Horatio,"
;; Examine the definition of each of the idioms
;; ElimLB => (lambda (V) (k (s 'v (<> " " V)) V))
;; ElimTB => (lambda (V) (k (rev (s 'v (<> " " (rev V)))) V))
;; RedBlank 
;; => (lambda (V) (let ((A (<> V " "))) (k (v A (dp 1 (rot 1 (cat 1 A)))) V)))
;; Assign the blank compressor, that occurs in each of the idioms, to T:
;; (a T (<> " " V))
;; Combine the blank removal idioms:
;; (k (^ (s 'v T) (rev (s 'v (rev T))) (v T (dp 1 (rot 1 (cat 1 T))))) V)
;; => "There are more things in heaven and earth, Horatio,"
;; (a V1 #RS)    ; assign the previous result to V1
;; (setq N (+ 1 (r '+ (= " " V1))))  ; count the number of words
;; => 9
;; (setq S (- (p V) (p V1)))  ; the number of removed blanks
;; => [9]
;; (setq B (cat 1 (+ 1 (k (elt V1 " ") (i (p V1)))))) ; starting positions
;; => [1 7 11 16 23 26 33 37 44]
;; (setq R (JUSTIFY 1 N S B)) => [1 8 13 19 27 31 39 44 53]
;; Calculate the distribution of blanks by subtracting the starting positions
;; and adjusting for single blanks. This is effectively recomputing the value
;; of G in JUSTIFY. Note the use of N-wise reduce with N=-2 to calculate the
;; running difference.
;; (setq EB (r '- 1 -2 (- R (+ 1 (- B (i N)))))) => [2 2 2 2 2 2 2 3]
;; (a BP (= " " V1))  ;; blank positions in compressed string
;; Replicate blanks in V1 according to the distribution EB
;; See also the gap opener idiom in 15.12
;; (k (+ (~ BP) (ex BP EB)) V1)
;; => "There  are  more  things  in  heaven  and  earth,   Horatio,"
;;  
;; Packaging all this in a function MJust:

(defun MJust (V Z)
  (let (T V1 N S B R EB BP)
    (a T (<> " " V)
       V1 (k (^ (s 'v T) (rev (s 'v (rev T))) ; conforming input string
		(v T (dp 1 (rot 1 (cat 1 T))))) V)
       N (+ 1 (r '+ (= " " V1)))                  ; number of words 
       S (- (p V) (p V1))                         ; number of extra blanks
       B (cat 1 (+ 1 (k (elt V1 " ") (i (p V1)))))  ; starting positions
       R (JUSTIFY Z N S B)                        ; justified starting positions
       EB (r '- 1 -2 (- R (+ 1 (- B (i N)))))     ; extrablank distribution
       BP (= " " V1))                             ; blank positions in V1
    (k (+ (~ BP) (ex BP EB)) V1)))                ; return justified string

;; (MJust V 1) =>
;; "There  are  more   things   in   heaven   and   earth,   Horatio,"
;; (MJust V 2) =>
;; "There   are   more   things   in   heaven   and  earth,  Horatio,"
;;
       
;; 2. Example of alps/APL used to prove that (a R (PRIMES N))
;; produces a vector R whose components are all of the primes less
;; than or equal to N.
(defun PRIMES (N)
  (k (= 2 (r '+ 1 (= 0 (o '| (i N) (i N))))) (i N)))

;; Proof:
;; a. Let (a C (= 2 (r '+ 1 (= 0 (o '| (i N) (i N))))))

;; b. C is a boolean vector of length N, it compresses (i N). Hence, R
;; contains only elements from (i N) i.e. the first N positive
;; integers.

;; c. For some J, 1<=J<=N, consider the Jth element of C: (aref C J). Then
;;        (aref C J) => (aref (= 2 (r '+ 1 (= 0 (o '| (i N) (i N))))) J)
;;                   =>       (= 2 (r '+ 1 (= 0 (o '| (i N) J))))
;;                   =>       (= 2 (r '+ 1 (= 0 (| (i N) J))))
;;                   =>       (= 2 (r '+ 1 (= 0 (| (i J) J))))

;; d. (eq (aref C J) 1) iff J has precisely two exact divisors from (i J),
;; that is if J is prime.
;; 

;; e. (eq (aref C J) 1) selects J in (k C (i N)). Hence, R contains
;; primes less than or equal to N.

;; f. Conversely, consider any prime P, 2<=P<=N. Then (elt P (i N)) and
;; (= 2 (r '+ 1 (= 0 (| (i N) P)))) => 1

;; g. Thus, (eq (aref C P) 1) and (elt P R)
;; QED
;;
;; Annotated example of PRIMES in action:
;; (setq N 10) ;=> 10
;; The dyadic function (| X Y) returns the remainder of dividing Y by X.
;;
;;             (= 0 (o '| (i N) (i N))) =>     [1 1 1 1 1 1 1 1 1 1	
;; 	    			                0 1 0 1 0 1 0 1 0 1	
;; 				                0 0 1 0 0 1 0 0 1 0	
;; 				                0 0 0 1 0 0 0 1 0 0	
;; 				                0 0 0 0 1 0 0 0 0 1	
;; 				                0 0 0 0 0 1 0 0 0 0	
;; 				                0 0 0 0 0 0 1 0 0 0	
;; 				                0 0 0 0 0 0 0 1 0 0	
;;    				                0 0 0 0 0 0 0 0 1 0	
;;   				                0 0 0 0 0 0 0 0 0 1]
;; summing down the columns (r '+ 1 ....)
;;      (r '+ 1 (= 0 (o '| (i N) (i N))))  ;=> [1 2 2 3 2 4 2 4 3 4]
;; (= 2 (r '+ 1 (= 0 (o '| (i N) (i N))))) ;=> [0 1 1 0 1 0 1 0 0 0]
;; (i N)                                   ;=> [1 2 3 4 5 6 7 8 9 10]
;; compress (i N)
;; (k (= 2 (r '+ 1 (= 0 (o '| (i N) (i N))))) (i N)) ;=> [2 3 5 7]
;;
;; Note that in the matrix above, for a number J, column J has a one
;; where the index of row I divides J exactly. Row 1 is all ones and
;; the diagonal elements are also all ones since every integer is
;; divisible by 1 and itself. Therefore if column J has ones in rows
;; other than 1 and J then J is not a prime.
;;
;; A prime number is a number that has exactly two integer divisors.
;; The function PRIMES expresses this definition quite neatly.
;; It is however not very efficient as it creates an NxN matrix.
;; See the file lisp/prime.al for other implementations.

;; 3. Is X an integer ?
(defun IsInt (X) (= X (f X)))
;; (IsInt [-1.1 -1 -.9 0 .5 1.3 4 4.5]) ;=> [0 1 0 1 0 0 1 0]
;; alps also has a built-in convenience function  intp
;; (intp [-1.1 -1 -.9 0 .5 1.3 4 4.5])    ;=> [0 1 0 1 0 0 1 0]

;; 4. Is A numeric, as opposed to character
(defun IsNum (A) (= 0 (ex 0 (p 0 (rav A)))))
;; (IsNum 1)         ;=> [1]
;; (IsNum "ASD")     ;=> [0]
;; (IsNum $[a b c])  ;=> [0]
;; Note one could also use the alps lisp predicate nump: (= (nump A) t)
;; but the idiom is illustrative of two interesting APL features -
;; reshape 0 of X: (p 0 X) creates a null vector of the same type as X
;; and the expansion: (ex 0 N) will be [0] for a null vector N,
;; " " for null character vector and $[nil] for a null reference vector.
;; Using the lisp equality predicate eq:
;; (eq []  (p 0 1))        ;=> t
;; (eq ""  (p 0 "ASD"))    ;=> t
;; (eq $[] (p 0 $[a b c])) ;=> t
;; See also 15. in this section.

;; 5. Is L made up entirely of elements of V ?
(defun IsOf (L V) (r '^ (elt L V)))
;; (IsOf "aeiou"  "facetious") ;=> 1

;; 6. Is M a symmetric matrix ?
(defun IsSymm (M) (r '^ (rav (= M (tr M)))))
;; (IsSymm (o '+ (i 10) (i 10))) ;=> 1
;; (IsSymm (o '- (i 10) (i 10))) ;=> 0

;; 7. Are all of the elements of V unique ?
(defun Unique (V) (r '^ (= (ind V V) (i (p V)))))
;; (Unique (? 10 10))      ;=> 1
;; (Unique {1 (? 10 10)})  ;=> 0

;; 8. Is V nondecreasing ?
(defun NonDecreasing (V) (r '^ (= (gup V) (i (p V)))))
;; (NonDecreasing  [-1.1 -1 -.9 0 .5 1.3 4 4.5]) ;=> 1

;; 9. Are two equal length vectors equal ?
(defun VecEq (V W) (r '^ (= V W)))
;; (VecEq (i 10) (i 10)) ;=> 1

;; 10. Does the vector V contain any elements from L ?
(defun HasSome (V L) (r 'v (elt L V)))
;; (HasSome "aeiou"  "bcdfg")  ;=> 0
;; (HasSome "aeiou"  "bcdfgo") ;=> 1

;; 11. Are any of the rows of the matrix M duplicated
(defun HasDups (M) (r '~^ (= 1 (r '+ 1 (. '^ '= M (tr M))))))
;; Note the use of nand in the outermost reduction
;; (HasDups (p [6 5] "Hello ")) ;=> 0
;; (HasDups (p [7 5] "Hello ")) ;=> 1

;; 12. Is the string S free from any repeated spaces
(defun NoDupSpace (S)
  (let ((B {1 (<> S " ")})) (r '^ (v B (dp 1 (rot -1 (cat 1 B)))))))
;; (NoDupSpace "sadas  ads asd") ;=> 0
;; (NoDupSpace "sadas ads asd")  ;=> 1
;; See also 5.5.

;; 13. Is the vector X a permutation of the vector Y ?
(defun IsPerm (X Y) (. '^ '= (aref X (gup X)) (aref Y (gup Y))))
;; (IsPerm (i 10) (? 10 10))  ;=> 1
;; (IsPerm (i 5) [5 2 3 4 3]) ;=> 0

;; 14. Is the Boolean vector V either all 1's or all 0's ?
(defun All1v0 (V L) ; 9 variants, L selects the variant
  (prog ()
	(go L)
	L1 (return (r '~^ (r 'v (o '= [0 1] V))))
	L2 (return (v (r '^ V) (~ (r 'v V))))
	L3 (return (= 0 (| (p V) (r '+ V))))
	L4 (return (r '^ 1 (= V (tk 1 V))))
	L5 (return (~ (r '^ (elt V (~ V)))))
	L6 (return (r '^ (= #IO (ind V V))))
	L7 (return (r '^ (/ V (r 'v V))))
	L8 (return (= (r 'c V) (r 'f V)))
	L9 (return (r '<> (elt [1 0] V)))
	))
(defun TAll1v0 ()
  (let* ((#IO 0) (L $[L1 L2 L3 L4 L5 L6 L7 L8 L9]) V LN
        (A (tr (enc (p 5 2) (i 32))))) ; all combinations of 1's and 0's
    (FOR I 0 8
	 (FOR J 0 31
	      (a V (aref A ${J : }) LN  (aref L I))
	      (when (neql (All1v0 V LN) (elt J [0 31]))
		(princl (fmt "Error with variant " LN "  V=" V))
		;; The next expression obtains the offending code
		;; from the function All1v0 showing how code can be
		;; treated as data
		(prin0 (car (cdadr (member LN (caddr All1v0)))))
		(princl (fmt " => "  (All1v0 V LN)))
		)))))
;; Test all variants on all combinations
;; (TAll1v0) ;=> 9
;; If, for example, the <> is changed to = in L9:
;; (TAll1v0) =>
;; Error with variant L9  V=0 0 0 0 0
;; (r '= (elt [1 0] V))  => 0
;; Error with variant L9  V=0 0 0 0 1
;; (r '= (elt [1 0] V))  => 1
;; ...
;; Error with variant L9  V=1 1 1 1 1
;; (r '= (elt [1 0] V))  => 0

;; 15. Are two entities (of any sort) identical ?
(defun EQUALS (X Y)
  (cond ((neql (p 0 X) (p 0 Y))  0) ; check type
	((ne (rank X) (rank Y)) 0)  ; check rank
	((neql (p X) (p Y))     0)  ; check shape
	(t (r '^ (rav (= X Y))))))  ; go compare
;; (EQUALS (i 10) (rev (i [10 1]))) ;=> 1
;; (EQUALS "Hello" {"Hell" "o"})    ;=> 1
;; (EQUALS (i 10) "Hello")          ;=> 0
;; (EQUALS (i 10) (i 11))           ;=> 0
;; (EQUALS (p [2 3] 0) (p (i 3) 0)) ;=> 0

;;
;; Section 7.0 Scan
;;
;; The scan operator is a powerful device in APL/alps for avoiding loops.
;;

;; 1. Running sum
;; (s '+ (i 10))  ;=> [1 3 6 10 15 21 28 36 45 55]

;; 2. Progressive maxima
;; (s 'c [3 1 2 4 8 5 6 7 10 9])  ;=> [3 3 3 4 8 8 8 8 10 10]

;; 3. Progressive minima
;; (s 'f [9 10 7 6 5 8 4 2 1 3])  ;=> [9 9 7 6 5 5 4 2 1 1]

;; 4. Generate the sequence 1 1 2 2 ... N N
;; The - primitive in APL denotes alternating sum for scans and reductions
;; (| (s '- 1 -1 (* 2 (i 10))))  ;=> [2 2 4 4 6 6 8 8 10 10]

;; Scans of Logical Arrays

;; 5. Turn on all zeros after the first 1
;; (s 'v [0 0 1 0 0 1 1 0])   ;=> [0 0 1 1 1 1 1 1]

;; 6. Leave only the leftmost 1 turned on
;; (s '< 1 -1 [0 0 1 1 0 1 1 0]) ;=> [0 0 1 0 0 0 0 0]
;; Note in APL this is simply <\0 0 1 1 0 1 1 0 => 0 0 1 0 0 0 0 0
;; In alps we use the -1 direction parameter after the axis to select
;; APL semantics when the operation is non-commutative

;; 7. Leave only the first 0 turned off
;; (s '<= 1 -1 [1 0 1 1 0 1 1 0])  ;=> [1 0 1 1 1 1 1 1]

;; 8. Create a vector of running even parity on B
;;    Also, convert reflected Gray code into binary
;; (s '<> [1 0 1 0 1 0 1 0 1 1 0 0]) ;=> [1 1 0 0 1 1 0 0 1 0 0 0]

;; 9. Turn off all elements after the first 0
;; (s '^ [1 1 1 1 0 1 1 0])  ;=> [1 1 1 1 0 0 0 0]

;; 10. Power of 2 that evenly divides N
;;     Uses 5. on reverse of argument
;; (* 7 (exp 2 5)) '=> 224
;; (enc (p 9 2) 224)                  ;=> [0 1 1 1 0 0 0 0 0]
;; (rev (enc (p 9 2) 224))            ;=> [0 0 0 0 0 1 1 1 0]
;; (s 'v (rev (enc (p 9 2) 224)))     ;=> [0 0 0 0 0 1 1 1 1] 
;; (~ (s 'v (rev (enc (p 9 2) 224)))) ;=> [1 1 1 1 1 0 0 0 0] 
;; (r '+ (~ (s 'v (rev (enc (p 9 2) 224))))) ;=> 5            
;; Putting this into a function
(defun NP2A  (N) ;; max power of 2 that divides N APL version
  (r '+ (~ (s 'v (rev (enc (p 9 2) N))))))
;;
;;  Here is a recursive lisp version for contrast
(defun NP2L (N) ;; max power of 2 that divides N lisp version
  (cond ((onep (| 2 N)) 0) (t (+ 1 (NP2L (/ N 2)))))))
;; (NP2L 224) ;=> 5
;; (NP2A 224) ;=> 5
;; Test for all values from 1 to 256
;; (genc '(lambda (X) (eq (NP2A X) (NP2L X))) (explod (i 256))) ;=> t
;;
;; Section 8.0 Text processing idioms
;;

;; Because APL does not contain a complete complement of text
;; processing operators, the use of APL in business applications
;; necessitated the development of a library of text processing
;; idioms. MAKEARRAY, Expand, and the blank removal idioms have
;; already been described in section 5, Annotated examples. These, and
;; the others given below are generally more useful as easily grasped
;; examples of APL functions.

;; 1. Pattern matching:
(defun PatMat (A B)
  (k (dp -1 (r '^ 1 (rot (- (i (p A)) 1) (cat (o '= A B) 0)))) (i (p B))))
;; (PatMat "the" "the quick brown fox jumped over the lazy dog") ;=> [1 33]

;; 2. Left justify a word list
(defun LeftJust (L) (rot (r '+ (s '^ (= L " "))) L))
;; (setq L (p [4 6] "Hello Polly   Fred   Joe"))
;; "Hello 
;;  Polly 
;;    Fred
;;     Joe"
;; (LeftJust L)
;; "Hello 
;;  Polly 
;;  Fred  
;;  Joe   "

;; 3. Right justify a word list
(defun RightJust (L) (rot (- 1 (dec (= L " ") 1)) L))
;; (RightJust L)
;; " Hello
;;   Polly
;;    Fred
;;     Joe"
;;
;; Another version a bit easier to understand
(defun RightJust2 (M) (rot (- (r '+ (s '^ -1 -1 (rev  (= " " M))))) M))

;; 3.1 Center justify a word list
(defun CenterJust (L)
  (let ((B (= L " ")))
    (rot (c (/ (+ (r '+ (s '^ B)) (- 1 (dec B 1))) 2)) L)))
;;(CenterJust (Expand  (p [3 6] "Hello Fred     Joe") "    Sebastien"))
;; "   Hello   
;;     Fred    
;;      Joe    
;;   Sebastien "


;; 4. Replace charaters C with " "'s in string B
(defun RepByBlank (B C) (let ((A (<> B C))) (ex A (k A B))))
;; (RepByBlank "Next_Page" "_") ;=> "Next Page"

;; 5. Filter out blank rows from a word list
(defun RemBlankLine (L) (k 1 (~ (. '^ '= L " ")) L))
;; (setq L (p [5 6] "Hello Polly         Fred   Joe"))
;; "Hello 
;;  Polly 
;;       
;;    Fred
;;     Joe"
;; (RemBlankLine L)
;; "Hello 
;;  Polly 
;;    Fred
;;     Joe"

;; 6. Alphabetically sort a word list
(defun ASort (L) "Alphabetically sort a word list L"
       (aref L (gup (dec 27 (tr (+ -1 (ind #uc L))))) ()))
;; #uc uppercase alphabetic characters defined in prims.al

;; (let ((L (p [3 5] "HelloPollySue  "))) (eql (ASort (rot 1 1 L)) L)) ;=> t

;; 7. Sort a word list by length of word
(defun LSort (L) (aref L (gup (. '+ '<> L " ")) ()))
;; (let ((L (p [3 5] "PollySue  Hi   "))) (eql (LSort L) (rev 1 L))) ;=> t

;; 8. Remove duplicate words
(defun RemDupW (L)
  (let ((Q (dec 2 (. '^ '= L (tr L))))) (k 1 (= (ind Q Q) (i (p Q))) L)))
;; (let ((L (p [3 5] "HelloPollySue  "))) (eql (RemDupW (p [9 5] L)) L)) ;=> t

;; 9. Find the number of occurrences of word W in list L
(defun Wocc (W L) (r '+ (. '^ '= L W)))
;; (let ((L (p [3 5] "Hi   Sue  Hi   "))) (Wocc "Hi   " L))  ;=> 2

;; 10. Create a text array from user input
(defun EnterText ()
  (prog ((T (p [0 0] "")) Line)
	Loop (go (tk (= 0 (p (a Line (getl)))) $[Out]))
	     (a T (Expand T Line))
	     (go 'Loop)
	Out  (return T)))

;; 11. Remove text between brackets (from dyalog APL)
(defun ElimB (T) (k (~v (elt T "()") (s '<> (elt T "()"))) T))
;; (ElimB "The old dog (called cerberus) barked.") ;=> "The old dog  barked."

;;
;; Section 9.0 Phrases
;;

;; A few APL idioms are so commonly used that, when reading a program
;; aloud, they are typically replaced by an English phrase or
;; word. Examples of these idioms are given below.

;; 1. If -- conditionally branch to label L if condition C is true:
;; (go (k C $[L]))
;; This idiom is rarely used in practise since LISP has a plethora of
;; conditional constructs. It is included here for purposes of
;; illustration only.
(defun TIF (C)
  (prog () ;; need prog construct when using (go) and (return)
	(go (k (~ (elt C [0 1])) $[Error]))
	(go (k C $[L]))
	(return "false")
	L     (return "true")
	Error "Argument can only be 0 or 1"))

;; Applying (TIF):  
;; (TIF 1)  ;=> "true"
;; (TIF 0)  ;=> "false"
;; (TIF 42) ;=> "Argument can only be 0 or 1"

;; 2. Positions - of elements of V in W: (k (elt W V) (i (p W)))
(defun Posns (V W) (k (elt W V) (i (p W))))
;; (Posns "aeiou" "facetious") ;=> [2 4 6 7 8]

;; 2.1 Replace characters C with character R in V
(defun RepChar (C R V) "Replace characters C with character R in V"
       (let ((T (rav V))) (aset T R (k (elt T C) (i (p T)))) (p (p V) T)))
;; Here we replace periods and commas by blanks
;; (RepChar ",." " " "This,is,an,example....") ;=> "This is an example    "

;; 2.2 Replace elements of V satisfying condition C with element R
(defun RepElem (C R V) "Replace elements satisfying condition C with R in V"
  (let ((T (rav V))) (aset T R (k (C T) (i (p T)))) (p (p V) T)))
;; Here we replace the non-printable characters in a string by a ? 
;; #p contains the printable characters (defined in prims.al).
;; (RepElem '(lambda (X) (~(elt X #p))) "?" "\DHeader 1\M\J") ;=> "?Header 1??"
;; See also 14.2 for condionally removing elements
;; (RemElem "\DHeader 1\M\J" '(lambda (X Y) (~ (elt X V))) #p) ;=> "Header 1"

;; 
;; 3. Round -- to the nearest integer (for positive real numbers, R):
;; (f (+ .5 R)))
(defun RTNI (R) (f (+ .5 R)))
;; (RTNI 0.2342)  ;=> 0
;; (RTNI 0.5342)  ;=> 1
;;  Make S a sine wave of one period over 11 points with amplitude 10
;; rounded to 3 decimal places (see 16.1 for rounding to P places RP)
;; (p (a S (RP (* 10 (% 1 (% (/ (i [0 10]) 5)))) 3))) ;=> 11
;; S ;=> [0 5.878 9.511 9.511 5.878 0 -5.878 -9.511 -9.511 -5.878 0]
;; (RTNI S) ;=> [0 6 10 10 6 0 -6 -10 -10 -6 0]
;; To generate a sine wave with amplitude A, periods P and number of points N
(defun VSin (A P N) (* A (% 1 (% (/ (i {0 N}) (/ N (* 2 P)))))))
;; Plot a sinewave of 60 points over 3 periods with amplitude 5
;; (aref " *" (+ #IO (o '= (i [5 -5]) (RTNI (VSin 5 3 60))))) 
;; "    ***                 ***                 ***              
;;     *   *               *   *               *   *             
;;    *     *             *     *             *     *            
;;   *       *           *       *           *       *           
;;                                                              
;;  *         *         *         *         *         *         *
;;                                                              
;;             *       *           *       *           *       * 
;;              *     *             *     *             *     *  
;;               *   *               *   *               *   *   
;;                ***                 ***                 ***    "


;;
;; 4. Sort -- up or down (aref V (gup V)) or (aref V (gdn V))

;; 5. Indeces -- into a vector (i (p V))


;; 6. Last -- element of a vector: (aref V (p V))
;;         -- row of a matrix    : (aref M (aref (p M) 1) ())
;;                               : (aref M (tally  M) ())
;;         -- column of a matrix : (aref M () (aref (p M) 2))
;;                               : (aref M () (p [] (tk -1 (p M))))

;; 7. All -- (see Predicates):  (r '^ A)

;; 8. There exists (some) -- (see Predicates): (r 'v A)

;; 9. Ordinality -- of a vector (gup (gup V))
;; (gup (gup [1 2 2 3 3 2 2 1])) ;=> [1 3 4 7 8 5 6 2]

;; 10. Average -- of a vector: (/ (r '+ V) (c 1 (p V)))
(defun avgv (V) (/ (r '+ V) (c 1 (p V))))
;;  (avgv [1 2 2 3 3 2 2 1]) ;=> [2]

;;
;; Section 10.0 Templates
;;

;; A template is a pattern that can be used in various situations for
;; creating useful constructs. Some examples are given below where alpha
;; stands for a class of appropriate operators or idioms.


;; 1. (p (p A) (alpha (rav A))) Apply alpha to the ravel of A and
;; reshape the result back to the original shape of A.

;; 2. (+ (* B alpha) (* D (~ alpha))): Avoid conditional branching of
;; control by constructing a vector with two types of components, B if
;; the condition alpha is true (i.e. has the value 1), and D
;; otherwise. (See, for example, Section 6 Predicates 1. - JUSTIFY,
;; second last line assinging value to G)

;; 3. (tr A (o alpha B C)) Use the outer product to compute a
;; multi-dimensioned array of values, and the use diagonal transpose
;; (using argument A) to get rid of unneeded elements.

;;
;; Section 11.0 Identities
;;

;; Because APL is closer to a mathematical formalism than most other
;; programming languages, mathematical properties of APL and to a
;; certain extent alps programs are more apparent. As an example of
;; this "phenomenon", several identities expressed in alps are given
;; here.

;; 1. (exp (- A)) => (/ (exp A)) which is the same as to say that e to
;; the power of -A is the same as 1 over e to the power of A

;; 2. (exp (+ A B)) => (* (exp A) (exp B))

;; 3. (- (c A)) => (f (- A))

;; 4. (~ (^ A B)) => (v (~ A) (~ B)) or in LISP:
;;    (not (and A B)) => (or (not A) (not B))
;; De Morgans rule

;; 5. (- (+ A)) => (+ (- A))

;; 6. (- (- A) B) => (+ (- A) (- B))

;; 7. (* (N (+ A B))) => (+ (* N A) (* N B))
;; this is  the distributive law for addition and multiplication

;; 8. (r '* (p A)) => (p (, A)) the product of the dimensions of A is
;; equal to the number of elements in the ravel of A

;; 9. (aref V A) => (p (p A) (aref V (rav A))) This is a powerful
;; property of the APL subscripting function: The shape of the result
;; is the shape of the index.

;; 10. (tr A (tr B C)) => (tr (aref A B) C)

;; (! (- F 1)) <=> (/ (! F) F)
;; (let ((F (/ (i 5) 2))) (eql (! (-  F 1)) (/ (! F) F))) ;=> t

;;
;; Section 12.0 Extensions
;;

;; Some vector operators do not extend to higher dimensioned
;; arrays. Typical ways of achieving the same effect using idioms are
;; given in this section.

;; 1. Element of: If W is a word and L is a compatible word list in
;; the APL sense i.e. (rank L) => 2; (p W) => (aref (p L) 2), then 
;; (r 'v (. '^ '= L  W)) is a simple way to obtain the effect of
;; element-of (elt) on a rank two array. 
(defun El (W L) (r 'v (. '^ '= L (tr(tk (cat (dp -1 (p W)) (tk -1 (p L))) W)))))
;; The (tk (aref (p L) 2) W) operation makes W compatible with L
;; provided that (p W) is less than or equal to (aref (p L) 2).
;; Otherwise W will be truncated leading to possible false positives.
;;
;; (p [4 5] "HelloPollyFred Joe  ") =>
;; "Hello
;;  Polly
;;  Fred 
;;  Joe  "
;; (El "Sue" (p [4 5] "HelloPollyFred Joe  ")) ;=> 0
;; (El "Joe" (p [4 5] "HelloPollyFred Joe  ")) ;=> 1
;; (El (p [2 3] "JoeSue")   (p [4 5] "HelloPollyFred Joe  ")) ;=> [0 0 0 1]
;; (El (p [2 4] "FredJoe ") (p [4 5] "HelloPollyFred Joe  ")) ;=> [0 0 1 1]

;; 2. Compression: Given a boolean array A and a compatible array B,
;; it is desired to create a resultant array C, with the same number
;; of rows, such that
;; (aref C I ()) is the same as (k (aref A I ()) (aref B I ())), possibly
;; padded with zeros. In other words it is desired to compress each
;; row of B by the corresponding row in A. This effect can be
;; accomplished with the following idiom cast as Ccomp:
(defun Ccomp (A B)
  (let ((H (r 'c (r '+ A)))) ; number of elements in rows of the result
    (p (cat (tally  B) H)
       (ex (rav (o '>= (r '+ A) (i H))) (k (rav A) (rav B))))))
;; (eql (Ccomp (LTM 5) (p [5 5] (i 25))) (* (LTM 5) (p [5 5] (i 25)))) ;=> t

;; 3. Primitive scalar operations: For example, to multiply each row
;; of the matrix M by the compatible vector V, two approaches are
;; possible: (a C (* M (p (p M) V))) or (a C (tr [1 2 2] (o '* M V)))
;; Note that the second version uses template 10.3. Also the index for the
;; transposition is different from that in the original report.

;; (setq M (p [5 5] (i 25)))
;; [ 1  2  3  4  5
;;   6  7  8  9 10
;;  11 12 13 14 15
;;  16 17 18 19 20
;;  21 22 23 24 25]
;;
;; (setq V [5 4 3 2 1]) => [5 4 3 2 1]
;;
;; (* M (p (p M) V))  ;version 1 =>
;; [  5  8  9  8  5
;;   30 28 24 18 10
;;   55 48 39 28 15
;;   80 68 54 38 20
;;  105 88 69 48 25]
;;
;; (tr [1  2 2] (o '* M V)) ;version 2 => 
;; [  5  8  9  8  5
;;   30 28 24 18 10
;;   55 48 39 28 15
;;   80 68 54 38 20
;;  105 88 69 48 25]
;;
;; Similarly, to multiply each row of the matrix M by the successive
;; elements of a compatible vector V, the two approaches are:
;; (a C (* M (tr (p (p M) V)))) and (a C (tr [1 2 1] (o '* M V)))
;;
;; (* M (tr (p (p M) V))) =>
;; [ 5 10 15 20 25
;;  24 28 32 36 40
;;  33 36 39 42 45
;;  32 34 36 38 40
;;  21 22 23 24 25]
;;
;; (tr [1  2 1] (o '* M V))
;; [ 5 10 15 20 25
;;  24 28 32 36 40
;;  33 36 39 42 45
;;  32 34 36 38 40
;;  21 22 23 24 25]
;;
;; alps currently also extends single column arrays compatible with the first
;; dimension of a matrix. 
;;
;; (* M (p {(p V) 1} V))
;; [ 5 10 15 20 25
;;  24 28 32 36 40
;;  33 36 39 42 45
;;  32 34 36 38 40
;;  21 22 23 24 25]

;; 4. Dyadic iota, aka index: To look up and entry, E, in a table, T,
;; the following idiom cast as function LOOKUP can be used to extend
;; ind to two dimensions.
;;(ind "abcd" "d") => [4] ; (ind "abcd" "s") => [5]

(defun LOOKUP (E T)
  (let ((A (c (p E) (dp 1 (p T))))) ; the larger of E and row size of T
    (ind (. '^ '= (tk (cat (tally  T) A) T) (tk A E)) 1)))
;;
;; (LOOKUP "Fred" (p [4 5] "HelloPollyFred Joe  ")) ;=> 3
;;
;; (setq L (p [4 5] "HelloPollyFred Joe  ")) =>
;; "Hello
;;  Polly
;;  Fred 
;;  Joe  "
;;
;; (LOOKUP "Joe" L)   => 4
;; (LOOKUP "Polly" L) => 2
;; (LOOKUP "Jack" L)  => 5
;;
;; Note: This again is simply an illustration of APL functions in
;; alps. For all list processing needs one would normally use the LISP
;; functions
;; (setq L '("Hello" "Polly" "Fred" "Joe")) => ("Hello" "Polly" "Fred" "Joe")
;; (member  "Fred" L) => ("Fred" "Joe")
;; (member "Jack" L)  => nil
;;
;; To achieve the effect of dyadic iota working on two compatible
;; tables, (ind B A), the following approach embodied in the function
;; (TLOOKUP A B) is useful. It returns the vector of the indeces of
;; the rows of A in the rows of B:
(defun TLOOKUP (A B)
  (. 'c '* (s '< -1 -1 (cat (. '^ '= A (tr B)) 1)) (i (+ 1 (tally  B)))))
;;
;; (setq L (p [4 5] "HelloPollyFred Joe  ")) =>
;; "Hello
;;  Polly
;;  Fred 
;;  Joe  "
;;
;; (TLOOKUP (Expand (rot 1 2 L) "Sue")  L) => [3 4 1 2 5]
;;


;;
;; Section 13.0 Coding tricks
;;

;; A few miscellaneous APL programming tricks in alps are given in
;; this section.

;; 1. To accomplish the effect of (/ (* A C) (* B D)) the alternate
;; product can be used:
;; With APL reduction semantics (r '/ -1 -1 {A B C D}), or
;; ;;
;; (/ (* 6 10) (* 3 5))    ;=> 4
;; (r '/ -1 -1 [6 3 10 5]) ;=> 4

;; 2, To sort a vector, V, in either ascending or descending order,
;; depending on whether B is +1 or -1: (a W (aref (* (gup V) B) V))
;; alternatively (aref ((aref $[gup gdn] (+ 1 (> B 0))) V) V)

;; 3. To achieve the effect of (aset A (aref B I J) I J J) for all I
;; and J provided that (eq (p B) (p (tr [1 2 2] A))):
;;    (aset (a C (rav A)) (rav B) (rav (tr [1 2 2] (p (p A) (i (p C))))))
;;    (a A (p (p A) C))
;; In alps the assignment to C nust happen at the left.
;; Note in the report the p is missing in (i (p C))
(defun CT3a ()
    (let* ((A (p [2 3 4] 0))
	   (B (p [2 3] (rev (i 6)))))
      (FOR I 1 2
	(FOR J 1 3
	     (aset A (aref B I J) I J J)))
      A))
(defun CT3b ()
  (let* ((A (p [2 3 4] 0))
	 (B (p [2 3] (rev (i 6))))
	 (C (rav A)))
    (aset C (rav B) (rav (tr [1 2 2] (p (p A) (i (p C))))))
    (a A (p (p A) C))))
;;
;; (eql (CT3a) (CT3b)) ;=> t
;;
;; (CT3a)
;; [6 0 0 0
;;  0 5 0 0
;;  0 0 4 0
;; 
;;  3 0 0 0
;;  0 2 0 0
;;  0 0 1 0]

;; 4. To achieve the effect of
;;  (aset A (+ (aref B J I) (* (aref C I J) (aref D K J I))) I J K) for 
;; all I, J and K: (a A (tr [2 1 1 2 3 2 1] (o '+ B (o '* C D))))
(defun CT4a ()
  (let ((A (p [2 3 4] 0))
	(B (p [3 2] (i 6)))
	(C (p [2 3] (i 6)))
	(D (p [4 3 2] (i 24))))
    (FOR I 1 2
      (FOR J 1 3
	(FOR K 1 4
	     (aset A (+ (aref B J I) (* (aref C I J) (aref D K J I))) I J K))))
    A))
(defun CT4b () 
  (let ((A (p [2 3 4] 0))
	(B (p [3 2] (i 6)))
	(C (p [2 3] (i 6)))
	(D (p [4 3 2] (i 24))))
    (a A (tr [2 1 1 2 3 2 1] (o '+ B (o '* C D))))))
;;
;; (eql (CT4a) (CT4b)) ;=> t
;; 
;; (CT4a)
;; [ 2  8  14  20
;;   9 21  33  45
;;  20 38  56  74
;; 
;;  10 34  58  82
;;  24 54  84 114
;;  42 78 114 150]

;; 
;; Section 14.0 Mini-operations
;;

;; 1. Find the position of the leftmost zero of a boolean vector V
(defun LeftZero (V) (+ 1 (r '+ (s '^ V))))
;; (LeftZero [1 1 0 1 0 0 1 1]) ;=> 3

;; 2. Remove elements from S satisfying condition C wrt value V
(defun RemElem (S C V) (k (~ (C S V)) S))
;; (RemElem [1 2 0 3 0 1 3] '= 0) ;=> [1 2 3 1 3]

;; 3. Convert a set of positive integers into a mask i.e. turn on bits
(defun Mask (V) (elt (i (r 'c V)) V ))
;; (Mask [1 5 9]) ;=> [1 0 0 0 1 0 0 0 1]

;; 4. Count occurrences of each of the different elements of a vector
(defun CountOcc (V) (r '+ (o '= (RemDup V) V)))
;; (CountOcc (p 31 (i 5))) ;=> [7 6 6 6 6]

;; 5. Find where the 1s occur in a boolean vector
(defun FindOnes (V) (k V (i (p V))))
;; (let ((V [0 1 0 1 0 1 1 ])) (eql V (Mask (FindOnes V)))) ;=> t

;; 6. Locate the maximum and minimum elements of a vector
(defun LocMax (V) (ind V (r 'c V)))
(defun LocMin (V) (ind V (r 'f V)))
;; (LocMax [1 4 -99 3 100 12 2]) ;=> 5

;; 7. Find the position of the first occurrence of any element of W in V
(defun FFPos (W V) (r 'f (ind V W)))
;; (FFPos  [34  35 46  3 54 2 35] (i 5)) ;=> 2

;; 8. Extend a transitive binary relation
(defun ExtBR (B) (. 'v '^ B B))

;; 9. Isolate the fractional part of a real number
(defun Frac (S) (| 1 S))
;; (Frac 3.14159) ;=> .14159

;; 9.1 Isolate the integral part of a real number
(defun Int (S) ((aref $[c c f] (+ 1 #IO (* S))) S))
;; (Int -1.1) ;=> -1
;; alps also has int
;; (int -1.23) ;=> -1

;; 10. Separate into integer and fractional parts
(defun FracSplit (S) (enc [0 1] S)) 
;; (FracSplit 3.14159) ;=> [3 .14159]

;; 11. Create an arithmetic progression vector of N elements
;; with increment D starting at S
(defun APV (S N D) (let ((#IO 0)) (+ S (* D (i N)))))
;; (APV 1 10 3) ;=> [1 4 7 10 13 16 19 22 25 28]

;; 12. Find the number of occurrence of scalar S in vector V
(defun CountS (S V) (. '+ '= S V)) ; inner product
;; (CountS 2 [1 2 2 2 2 23 34  3 3 32 2 2 2 2]) ;=> 8

;; 13. Compute the original of a sum scanned vector
(defun Orig (V) (- V (cat 0 (dp -1 V))))
;; (Orig (s '+ (i 10))) ;=> [1 2 3 4 5 6 7 8 9 10]
;; Another form is with reduce n-wise
(defun UnScan (V) (cat (tk 1 V) (r '- -1 -2 V )))
;; (UnScan (s '+ (i 10))) ;=> [1 2 3 4 5 6 7 8 9 10]

;; 16. Encode a boolean vector into an integer
(defun B2I (B) (dec 2 B))
;; (B2I [0 0 0 0 1 0 0 1]) ;=> 9

;; 17. Create an NxN identity matrix
(defun IDM (N) (o '= (i N) (i N))) 
(defun IDM1 (N) (p {N N} (cat 1 (p N 0))))

;; 18. Create an upper-triangular matrix
(defun UTM (N) (o '<= (i N) (i N))) 

;; 19. Create a lower-triangular matrix
(defun LTM (N) (o '>= (i N) (i N))) 

;; 20. Align the diagonals of a matrix into columns with (wrap-around)
(defun ColDiag (M) (rot (+ -1 (i (tally  M))) M))
;(setq M (o '* (i 5) (i 5))) =>
;[1  2  3  4  5
; 2  4  6  8 10
; 3  6  9 12 15
; 4  8 12 16 20
; 5 10 15 20 25]
;(ColDiag M) =>
;[ 1  2  3  4  5
;  4  6  8 10  2
;  9 12 15  3  6
; 16 20  4  8 12
; 25  5 10 15 20]

;; If ColDiag is applied recursively on a matrix as often as there are
;; elements in a row of the matrix the result is again the original
;; matrix.  To check this define a function to recursively apply a
;; function F N times to an initial argument X
(de FoFn (F X N) (if (zerop N) X (FoFn F (F X) (- N 1))))
;; (let ((M (o '* (i 5) (i 5)))) (eql (FoFn ColDiag M 5) M)) ;=> t

;; 21. Invert a permutation P 
(defun InvPerm (P) (gup P))
(defun InvDPerm (P) (i P (i (p  P)))) ; for direct representation only

;; 22. Form the transitive closure of a relation (TESTME)
(defun TC (G) (prog (T)
LOOP  (go (tk (r 'v (rav (a G (<> (a T G) (v G (. 'v '^ G)))))) $[LOOP]))))

;; 23. Change all zeros to N's in the vector V
(defun FlipZero (V N) (+ V (* N (= V 0))))
;; (FlipZero [1 2 0 4 0 2 1] 3) ;=> [1 2 3 4 3 2 1]

;; 24. Evaluate a polynomial with coefficients C at point X
(defun Poly (C X) (dec X C))
; 4*X^3 - 2*X^2 + 3 evaluated at 1 and 2 
;; (Poly  [4 -2 0 3] 1) ;=> 5
;; (Poly  [4 -2 0 3] 2) ;=> 27

;;
;; Section 15.0 Other operations
;;

;; 1. ROWSORT -- Sort each of the rows of a matrix indepedently:
(defun ROWSORT (A)
  (let ((C (gup (rav A))))
    (p (p A) (aref (aref (rav A) C)
		   (gup (aref (rav (tr (p (rev (p A)) (i (tally  A)))))
			      C))))))
;; (a L (p [4 5] "HelloPollyFred Joe  "))
;; (ROWSORT L)
;; "Hello
;;  Plloy
;;   Fder
;;    Jeo"
;;
;; Alternatively:
(defun ROWSORT2 (A)
  (let ((R (p A))
	(C (rav A))
	(X (- (gup (rav A)) #IO)))
    (p R (aref C (+ #IO (aref X (gup (f (/ X (tk -1 R))))))))))

;; 2. Make A the same rank as C without changing its contents:
;; (a A (p (cat (p (- (rank C) (rank A)) 1) (p A)) A))
;; The original APL idiom is wrong, it should read ((((ppC)-ppA)p1),(pA))pA
(defun SameRank (A C) (p (cat (p (- (rank C) (rank A)) 1) (p A)) A))
;;
;; (a A (p [2 3] (i 6)))
;; (a C (p [2 3 4] (i 24)))
;; (setq A (SameRank A C)) =>
;; [1 2 3
;;  4 5 6]
;; (p A)    => [1 2 3]
;; (rank A) => 3

;; 3. What are the "upper-left-hand-corners" in array A of places
;; where A can fit, where C is of arbitrary rank and A is the same
;; rank as C. AMTOAL is annotated in 5.7
(defun Places  (A C) "Places of A in C"
       (p (p C) (. '^ '>= (+ 1 (- (p C) (p A))) (AMTOAL (p (p C) 1)))))
;;
;; (Places (p [2 3] 0) (p [5 5] 0)) =>
;; [1 1 1 0 0
;;  1 1 1 0 0
;;  1 1 1 0 0
;;  1 1 1 0 0
;;  0 0 0 0 0]

;; 4. Map a vector, V, uniformly into N buckets. Bucket returns a
;; vector of bucket numbers for the corresponding elements of V.
(defun Bucket (V N)
  (let ((A (- V (r 'f V)))) ; V - smallest number in V
    (r '+ (o '>= (* A (/ N (r 'c A))) (- (i N) 1)))))
;;
;; (setq V [6 7 8 12 13 13 17 20 23 24]) => [6 7 8 12 13 13 17 20 23 24]
;; (p V) => [10]
;; Show the elements of the vector with corresponding bucket numbers
;; (p [2 10] (cat V (Bucket V 5))) =>
;; [6 7 8 12 13 13 17 20 23 24
;;  1 1 1  2  2  2  4  4  5  5]
;; To get a count of hits in each bucket use (r '+ (o '= (i N) (Bucket V N)))
;; (r '+ (o '= (i 5) (Bucket [6 7 8 12 13 13 17 20 23 24] 5))) ;=> [3 3 0 2 2]
;; (r '+ [3 3 0 2 2]) => 10 ; sum of number of elements in all buckets

;; 5. Merge S1 and S2 under control of boolean vector B:
(defun M5a (S1 S2 B)
  (let ((SS1 (ex B S1)))
    (aset SS1 S2 (k (~ B) (i (p B))))
    SS1))
;; (M5a "abcde" "ABCDE" [1 0 1 0 1 0 1 0 1 0]) ;=> "aAbBcCdDeE"
;; (M5a "abcde" "ABCDE" [1 1 0 0 1 1 0 0 0 1]) ;=> "abABcdCDEe"
(defun M5b (S1 S2 B)
  (let ((R (cat S1 S2)))
    (aset R (cat S1 S2) (gdn B))
    R))
;; (M5b "abcde" "ABCDE" [1 0 1 0 1 0 1 0 1 0]) ;=> "aAbBcCdDeE"
;; (M5b "abcde" "ABCDE" [1 1 0 0 1 1 0 0 0 1]) ;=> "abABcdCDEe"

;; 6. Put the string (or vector), B, into the string, A, at position N:
(defun Ins (A B N) (aref {A B} (gup {(i (p A)) (p (p B) N)})))
;; (Ins "Hello Polly" "dear " 6) ;=> "Hello dear Polly"

;; 7. Depth of parenthesization:
(defun DPar (S) (s '+ (- (= S "(") (cat 0 (dp -1 (= S ")"))))))
;; (DPar "(i(|(- B A)))") ;=> [1 1 2 2 3 3 3 3 3 3 3 2 1]

;; 8. Create a truth table of order N:
(defun CTT (N) (tr (enc  (p N 2) (+ -1 (i (exp 2 N))))))
;; (CTT 3) =>
;; [0 0 0
;;  0 0 1
;;  0 1 0
;;  0 1 1
;;  1 0 0
;;  1 0 1
;;  1 1 0
;;  1 1 1]

;; 9. Column indeces of first occurrences of E's in rows of M
(defun CIFO (E M) (+ 1 (r '+ (s '^ (<> M E)))))
;; (CIFO 1 (IDM 5)) ;=> [1 2 3 4 5]
;; (CIFO 0 (IDM 5)) ;=> [2 1 1 1 1]

;; 10. Move all of a set of points into the first quadrant:
(defun M2FQ (P) (tr [1 2 1] (o '- P (r 'f P))))
;; (a P1 (tr (p [5 2] [ 0 0   0  1    1  1    1 0   0 0])))
;; (a P2 (tr (p [5 2] [ 0 0   0 -1    1 -1    1 0   0 0])))
;; (a P3 (tr (p [5 2] [ 0 0   0 -1   -1 -1   -1 0   0 0])))
;; (a P4 (tr (p [5 2] [ 0 0   0  1   -1  1   -1 0   0 0])))
;; (mapcar 'M2FQ (list P1 P2 P3 P4)) =>
;; ([0 0 1 1 0
;;   0 1 1 0 0]
;;
;;  [0 0 1 1 0
;;   1 0 0 1 1]
;;
;;  [1 1 0 0 1
;;   1 0 0 1 1]
;;
;;  [1 1 0 0 1
;;   0 1 1 0 0]
;; )
;; TODO: use extension element-of or such like for test.

;; 11. Find the number of elements common to two vectors not counting
;; duplications: (r '+ (elt A B))
;; (r '+ (elt "aeiou" "facetiousness")) ;=> 5

;; 12. Gap opener: X is a vector. P is a vector of indeces into X. G
;; is a vector of the same length as X, containing non-negative
;; integers. The object is to open up gaps at (aref X (aref P I)) of
;; size (aref G I):

(defun MkGap (X P G)
  (ex (elt (i (+ (p X) (r '+ G)))
	 (+ (i (p X)) (aref (s '+ (cat 0 G))
			    (+ 1 (r '+ (o '> (i (p X)) (rav P))))))) X))

;; Values taken from annotations to predicates 6.1
;; (a X "There are more things in heaven and earth, Horatio,")
;; (a BP (k (= " " X) (i (p X))))   ; positions of blanks in X
;; (a G [2 2 2 2 2 2 2 3])          ; desired blank distribution
;; (MkGap X BP (- G 1)) =>
;; "There  are  more  things  in  heaven  and  earth,   Horatio,"
;; Testing against MJust
(defun TMkGap ()
  (let* ((X "There are more things in heaven and earth, Horatio,")
	 (BP (k (= " " X) (i (p X))))
	 (G [2 2 2 2 2 2 2 3])
	 (S (r '+ (- G 1))))     ; sum of extra blanks in G
    (eql (MJust (cat X (p S " ")) 1) (MkGap X BP (- G 1)))))
;; (TMkGap) ;=> t

;;
;; Section 16.0 Applications
;;

;; 1. Round the number N to P places
(defun RP (N P) (* (exp 10 (- P)) (f (+ .5 (* N (exp 10 P))))))
;; (RP (+ (* (/ (i 4) 100) 10) 1.23456789) 3)  ;=> [1.335  1.435  1.535  1.635]

;; 2. Convert degress to radians: (a R (* D (% (/ 180))))

;; 3. Convert radians to degrees: (a D (* R (/ 180 (% 1))))

;; 4. Compute the value of principal P, computed at interest rate R
;; for N periods: (a V (* P (exp (+ 1 R) N)))

;; 5. Compute the effextion rate of interest, given the nominal rate NR:
;; (a ER (exp (+ 1 (/ NR N)) N))

;; 6. Compute the limit of the nominal rate NR when continuously compounded:
;; (a ER (exp NR))

;; 7. Compute the binomial coefficients of (exp (+ X Y) N):
;; (a V (! N {0 (i N)}))

;; 8. Compute the distances among a set of points in two-space where the
;; points are represented by a vector of coordinates:
;; (a R (sqrt (r '+ (sqr (tr [1 3 2 3] (o '- P P))))))
;;
;; (a P (p [4 2] [ 0 0  0 1  1 1  1 0])) ; corners of a square
;; (a R (sqrt (r '+ (sqr (tr [1 3 2 3] (o '- P P))))))
;; R =>
;; [0.000000000 1.000000000 1.414213562 1.000000000
;;  1.000000000 0.000000000 1.000000000 1.414213562
;;  1.414213562 1.000000000 0.000000000 1.000000000
;;  1.000000000 1.414213562 1.000000000 0.000000000]
;; For each Column I Row J gives the distance between point I and point J
;;
;; alps also has a built-in dyadic function dist which computes
;; euclidean distance:
;; (r 'dist (tr [1 3 2 3] (o '- P P))) =>
;; [0.000000000 1.000000000 1.414213562 1.000000000
;;  1.000000000 0.000000000 1.000000000 1.414213562
;;  1.414213562 1.000000000 0.000000000 1.000000000
;;  1.000000000 1.414213562 1.000000000 0.000000000]

;; 
;; Section 17.0 Subroutines, Macros and idioms
;;

;; Idioms are neither macros nor subroutines. Because of their
;; flexibility, they would require too many arguments to be
;; implemented by macros in APL. Using subroutines would require a
;; higher degree of generality than the problem might warrant. These
;; points are illustrated by the following four functions which make
;; use of the same idiom, but in different ways.

;; 1. Compute the  determinant of a third-order matrix
(defun DET (A)
  (r '- (r '+ (r '* 2 (rot (p [2 3] [0 1 2 0 2 1]) (cat .5 A A))))))
;; (DET (* 4(IDM 3))) ;=> 64

;; 2. Determine whether a pair of line segments intersect.
;; The array A contains four rows and two columns. Each row gives the
;; coordinates of a point. Each pair of rows determine a line.
(defun INTRSCT (A)
  (let* ((C (dp [0 -1 0]
	       (rot 2 (tr (p [3 4] (cat 0 (i 3)))) (p [4 4 3] (cat 1 A)))))
	 (D (rot (tr [2 1 3] (p [4 2 3] [0 1 2 0 2 1]))  (cat .5 C C))))
    (r '^ (>= 0 (r '* (p [2 2] (rot 1 (r '- 1 -1 (r '+ (r '* 3 D))))))))))
;;
;; (INTRSCT (p [4 2] [-1 -1 1 1 1 -1 -1 1])) ;=> 1
;; (INTRSCT (p [4 2] [-1 -1 0 1 0 -1 1 1]))  ;=> 0

;; 3. Determine whether a polygon is convex. A is an arry of vertices,
;; no three of which are co-linear. The each row represents a
;; coordinate of a vertex. An N-a-gon will have N rows.

(defun CVX (A)
  (let* ((N (tally  A))
	 (Q (| N (o '+ (i N) (i [0 2]))))
	 (P (aref (cat 1 A) (+ Q (* N (= 0 Q))) () ))
	 (R (rot (tr [2 1 3] (p {N 2 3} [0 1 2 0 2 1]))	(cat .5 P P))))
    (= 0 (| N (r '+ (<= 0 (r '- 1 -1 (r '+ (r '* 3 R)))))))))

;; A square
;; (CVX  (p [4 2] [-1 -1  -1 1  1 1   1 -1 ])) ;=> [1]
;; A non-convex quadrilateral that looks like a triangle
;; with the last two sides co-linear
;; (CVX  (p [4 2] [-1 -1  -1 1  1 1   0 0 ])) ;=> [0]

;; 4. Determine whether a point is inside of a convex polygon

(defun INSIDE (H A)
  (let* ((N (tally  A))
	 (Q (| N (o '+ (i N) (i [0 2]))))
	 (P (aref (cat 1 A) (+ Q (* N (= 0 Q))) ()))
	 (S (+ P))  ; must make a copy of P
	 (D (tr [1 3 2 4] (p {2 N [2 3]} [0 1 2 0 2 1])))
	 (R))
    (aset S (p {N 3} {1 H}) () 3 ())
    (a R (rot D (cat .5 (cat .5 S S) (cat .5 P P))))
    (r '^ (<= 0 (r '* 1 (r '- 2 -1 (r '+ (r '* 4 R))))))))

;; (INSIDE [1  1]   (p [4 2] [-1 -1  -1 1  1 1   1 -1 ]))   ;=> 1
;; (INSIDE [1  1.1] (p [4 2] [-1 -1  -1 1  1 1   1 -1 ]))   ;=> 0

;;
;; Section 18.0 Subscription
;;

;; APL's and accordingly alps' subcription operator is more general
;; than that of any other programming language (as of 1977). It is
;; possible, for example, to subscript a vector by an appropriate
;; array of any rank. The power of this operator is demonstrated by
;; the following examples.

;; 1. Print out the string S with large characters (where CHARS is a
;; boolean. rank three array, having one (7x5) character per plane):

(defun Banner (S)
  (let* ((#IO 1) (A #p) ; printable characters from prims.al
	 (I (rav (ind A (RepChar " " "_" S)))) ; protect spaces
	 (Q (tr [2 1 3 2] (o '* (aref CHARS I ()()) I)))
	 (T (cat (aref (cat A "?") (+ Q (= Q 0))) " "))
	 (N (r '* (dp 1 (p T))))
	 (B (p {7 N} T))
	 (V (r '^ 1 (= B " ")))
	 ;; drop redundant and last blank column
	 (R (k (cat (dp -1 (~ (^ V (rot 1 V)))) 0) B)))
    (p (p R) (RepChar "_" " " (rav R))))) ; replace spaces

(defun LoadFont (F)
  (let ((#IO 1))
  (rev 2 (tr [2 1 3] (enc (p 7 2) (implod .5 (mapcar 'implod (readf F))))))))

;; (a CHARS (LoadFont "data/font"))
;;   
;;
;; (Banner "Hello Polly")
;; "H   H       ll  ll              PPPP        ll  ll       
;;  H   H        l   l              P   P        l   l       
;;  H   H  eee   l   l   ooo        P   P  ooo   l   l  y   y
;;  HHHHH e   e  l   l  o   o       PPPP  o   o  l   l  y   y
;;  H   H eeeee  l   l  o   o       P     o   o  l   l   yyyy
;;  H   H e      l   l  o   o       P     o   o  l   l      y
;;  H   H  eee  lll lll  ooo        P      ooo  lll lll  yyy "


;; 2. Create a checkerboard:

(defun CheckerBoard ()
  (let* ((X (p [2 3 5] (tr (p [15 2] "/ "))))
	 (Y (dp [0 -1] (p [8 9] [2 1]))))
    (p (* (p Y) (dp 1 (p X))) (tr [1 3 2 4] (aref X Y ()())))))

;; (CheckerBoard)
;; "     /////     /////     /////     /////
;;       /////     /////     /////     /////
;;       /////     /////     /////     /////
;;  /////     /////     /////     /////     
;;  /////     /////     /////     /////     
;;  /////     /////     /////     /////     
;;       /////     /////     /////     /////
;;       /////     /////     /////     /////
;;       /////     /////     /////     /////
;;  /////     /////     /////     /////     
;;  /////     /////     /////     /////     
;;  /////     /////     /////     /////     
;;       /////     /////     /////     /////
;;       /////     /////     /////     /////
;;       /////     /////     /////     /////
;;  /////     /////     /////     /////     
;;  /////     /////     /////     /////     
;;  /////     /////     /////     /////     
;;       /////     /////     /////     /////
;;       /////     /////     /////     /////
;;       /////     /////     /////     /////
;;  /////     /////     /////     /////     
;;  /////     /////     /////     /////     
;;  /////     /////     /////     /////     "

;; 3. Wallpaper

(defun Wallpaper (H W)
  (let* ((U (aref W (? (aref H 1) (p W))))
	 (X (tr [2 3 1] (p (aref H [2 2 1]) U)))
	 (Y (? (p (tk -2 H) (aref H 1)))))
    (p (* (p Y) (dp 1 (p X))) (tr [1 3 2 4] (aref X Y ()())))))

;; 4. Instead of using Q, as in idioms 3. and 4. of the previous
;; section, an array B can be generated from A (B is made of N KxK
;; arrays of all K consecutive rows of A. (p A) => {N K}.
;; (a B (aref A (rot (- (i K) 1) (tr (p {K N} (i N))))))

;;
;; Section 19.0 Problems
;;

;; 1. Symbol Table Update problem: A a vector of integers (the symbol
;; table), X an integer (the key), B is the usage count of key in A
;;	(a A (cat A (k (a Y (~ (elt X A))) X)))
;;      (a  B (+ (cat B (k Y 0)) (= X A)))
;; In the report the RHS of the assignment on the second line should read:
;; (B,Y/0)+X=A
;; Discussion: Y indicates whether or not A and B should be extended.
;; If Y is 1, the (k 1 xxx)'s yield X and 0, respectively.
;; Otherwise, the compressions yield the empty vector.
;; (= X A) is a logical vector indicating 0 should be added to all
;; elements of B except the one corresponding to X, which will be
;; incremented by 1.
;; The idiom is encapsulated by a closure below to avoid having A, B
;; and Y as global variables.
(defun mkST ()
  (let (A B Y
	(RST '(lambda () (a A [] B [])))
	(STU '(lambda (X)
		(a A (cat A (k (a Y (~ (elt X A))) X))
		   B (+ (cat B (k Y 0)) (= X A))))))
    (RST)       ;; reset values initially
    (fun STU))) ;; return a closure around the symbol table update function

(a mst (mkST))    ;; create a symbol table functional value (aka object) mst
;; STU returns the value of B 
;; (mst 4)         ;=> [1]
;; (mst 5)         ;=> [1 1]
;; (mst 4)         ;=> [2 1]
;; (mst 7)         ;=> [2 1 1]
;; (mst 7)         ;=> [2 1 2]
;; the value of A in the object
;; (se 'A mst)     ;=> [4 5 7]
;; reset the symbol table
;; (se '(RST) mst) ;=> []
;; (se 'A mst)     ;=> []

;; 2. Replication: Given 2 vectors X and Y, (eq (p X) (p Y)). X a set
;; of elements and Y a vector of positive integers controlling the
;; replication
(defun Rep (X Y)
  (aref X (s '+ (elt (i (r '+ Y)) (dp -1 (+ 1 (s '+ (cat 0 Y))))))))
;; We could instead just use the built-in function compress/replicate: k
;; (eql (Rep (i 3) (i 3)) (k (i 3) (i 3))) ;=> t
;; (Rep (i 3) (i 3)) ;=> [1 2 2 3 3 3]

;; 3. KWIC: Given a list of titles (of papers, books, etc.), it is
;; desired to create a "Key Word In Context" (KWIC) index. Each title
;; will occur in the index as many times as it contains key words. The
;; titles are to appear in alphabetical order by the key word being
;; emphasized on that line. On input, L is a list of non-key words to
;; be ignored, and A is the list of titles.

(defun KWIC (L A)
  (let (V G F FF H K C U Z R B D S P M J N)
    (a U (~ (elt (cat " " A) ",;.:?! "))
       G (> U (rot (p (tally  A) -1) U))
       F (r '+ G)
       FF (r '+ F)
       H (+ -2 (k (rav G) (rav (p (p U) (i (dp 1 (p U)))))))
       K (+ 1 (r '+ (o '> (i FF) (s '+ F))))
       V (rav (cat U 0))
       C (<> V (rot -1 V))
       Z (| (r '- -1 -1 (p (cat (/ (r '+ C) 2) 2) (k C (i (p V))))))
       R (r 'c Z)
       B (p (cat FF R) (ex (rav (o ' >= Z (i R)))
			   (k (rav (dp [0 1] U)) (rav A))))
       D (dp 1 (c (p B) (p L)))
       S (r '^ (. 'v '<> (tk {FF D} B) (tr (tk (cat (tally  L) D) L))))
       M (k 1 S B)
       P (gup (dec (p (dp 1 (p M)) 53) (tr (+ -1 (ind #a M)))))
       J (aref (k 1 S H) P)
       N (rot J (cat (aref A (aref (k 1 S K) P) ()) (p (cat (p J) 5) " "))))
    (rot (- (f (/ (dp 1 (p A)) 2))) (cat (p (cat (p J) 3) " ") N))))

(defun TKWIC () ;; Quick test of KWIC
  (let ((A (ReduceL Expand
 	     (list (p [0 0]"")
 "Characterisation of Through Spectra of silicon micro-rings"             
 "The unified modeling language"                                                
 "On the order of parameters in varyadic functions"
 "CMOS fabrication of silicon micro-rings"
 "The use of finite difference time domain modeling for the characterisation of  silicon micro-rings"
 "APL idioms for idiots"
 "Zettabyte file systems for non volatile memory systems")))
	(L (MakeArray "The the of on a an for use in functions ")))
    (KWIC L A)))

;; 4. FIND: Given an array B of arbitrary rank and an array A that will "fit"
;; into B (i.e.  (rank B) >= (rank A) and (tk (- (p A))) >= (p A)), it
;; is desired to find all of the places in B where A matches. A match
;; is indicated by giving the coordiantes of its "upper left hand
;; corner". This could be called the multi-dimension pattern matching
;; problem.
(defun FIND (A B) "FIND returns top left hand coords of B where A \"fits\" in B"
  (let* ((D (p (cat (p (- (rank B) (rank A)) 1) (p A)) A))
	 (K (+ 1 (- (p B) (p D))))
	 (U (+ 1 (enc K (+ -1 (i (r '* K))))))
	 (V (o '+ U (enc (p D) (+ -1 (i (p (rav D)))))))
	 (H (+ 1 (dec (p B) (+ -1 (tr [1 2 1 3] V)))))
	 (C (aref (rav B) H)))
    (k (. '^ '= C (rav A)) U)))
;; (tr (FIND (+ (p [2 3] (i 6)) (p [2 1] [12 14])) (p [5 5] (i 25)))) ;=> [3 3]

;; 5. Max sets. Let S be a vector of elements. Let V be another vector
;; such that (eq (p S) (p V)). (aref V I) tells which set (aref S I)
;; belongs to. That is, V contains all of the positive integers in the
;; set (i (r 'c V)), possibly including duplications. It is desired to
;; return a vector W containing the maximum element of each set.
(defun MaxSet (S V) (. 'c '* S (o '= V (i (r 'c V)))))
;; (MaxSet (i 10) [1 1 2 2 2 1 1 2 2 1]) ;=> [10 9]

;; 6. Find the integers <=N whose squares have decimal expansions that
;; are palindromes.
(defun FSPalin (N)
  (let* ((Y (tr (enc (p (+ 1 (f (l 10 (+ 1 (sqr N))))) 10) (exp (i N) 2))))
	 (T (= Y (rot  (- (r '+ (s '^ (= Y 0)))) (rev Y)))))
    (k (r '^ T) (i N))))
;; (FSPalin 100)       ;=> [1 2 3 11 22 26]
;; (sqr (FSPalin 100)) ;=> [1 4 9 121 484 676]

;; 7. Ulam's spiral.
;; Create Ulam's spiral of primes.  See Scientific American, Vol 210,
;; Number 3, March 1964, pp 120-128 Begin by creating a rectangular
;; array, Y, containing the integers (i (p (rav Y))) in the following
;; configuration:
;;             [9 2 3
;;              8 1 4
;;              7 6 5]
;; Notice how the integers increase as they spiral out from the center.
;; Now, replace all the prime numbers by *'s and all of the composite
;; numbers by -'s. The result is a dramatic presentation of the distribution
;; of prime numbers.
;; In the solution below, SPIRAL builds Y and the last line performs
;; the rest.
(defun Ulam (N)
  (let* ((#IO 1)
	 (LINEAR '(lambda (N C) (+ 1 (dec (cat N N) (- C 1)))))
	 (IsPrime '(lambda(X) (elt X (PRIMES (r 'c (rav X))))))
	 (SPIRAL
	  '(lambda (N)
	     (let* ((A (i (* N 2)))
		    (C (| 4 (k (| (s '- 1 -1 A)) A))) ; note apl scan semantics
		    (G (f (+ .5 (/ N 2))))
		    (D (+ C (* 4 (= 0 C))))
		    (L (p [2 4] [-1 0 1 0 0 1 0 -1]))
		    (E (aref L () (tk  (- (* N N) 1) D))))
	       (p (cat N N) (gup (LINEAR N (tr [1 1 2] (o '+ (cat G G)
							  (s '+ (cat 0 E)))))))))))
    (aref "-*" (+ 1 (IsPrime (SPIRAL N))))))
;; (eql (Ulam 5) (p [5 5] "--*-*--**-*-----*-*---*-*"))  ;=> t
;; (Ulam 10)
;; "-*-----*--
;;  ----*-----
;;  -----*-*--
;;  *---*-*-*-
;;  -*--**----
;;  --*-----*-
;;  ---*-*---*
;;  ----*-*---
;;  -*-*---*--
;;  *-*---*---"

;; The function Ulam above is a direct translation of the APL code into alps.
;; Examinining the function reveals that it entails some useless
;; operations, probably remnants from the experimentation phase.
;; The (+ 1 ..) in LINEAR does not change the result of (gup)
;; The (tr [1 1 2] (o '+ (cat G G) (s '+ (cat 0 E)))) can be simplified
;; to (+ G (s '+ (cat 0 E)) since the subarrays of the first dimension
;; of the outer product must be equal.
;; Ulam2 removes these redundancies and incorporates LINEAR into SPIRAL

(defun Ulam2 (N)
  (let* ((#IO 1)
	 (IsPrime '(lambda(X) (elt X (PRIMES (r 'c (rav X)))))))
	 (a SPIRAL
	  '(lambda (N)
	     (let* ((A (i (* N 2)))
		    (C (| 4 (k (| (s '- 1 -1 A)) A))) ; note apl scan semantics
		    (G (f (+ .5 (/ N 2))))
		    (D (+ C (* 4 (= 0 C))))
		    (L (p [2 4] [-1 0 1 0 0 1 0 -1]))
		    (E (aref L () (tk  (- (* N N) 1) D))))
	       (p (cat N N) (gup (dec (cat N N) (+ G -1 (s '+ (cat 0 E)))))))))
    (aref "-*" (+ 1 (IsPrime (SPIRAL N))))))

;; Testing Ulam against Ulam2 on the first 20 positive integers
;;(apply 'and (mapcar  '(lambda (X) (eql (Ulam X) (Ulam2 X))) (explod (i 20))))
;; => t

;; Others.

(defun Monotonic (F X) (r '^ (F (dp -1 X) (dp 1 X)))) ; test for monotonicity
;; monotonically increasing
;; (Monotonic '<= [1 2 3 3 4 5]) ;=> 1
;; strictly monotonically increasing
;; (Monotonic '< [1 2 3 3 4 5])  ;=> 0

; Convert Roman numerals to Arabic
(defun R2A (R) "Roman to Arabic"
  (let ((N (cat 0 (aref [1000 500 100 50 10 5 1] (ind "MDCLXVI" R)))))
    (r '+ (* N (exp -1 (< N (rot 1 N))))))) 
;; (R2A "MCMLIV") ;=> 1954

;; Convert Arabic to Roman numerals
(defun A2R (A) "Arabic to Roman"
  (let* ((#IO 0) (I (enc [0 1000] A))
	 (R (enc [0 5] (enc [10 10 10 10] (a A (aref I 1)))))
	 (S (rot 1 (aref R 0 ()) (= R 4)))
	 (T (c 0 (- R (o '* [1 3] (= 4 (aref R 1 ()))))))
	 (U (rav (tr (o '< (i 4) (rav (tr (cat 1 T S))))))))
    (cat (p (aref I 0) "M") (k U (rav (tr (p [4 16] "xMxxDCMDLXCLVIXV")))))))
;; (A2R 1954) ;=> "MCMLIV"

(de FirstRow (X) ; of an array of arbitrary rank
    (apply 'aref X (mapcar '(lambda (Y) (if (lt Y (rank X)) 1  nil))
                                             (explod (i (rank X))))))

;; Convert character array to lower case
(defun LowerCase (X) "Convert char arg X to lower case"
    (let ((N (num X))) (chr (+ N (* 32 (^ (> N 64) (< N 91)))))))
;; (LowerCase "TheCamelIsNoMore") ;=> "thecamelisnomore"

;; Convert character array to upper case
(defun UpperCase (X)  "Convert char arg X to upper case"
    (let ((N (num X))) (chr (- N (* 32 (^ (> N 96) (< N 123)))))))
;; (UpperCase "please don't shout")  ;=> "PLEASE DON'T SHOUT"

(defun LeftBit (A) (Scan '< A))
(defun Scan (Op A)  ; explict APL style scan
  (implod 1.5 (mapcar '(lambda (X) (r Op -1 -1 X))
		      (mapcar '(lambda (Y) (tk {(dp -1 (p A)) Y} A))
			      (explod (i (tk -1 (p A))))))))
;; (LeftBit [0 0 1 1 0 1 1 0])   ;=> [0 0 1 0 0 0 0 0]
;; (eql (s '< -1 -1 [0 0 1 1 0 1 1 0]) (LeftBit [0 0 1 1 0 1 1 0])) ;=> t

;; Reflect a character matrix X about axis A
(defun Reflect (A X) (cat A X (rev A X)))
;; (Reflect 1 (Banner "Hello Polly!"))
;; "H   H       ll  ll              PPPP        ll  ll        !
;;  H   H        l   l              P   P        l   l        !
;;  H   H  eee   l   l   ooo        P   P  ooo   l   l  y   y !
;;  HHHHH e   e  l   l  o   o       PPPP  o   o  l   l  y   y !
;;  H   H eeeee  l   l  o   o       P     o   o  l   l   yyyy !
;;  H   H e      l   l  o   o       P     o   o  l   l      y  
;;  H   H  eee  lll lll  ooo        P      ooo  lll lll  yyy  !
;;  H   H  eee  lll lll  ooo        P      ooo  lll lll  yyy  !
;;  H   H e      l   l  o   o       P     o   o  l   l      y  
;;  H   H eeeee  l   l  o   o       P     o   o  l   l   yyyy !
;;  HHHHH e   e  l   l  o   o       PPPP  o   o  l   l  y   y !
;;  H   H  eee   l   l   ooo        P   P  ooo   l   l  y   y !
;;  H   H        l   l              P   P        l   l        !
;;  H   H       ll  ll              PPPP        ll  ll        !"
;;(Reflect 2 (Banner "Hello"))
;; "H   H       ll  ll              ll  ll       H   H
;;  H   H        l   l              l   l        H   H
;;  H   H  eee   l   l   ooo  ooo   l   l   eee  H   H
;;  HHHHH e   e  l   l  o   oo   o  l   l  e   e HHHHH
;;  H   H eeeee  l   l  o   oo   o  l   l  eeeee H   H
;;  H   H e      l   l  o   oo   o  l   l      e H   H
;;  H   H  eee  lll lll  ooo  ooo  lll lll  eee  H   H"


;; Determinants for matrix order 2
(defun det (X) (r '- (r '* 1 (rot [0 1] X))))
(defun d2 (X) (- (r '* (tr [1 1] X)) (r '* (tr [1 1] (rot 1 X)))))
(defun d3 (X) (. '- '* (rot 1 (aref X () 1))  (aref X () 2)))

;; Recursively apply function F N times to V and produce a matrix of results
(de FoFM (F V N)
    (a V (p {1 (p (rav V))} V)) ;; force initial vector into matrix shape
    (if (zerop N) V (cat 1 V (FoFM F (F V) (- N 1)))))

;; Recursively apply function F N times to V and produce a list of results
(de FoFL (F V N) (if (zerop N) (list V) (cons V (FoFL F (F V) (- N 1)))))


(de FoF (F V C) (let ((R (F V))) (repeat (C (a V (F V)) R) (a R V))))

;; Generate the array of RxC submatrices of matrix A (Patch R C A) -> Res
;; (p Res) -> {(r '* 'f (/ (p A) (cat R C))) R C}
(defun Patch (R C A)
  (let* ((SD (cat R C))         ;; dimensions of the submatrix
	 (Fit (f (/ (p A) SD))) ;; how many submats fit in A [down across]
	 (S (r '* Fit))         ;; number of submats
	 (T (* SD Fit))         ;; Chunk to take out of A
         (LR (tk -1 T)))        ;; Length of row in chunk
    (tr [1 3 2] (p {S C R} (tr [1 3 2] (p {(* (/ S LR) C) R LR} (tk T A)))))))
