;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 10/09/06
;; CSCI-C311
;; Assignment 1


; 1) countdown takes a natural number n and returns a list of the natural
; numbers less than or equal to n.
(define countdown
  (lambda (x)
    (cond
      [(zero? x) '(0)]
      [else (cons x (countdown (sub1 x)))])))

; 2) insertL takes two symbols and a list and returns a new list with
; the second symbol inserted before each occurence of the first symbol.
(define insertL
  (lambda (x y ls)
    (cond
      [(null? ls) '()]
      [(equal? x (car ls)) (cons y (cons (car ls) (insertL x y (cdr ls))))]
      [else (cons (car ls) (insertL x y (cdr ls)))])))

; 3) remove-?s takes a list and returns a new list with the same elements
; as the original list, but with all instances of the symbol ? removed.
(define remove-?s
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(equal? (car ls) '?) (remove-?s (cdr ls))]
      [else (cons (car ls) (remove-?s (cdr ls)))])))

; 4) occurs-?s takes a list and returns the number of times the symbol ?
; occurs in the list.
(define occurs-?s
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(equal? (car ls) '?) (add1 (occurs-?s (cdr ls)))]
      [else (occurs-?s (cdr ls))])))

; 5) filter takes a predicate and a list and returns a new list containing
; the elements that satisfy the predicate.
(define filter
  (lambda (pred ls)
    (cond
      [(null? ls) '()]
      [(pred (car ls)) (cons (car ls) (filter pred (cdr ls)))]
      [else (filter pred (cdr ls))])))

; 6) zip takes two lists of equal length and forms a new list, each element
; of which is the two-element list formed by combining the corresponding
; elements of the two input lits.
(define zip
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) '()]
      [else (cons (cons (car ls1) (cons (car ls2) '())) 
        (zip (cdr ls1) (cdr ls2)))])))

; 7) map takes a procedure p of one argument and a list ls and returns a new
; list containing the results of applying p to the elements of ls.
(define map
  (lambda (proc ls)
    (cond
      [(null? ls) '()]
      [else (cons (proc (car ls)) (map proc (cdr ls)))])))

; 8) append takes two lists, ls1, and ls2 and appends ls2 to ls1.
(define append
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [else (cons (car ls1) (append (cdr ls1) ls2))])))

; 9) reverse takes a list and returns the reverse of that list.
(define reverse
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [else (append (reverse (cdr ls)) (list (car ls)))])))

; 10) fact takes a natural number and computes the factorial of that number.
(define fact
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n (fact (sub1 n)))])))

; 11) count-symbol takes a list of symbols and returns the number of
; symbols in the list.
(define count-symbols
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(list? (car ls)) (+ (count-symbols (car ls)) (count-symbols (cdr ls)))]
      [else (add1 (count-symbols (cdr ls)))])))

; 12) fib takes a natural number n as an input and computes the nth number,
; starting from zero, in the Fibonacci sequence
(define fib
  (lambda (n)
    (cond
      [(zero? n) 0]
      [(zero? (sub1 n)) 1]
      [else (+ (fib (sub1 (sub1 n))) (fib (sub1 n)))])))

; 13a) even-layers? takes an onion returns #t if the onion has an even
; number of layers and #f otherwise.
(define even-layers?
  (lambda (onion)
    (cond
      [(null? onion) #t]
      [else (odd-layers? (car onion))])))

; 13b)
;
(define odd-layers?
  (lambda (ogre)
    (cond
      [(null? ogre) #f]
      [else (even-layers? (car ogre))])))

; 14) cons-cell-count takes a data structre and returns the number of times
; that cons must be invoked to construct that data structure.
(define cons-cell-count
  (lambda (structure)
    (cond
      [(atom? structure) 0]
      [else (add1 (+ (cons-cell-count (car structure))
               (cons-cell-count (cdr structure))))])))

; 15) 
; ((w x) y (z))
; ((w . (x . ())) . (y . ((z . ()) . ())))

; 16) binary-to-decimal takes a falt list of 0's and 1's representing an 
; unsigned binary number in reverse bit order and returns the decimal
; representation of the number.
(define binary-to-decimal
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [else (+ (car ls) (* 2 (binary-to-decimal (cdr ls))))])))

; 17)
;(define fib-acc
;  (lambda (n x y)
;    (cond
;      [(zero? n) 0]
;      [else (fib-acc (sub1 n) ]

; fib-acc must take three arguments.
; But what three arguments do we feed it first?
; obviously n, but for x and y, are we to assume we feed it
; 0 and 1?  Seems too sloppy. :/


; 18) 

(define power
  (lambda (x n)
    (cond
      [(zero? n) 1]
      [(odd? n) (* x (power x (sub1 n)))]
      [else (* (power x (/ n 2)) (power x (/ n 2)))])))