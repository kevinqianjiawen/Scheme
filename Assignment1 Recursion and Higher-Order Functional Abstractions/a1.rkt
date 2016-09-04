#lang racket
;1.countdown: natural number -> list
; Define and test a procedure countdown
;that takes a natural number and returns
;a list of the natural numbers less than or
;equal to that number, in descending order.
(define countdown
  (lambda (n)
    (cond
      [(zero? n) (cons n empty)]
      [else (cons n (countdown (sub1 n)))])))


;2. insertR: symbol symbol list -> list
;Define and test a procedure insertR that takes two symbols
;and a list and returns a new list with the second symbol
;inserted after each occurrence of the first symbol
(define insertR
  (lambda (x y ls)
    (cond
      [(empty? ls) empty]
      [(eqv? x (first ls)) (cons (first ls) (cons y (insertR x y (rest ls))))]
      [else (cons (first ls) (insertR x y (rest ls)))])))



;3. remv-1st: symbol list -> list
;Define and test a procedure remv-1st that takes a a symbol
;and a list and returns a new list with the first occurrence of the symbol removed
(define remv-1st
  (lambda (s ls)
    (cond
      [(empty? ls) empty]
      [(eqv? s (first ls)) (rest ls)]
      [else (cons (first ls) (remv-1st s (rest ls)))])))


;4. list-index-ofv?: sysmol list -> index
;Define and test a procedure remv-1st that
;takes a a symbol and a list and returns a new list
;with the first occurrence of the symbol removed
(define list-index-ofv?
  (lambda (s ls)
    (cond
      [(empty? ls) -1]
      [(eqv? s (first ls)) 0 ]
      [else (add1 (list-index-ofv? s (rest ls)))])))



;5. filter: function ls -> blooean
;Define and test a procedure filter that takes a predicate
;and a list and returns a new list containing the elements that satisfy the predicate.
;A predicate is a procedure that takes a single argument and returns either #t or #f. The number? predicate,
;for example, returns #t if its argument is a number and #f otherwise. The argument satisfies the predicate,
;then, if the predicate returns #t for that argument.
(define filter
  (lambda (f ls)
    (cond
      [(empty? ls) empty]
      [(f (first ls)) (cons (first ls) (filter f (rest ls)))]
      [else (filter f (rest ls))])))



;6. zip: ls ls -> ls
;Define and test a procedure zip that takes two lists and forms a new list,
;each element of which is a pair formed by combining the corresponding elements of the two input lists.
;If the two lists are of uneven length, then drop the tail of the longer one.
(define zip
  (lambda (ls1 ls2)
    (cond
      [(empty? ls1) empty]
      [(empty? ls2) empty]
      [else (cons (cons ((first ls1) (first ls2)) (zip (rest ls1) (rest ls2))))])))




;7. map
;Define and test a procedure map that takes a procedure p of one argument
;and a list ls and returns a new list containing the results of applying p to the elements of ls
;Do not use Racket's built-in map in your definition.
(define map
  (lambda (f ls)
    (cond
      [(empty? ls) empty]
      [else (cons (f (car ls)) (map f (cdr ls)))])))


;8. append
;Define and test a procedure append that takes two lists, ls1 and ls2, and appends ls1 to ls2.
(define append
  (lambda (ls1 ls2)
    (cond
      [(empty? ls1) ls2]
      [else (cons (first ls1) (append (rest ls1) ls2))])))



;9. reverse
;Define and test a procedure reverse that takes a list and returns the reverse of that list.
(define reverse
  (lambda (ls)
    (cond
      [(empty? ls) empty]
      [else (append (reverse (rest ls)) (first ls))])))



;10. fact
;Define and test a procedure fact that takes a natural number and computes the factorial of that number.
;The factorial of a number is computed by multiplying it by the factorial of its predecessor.
;The factorial of 0 is defined to be 1.

(define fact
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n (fact (sub1 n)))])))
      

;11. memv
;Define and test a procedure memv that takes an element and a list and returns the first cdr whose car is eqv?
;to the element, or #f if the element is absent from the list.

(define memv
  (lambda (s ls)
    (cond
      [(empty? ls) #f]
      [(eqv? s (first ls)) ls]
      [else (memv s (rest ls))])))


;12. fib
;Define and test a procedure fib that takes a natural number n as input and computes the nth number,
;starting from zero, in the Fibonacci sequence (0, 1, 1, 2, 3, 5, 8, 13, 21, ...).
;Each number in the sequence is computed by adding the two previous numbers.

(define fib
  (lambda (n)
    (cond
      [(zero? n) 0]
      [(eqv? n 1) 1]
      [else  (+ (fib (- n 1)) (fib (- n 2)))])))


;13. The expressions (a b) and (a . (b . ())) are equivalent.
;Using this knowledge, rewrite the expression ((w x) y (z)) using as many dots as possible.
;Be sure to test your solution using Racket's equal? predicate.
;(You do not have to define a rewrite procedure; just rewrite the given expression by hand and place it in a comment.)

;  '((w . (x . ())) . (y . ((z . ()) . ())))




;14 binary->natural
; Define and test a procedure binary->natural that takes a flat list of 0s
;and 1s representing an unsigned binary number in reverse bit order and returns that number.
(define binary->natural
  (lambda (ls)
    (cond
      [(empty? ls) 0]
      [else (+ (+ (* 1 (first ls)) (* 2 (binary->natural (rest ls)))))])))






;15. minus
;Define subtraction using natural recursion.
;Your subtraction function, minus, need only take nonnegative inputs where the result will be nonnegative.
(define minus
  (lambda (n m)
         (cond
          [(zero? m) n]
          [else (sub1 (minus n (sub1 m)))])))



;16 div
;Define division using natural recursion. Your division function, div, need only work when the second number evenly divides the first.
;Division by zero is of course bad data.

(define div
  (lambda (n m)
    (cond
      [(zero? n) 0]
      [else (add1 (div (- n m) m))])))


;17 append-map
;Define a function append-map that, similar to map, takes both a procedure p of one argument
;a list of inputs ls and applies p to each of the elements of ls. Here, though,
;we mandate that the result of p on each element of ls is a list, and we append together the intermediate results.
;Do not use Racket's built-in append-map in your definition.

(define append-map
  (lambda (f ls)
    (cond
      [(empty? ls) empty]
      [else (append (f (first ls)) (append-map f (rest ls)))])))


;18 set-difference
;Define a function set-difference that takes two flat sets (lists with no duplicate elements)
;s1 and s2 and returns a list containing all the elements in s1 that are not in s2.

(define set-difference
  (lambda (ls1 ls2)
    (cond
      [(empty? ls1) empty]
      [(member (first ls1) ls2) (cons (first ls1) (set-difference (rest ls1) ls2))]
      [else (set-difference (rest ls1) ls2)])))



;19 powerset
;The procedure powerset takes a list and returns the power set of the elements in the list.
;The exact order of your lists may differ; this is accepta(ble.
(define powerset
  (lambda (ls)
    (cond
      [(empty? ls) '(())]
      [else (append (powerset (rest ls)) (map (lambda (xs) (cons (first ls) xs)) (powerset (rest ls))))])))

;20 cartesian-product
;The cartesian-product is defined over a list of sets (again simply lists that by our agreed upon convention don't have duplicates).
;The result is a list of tuples (i.e. lists). Each tuple has in the first position an element of the first set,
;in the second position an element of the second set, etc. The output list should contains all such combinations. The exact order of your tuples may differ; this is acceptable.
(define cartesian-product
  (lambda (ls)
    (cond
      [(empty? ls) '()]
      [(empty? ) '()]
      [else (append  (map (lambda (xs) (cons (first (first ls)) xs))(cartesian-product (rest ls))) (cartesian-product (first (rest ls))))])))
(cartesian-product '((5 4) (3 2 1)))


  ;(5 (3 2 1))
  ;((5 3) (5 2) (5 1))
  ;((5 4) (3 2 1))
  ;((5 3) (5 2) (5 1) (4 3) (4 2) (4 1)
    