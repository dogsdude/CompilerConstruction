
#lang racket
;Sam Lindsey, Assignment4:Intro to Racket, 10/26/18

(require 2htdp/planetcute)
;downseries
;Function: downseries takes a step, a high, and a low (all numbers). It then creates a list of values from
; high to low stepping down by step stopping before the value we add is less than low.
;Parameters: 3 numbers, step, high, low
;Result: A list of values from high to low (disallowing lists where low > high, or where our value somehow steps below low)
(define (downseries step high low)
  (if (< high low)
      '()
      (append (list high) (downseries step (- high step) low))))


;meow-string-map
;Function: meow-string-map takes a list of strings and appends "meow" to each element of that list. It does this by
; mapping over our list with "string-append". We pass a lambda function (i) to string-append because it requires two parameters and all we have
; is a list to pass, instead of each element (which the lambda takes care of for us)
;Parameters: a list of strings
;Result: a list of string with "meow" appeneded to each element
(define (meow-string-map lst)
  (map (lambda (i) (string-append i "meow")) lst))
  

;list-ref-div
;Function: list-ref-div takes a list and a number and takes the number and divides it by
; the length of the list. It then takes that value (integer division) and uses it to find that position in the list
; and then returns the element at that position.
;Parameters: a list and a number
;Result: a element
(define (list-ref-div lst n)
  (if (< n 0)
      (error "list-ref-div: negative number")
      (if (null? lst)
          (error "list-ref-div: empty list")
          (list-ref lst (quotient n (length lst))))))

;next-k-items
;Function: next-k-items takes a stream (s) and a number (k) and takes n elements out of the stream (k). It does this
; by cons-ing together the car (head) of the recursive call of cdr (tail).... everytime we call our stream in the function
; definition we have to make sure that it is "deferenced" so that we are passing something that use car and cdr and not
; just a procedure
;Parameters: a stream and a k
;Result: list of k elements
(define nats (letrec ([f (lambda (x)
                        (cons x
                              (lambda () (f (+ x 1)))))])
               (lambda () (f 1))))

(define (next-k-items s k)
  (if (= k 0)
      '()
        (cons (car (s)) (next-k-items (cdr (s)) (- k 1)))))

;kth-item
;Function: kth-item is similar to next-k-items, but it only returns the last element. To only get the last element
; we instead make our base case kick us out when k = 1 instead of when it equals 0. This base case returns the car (head)
; of the stream and nothing else (which is what we want).
;Parameters: a stream and a number
;Result: element at position k
(define (kth-item s k)
  (if (= k 1)
      (car (s))
      (kth-item (cdr (s)) (- k 1))))

;negate-2-and-5
;Function: negate-2-and-5 works as a stream. We pass in a lambda function that checks to see if the modulo of our number when
; applied to 2 or 5 spits back a zero (ie is divisble by 2 or 5). If the modulo of either of those is 0, we negate the number we had and continue
; to work through our stream. We then cons that number to our "list" and recursively continue onward.
;Parameters: None
;Result: A stream of natural numbers where numbers divisible by 2 or 5 are negated
#; (define negate-2-and-5 (letrec ([f (lambda (x)
                                    (if (= (modulo x 2) 0)
                                         (cons (* x -1) (lambda () (f (+ x 1))))
                                      (if (= (modulo x 5) 0)
                                        (cons (* x -1) (lambda () (f (+ x 1)))) 
                                        (lambda () (f (+ x 1))))))])
                         (lambda () (f 1))))

(define negate-2-and-5 (letrec ([f (lambda (x)
                                     (if (or (= (modulo x 2) 0) (= (modulo x 5) 0))
                                         (cons (* x -1) (lambda () (f (+ x 1))))
                                     (cons x (lambda () (f (+ x 1))))))])
                         (lambda () (f 1))))
;key-heart-star
;Function: key-heart-star is a stream that produces images from our imported file and then makes a stream
; of them as heart, key, star (the order we should actually make instead of "key-heart-star". We define a lambda with no params
; and cons that to a heart, which cons' a lambda to a key, which cons' a lambda to a yellow-star which then calls f.... which starts over our recursion
; thus generating our stream
;Parameters: None
;Result: a stream of heart-key-star

(define key-heart-star (letrec ([f (lambda ()
                                     (cons heart (lambda ()
                                                   (cons key (lambda ()
                                                               (cons yellow-star f))))))])
                         (lambda () (f))))


;two-pairs-stream
;Function: two-pairs-stream is a stream that produces a pair of 2 & whatever the next element of the stream we pass in is. We do this by making a
; recursive lambda that cons' the cons of 2 and the car (head) of x (our stream) to the next recursive call of that same stream. The final result
; should be a list of every pair we construct when calling the stream.
;Parameters: A stream, s
;Result: a stream that pairs 2 with whatever the next element of the stream we pass is
(define (two-pairs-stream s)
  (letrec ([f (lambda (x)
               (cons (cons 2 (car x)) (lambda ()  (f ((cdr x))))))])
    (lambda () (f  (s) ))))

;spin-stream
;Function: spin-stream takes two lists and spits out a new stream that pairs together the first element of one list with
; the first element of the second and then the second with the second and so on. If both lists are the same size then spin-stream will
; output the same pairings over and over again, repeating from the beginning when the lists run out of elements. However, the stream still works if passed two lists
; of different sizes. For example, if given (1,2,3) (a, b) we will return (1,a) (2,b) (3,a) (1,b) and so on. We do this by maintaing the
; positions of each given list instead of the car (head) and cdr (tail). By taking a list of size 2 and saying modulo of 3 on that list (3 mod 2)
; we return position 1 and so that's the position of the element we want.... we cons together the respective element of each list (based on position)
; and then cons that with the lambda of the recursive call of our function. The reason we split the function into two parts here (help and spin-stream)
; is to keep track of our value (n) which initially starts at 0 (help xs ys 0) and then increments on every call of help (+ 1 n)... By including our lambda
; in each of the calls we handle cases where infinte recursion may occur!
;Parameters: two lists xs and ys
;Result: a stream that pairs the first position of list 1 with first position of list 2, then position 2 with position 2
; and so on until the list ends, and then we start at posiion one again. Lists of different sizes will end up being paired
; differently as the stream continues!
  
(define (spin-stream xs ys)
 (lambda() (help xs ys 0)))

(define (help xs ys n)
  (cons (cons (list-ref xs (modulo n (length xs))) (list-ref ys (modulo n (length ys))))
        (lambda () (help xs ys (+ 1 n)))))
  
;kvpv-lookup
;Function: kvpv-lookup is a function that takes a value (v) and a vector (vec). It then searches through
; our vector searching for values of the head of the pair of the current pair. If the head (car) of the current element
; of our vector matches our value we return that pair. If it does not we continue recursively through our vector either
; until we find a first value of a pair that matches the provided value or we run out of pairs to search (in which case we return false).
;Parameters: a value v, a vector vec
;Result: either the pair whoose first value matches our provided value or #f (false)
(define (kvpv-lookup v vec)
  (letrec ([f (lambda (x)
                (if (equal? x (vector-length vec))
                     #f
                (if (equal? v (car (vector-ref vec x)))
                    (vector-ref vec x)
                ((lambda () (f (+ x 1)))))))])
    ((lambda () (f 0)))))

;cached-lookup
;Function: 
;Parameters: a list and a number (the size of the cache)
;Result: a simulation of a cache
;(define (cached-lookup lst n)
;  (make-vector n [#f])
; if (kvpv-lookup 
                

;TEST!!
(require test-engine/racket-tests)

; 1)
; notice, the first argument to check-expect is our test, the second argument is the expected result
(check-expect (downseries 2 11 3) '(11 9 7 5 3))

; 2)
(check-expect (meow-string-map '("hi" "hello" "there")) '("himeow" "hellomeow" "theremeow"))

; 3)
; you should probably have one that exceeds the bounds of your list though, like 6
(check-expect (list-ref-div (list 1 2 3) 0) 1)
(check-expect (list-ref-div (list 1 2 3) 3) 2)

; 4)
; this assumes you have nats defined
(check-expect (next-k-items nats 3) '(1 2 3))

; 5)
(check-expect (kth-item nats 3) 3)

; 6)
(check-expect (next-k-items negate-2-and-5 7) '(1 -2 3 -4 -5 -6 7))

; 7)
(check-expect (next-k-items key-heart-star 2) (list heart key))

; 8)
(check-expect (kth-item (two-pairs-stream nats) 2) '(2 . 2))

; 9)
(check-expect (next-k-items (spin-stream '(1 2 3) '("a" "b")) 6)
              '((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a") (3 . "b")))

; 10)
(check-expect (kvpv-lookup 2 '#((1 . 1) (2 . 1))) '(2 . 1))

; Achievement 1)
(check-expect (next-k-items (spin-stream '("a" "b") '(1 2 3 4 5)) 3)
             '(("a" . 1) ("b" . 2) ("a" . 3)))

(test)