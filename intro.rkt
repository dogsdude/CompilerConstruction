#lang racket
;let example
(let ([x 5])
  (let ([y x])
  (+ y x)))

;swap y & x
(let ([x 5]
      [y 6])
  (let ([x y]
        [y x])
    x))

;let* example
(let* ([x 5]
       [y x])
  (+ y x))

;(define (bar) z)

(define b 3)
(set! b 6)

#;(define (sumlist xs)
  (if(list? xs)
     (if(null? xs)
        0
        (if (number? (first xs))
            (+ (first xs) (sumlist (rest xs)))))))

(define fact
  (lambda (n)
    (if (= n 0)
        1
        (* n ( fact (- n 1))))))

(define (cube n)
  (* n (* n n)))

(define (cube2 n)
  (* n n n))

(define (fact2 n) (if (= n 0)
                      1
                      (* n (fact2 (- n 1)))))

(define (fact3 n)
  (if (= n 0)
     (1)
       (* n (fact3 (- n 1)))))

(define foo
  (lambda () (1 2)))
