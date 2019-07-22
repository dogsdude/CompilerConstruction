#lang racket

;Memoization, retaining the answers we get from fibonacci sequence
(define fibmem
  (letrec ([memotable null]
           [f (lambda (x)
                (let ([ans (assoc x memotable)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (or (= x 1) (= x 2))
                                         1
                                         (+ (f (- x 1))
                                            (f (- x 2))))])
                        (set! memotable (cons (cons x new-ans) memotable))
                        new-ans))))])
    f))


;Exponential (BAD)
(define (fib x)
  (if (or (= x 1) (= x 2) )
      1
      (+ (fib (- x 1))
         (fib (- x 2)))))

;Fast (GOOD)
(define (fib* x)
  (letrec ([f (lambda (acc1 acc2 y)
                (if (= y x)
                    (+ acc1 acc2)
                    (f (+ acc1 acc2) acc1 (+ y 1))))])
           (if (or (= x 1) (= x 2))
               1
               (f 1 1 3))))