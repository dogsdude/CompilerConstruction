#lang racket
;Multiplication
(define (my-mult x y-promise)
  (cond [(= x 0) 0]
        [(= x 1) (my-force y-promise)]
        [else (+ (my-force y-promise)
                 (my-mult (- x 1) y-promise))]
        ))

;Keep track of expensive calculation
;(define (my-delay fun)
;  (mcons #f fun))

(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e)
     (mcons #f (lambda() e))]))

;Crazy linked list (delaying and forcing something)
(define (my-force th)
  (cond [(mcar th) (mcdr th)]
        [else
         (set-mcar! th #t)
         (set-mcdr! th ((mcdr th)) )
         (mcdr th)]
        ))
         
(define x (cons 14 null))
(define y x)
(set! x (cons 42 null))

(define a (mcons 20 null))
(set-mcar! a 10)
(define b (mcons 30 null))
(set-mcdr! a b)

(if (< 5 3) "Hello" "Goodbye")

(define (my-if b t f) (if b (t) (f) ))

;Delays our executions (thunk)
(define (factorial x)
  (my-if (= x 0)
         (lambda () 1)
         (lambda () * x (factorial (- x 1)))))

;Stream maker
(define (stream-maker fn arg)
  (letrec ([f (lambda (x)
                (cons x
                      (lambda () (f (fn x arg)))))])
       (lambda () (f arg))))

(define nats* (stream-maker + 1))
(define powers* (stream-maker * 2))

;Stream = some infinite pattern of things
(define ones (lambda () (cons 1 ones)))

;Can't be a normal let, has to be a letrec
(define nats (letrec ([f (lambda (x)
                        (cons x
                              (lambda () (f (+ x 1)))))])
               (lambda () (f 1))))

;Write a function that processes a list
(define (number-until stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      ans
                      (f (cdr pr) (+ ans 1)))))])
    (f stream 1)))

;Somehow build this into a list...?
(define (list-until stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      (reverse ans)
                      (f (cdr pr) (cons (car pr) ans)))))])
    (f stream '() )))
    