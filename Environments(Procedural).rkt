#lang racket

;Procedural definition of an environment
(define(empty-env)
  (lambda(searchvar)
    (error "No Binding Found" searchvar)))


(define (extend-env savedvar savedval savedenv)
  (lambda (searchvar)
    (if (equal? searchvar savedvar)
        savedval
    (apply-env savedenv searchvar))))

(define (apply-env env searchvar)
  (env searchvar))