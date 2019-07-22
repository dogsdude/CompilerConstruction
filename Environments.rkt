#lang racket

(provide (all-defined-out))

;Build empty environment
(define (empty-env)
  ;Has the empty symbol in it!
  (list 'empty))

;Build extended environment
(define (extend-env var val enc)
  (list 'extend-env var value env))

;Build applied environment ->data structure way
(define (apply-env -env searchvar)
  ;Is the first thing empty?
  (cond ((equal? (first env) 'empty) (error "No bindin for" searchvar))
  ;Is the first thing the extended environment?
        ((equal? (first env) 'extend-env)
         (let ((savedvar (cadr env))
              ((savedval (caddr env))
              ((savedenv (cadddr env))))
              (if (equal? searchvar savedvar)
                  savedval
                  (apply-env savedenv searchvar))))
         (else (error "INVALID ENV")))))


  