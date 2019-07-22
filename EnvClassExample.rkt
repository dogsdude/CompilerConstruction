#lang racket
(provide (all-defined-out))

;; Create a function which allows name lookups in
;; an Empty Environment
 (define (empty-env)
   (lambda (searchvar)
     (error "No Binding Found")))

;; Think stream that's adding pairs...
(define (apply-env env searchvar)
  (env searchvar))

;; Give me back functions (not data structures)
(define (extend-env var val env)
  (lambda (searchvar)
    (if (equal? searchvar var)
        val
        (apply-env env searchvar))))


  