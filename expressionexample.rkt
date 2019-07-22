#lang racket
;Good flag for debugging -> transparent
(struct mult (e1 e2) #:transparent)
(struct add (e1 e2) #:transparent)

;This is a compiler to some degree...
;Converting syntaxes into different representations
(define (eval expr)
  (cond [(mult? expr) (* (eval(mult-e1 expr)) (eval(mult-e2 expr)))]
        [(add? expr) (+ (eval (add-e1 expr)) (eval(add-e2 expr)))]
        [(number? expr) expr]
        [else error "Uknown operation"]))