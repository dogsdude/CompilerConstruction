#lang racket

;Can't do this!
;(define my-if if)

;Similar to pattern matching in Haskell -> tells us what to do if we see it
;Syntatic sugar -> type if statements in a different way
;using these rules I can actually check things...
;Macro (define-syntax)
(define-syntax my-if
  (syntax-rules (then else) ;additional or new keywords
    [(my-if e1 then e2 else e3) (if e1 e2 e3)]
    ;[(my-if e1 then e2) ;(if e1 e2 e2)
    )) ;patterns

(my-if (= 5 2) then "hello" else "goodbye")


(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e)
     (mcons #f (lambda() e))]))

#; (define (double x)
     (* 2 x))

(define-syntax double1
  (syntax-rules ()
    [(double e) (* 2 e)]))

(define-syntax double2
  (syntax-rules ()
    {(double2 e) (+ e e)}))

(define-syntax double3
  (syntax-rules ()
    [(double3 e) (let ({x e})
                   (+ x x))]))

