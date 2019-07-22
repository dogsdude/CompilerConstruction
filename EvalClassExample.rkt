#lang racket
(require "AbstractSyntaxTreeClassExample.rkt"
         "EnvClassExample.rkt"
         "ParserClassExample.rkt")

(provide (all-defined-out))

;; Evaluation Function
(define (eval expression)
  (evalHelper expression (empty-env)))

;; Evaluation Helper Function
(define (evalHelper expr env)
  (match expr
    [(plus-expr e1 e2) (+ (evalHelper e1 env) (evalHelper e2 env))]
    [(multiply-expr e1 e2) (* (evalHelper e1 env) (evalHelper e2 env))]
    [(let-expr name val e) (evalHelper e
                                       (extend-env (identifier-expr-argone name)
                                                   (evalHelper val env)
                                                   env))]
    [(function-app funname parameter)
                (let ([fundef (apply-env env funname)])                                               ;; Get function definition from environment
                      (evalHelper (closer-body fundef)
                                  (extend-env (closure-parameter fundef)
                                              (evalHelper parameter env)
                                              (closure-env fundef))))];;
    
    [(lambda-expr parameter body) (closure parameter body env)]
    [(numeric-expr e) e]
    [(boolean-expr e) e]
    [(identifier-expr e) (apply-env env e)] 
    ))