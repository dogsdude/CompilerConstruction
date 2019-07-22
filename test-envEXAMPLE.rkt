#lang racket

(require "envEXAMPLE.rkt"
         test-engine/racket-tests)

;;; Splice together results of functions calls, not literal calls
(check-expect (empty-env) `(, (hash-copy #hash())))

(check-expect (extend-env (empty-env) 'x 5)
              `(, (hash-copy #hash((x . 5)))))

(check-expect (apply-env (empty-env) 'x) #f)

(check-expect (apply-env (extend-env (empty-env) 'x 5) 'x) 5)

;;; Test pushing and popping
;;; ` = don't eval    , = eval
(check-expect (push-scope (empty-env)) `(,(make-hash) , (make-hash)))
(check-expect (pop-scope (push-scope (empty-env))) `(, (make-hash)))

(test)