#lang racket
;Our syntax tree consturcted from running program, builds our structure

(provide (all-defined-out))

(struct plus-expr (argone argtwo) #:transparent)
(struct multiply-expr (argone argtwo) #:transparent)
(struct and-expr (argone argtwo) #:transparent)
(struct or-expr (argone argtwo) #:transparent)
(struct not-expr (argone argtwo) #:transparent)
(struct let-expr (id value body) #:transparent)