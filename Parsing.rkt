#lang racket
;Parsing -> sentence structure
;Make a tree

(require parser-tools/yacc
         ;We put the lex: in front of our functions
         (prefix-in lex: parser-tools/lex)
         "Lexing.rkt"
         "AbstractSyntaxTree.rkt")

(provide (all-defined-out))

;Builds parser for us & specify rules
(define myparser
  (parser
   ;What is the start symbol? prog = start
   (start prog)
   ;How do we know when we are done parsing? EOF = done
   (end EOF)
   ;What are tokens that are allowed?
   (tokens names-and-values
           end-of-file
           math-operators
           bool-operators
           let-keywords
           parens)
   ;What to do when things go wrong? Not the best way to handle this... (Doesn't say where...) we'd have to teach our tokens where they failed... nope
   (error (lambda (tok-ok? tok-name tok-value)
            (printf "Parser error: token ~a value ~a"
                     tok-name
                     tok-value)))
   ;What are the rules? Crazy matching cond thing
   ;Everything that is programmed can be generated from the "prog"
   (grammar
          (prog
               [(LEFTPAREN PLUS prog prog RIGHTPAREN) (plus-expr $3 $4)]
               [(LEFTPAREN MULTIPLY prog prog RIGHTPAREN) (multiply-expr $3 $4)]
               [(LEFTPAREN AND prog prog RIGHTPAREN) (and-expr $3 $4)]
               [(LEFTPAREN OR prog prog RIGHTPAREN) (or-expr $3 $4)]
               [(LEFTPAREN NOT prog RIGHTPAREN) (not-expr $3)]
               [(LEFTPAREN LET LEFTPAREN IDENTIFIER NUMERIC RIGHTPAREN prog RIGHTPAREN)
                            (let-expr $4 $5 $7)]
               [(LEFTPAREN LET LEFTPAREN IDENTIFIER BOOLEAN RIGHTPAREN prog RIGHTPAREN)
                             (let-expr $4 $5 $7)]
               [(BOOLEAN) $1] 
               [(NUMERIC) $1]
               )
          )
   ))

;Passes in stream slowly
(define (parse in)
    (myparser (get-tokenizer in)))

(define (parsestr str)
  (let ([in (open-input-string str)])
    (parse in)))