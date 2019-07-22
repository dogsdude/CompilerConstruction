#lang racket

;Taking source code and breaking in into tokens!!
;Tokens -> classes or types, labeling
;Syntactic category -> representing different things!

(require parser-tools/lex
         ;import prefix-in, but put a : in front of every one of them
         (prefix-in : parser-tools/lex-sre))

;This makes things all publically available
(provide (all-defined-out))

;Name of the list folowed by all posibilties
(define-empty-tokens parens (LEFTPAREN
                             RIGHTPAREN))

(define-empty-tokens bool-operators (AND
                                     OR
                                     NOT))

(define-empty-tokens math-operators (PLUS
                                     MULTIPLY))

(define-empty-tokens comparison-operators (SAME
                                           NOTSAME
                                           SMALLER
                                           NOTSMALLER))
(define-empty-tokens if-keywords (IF
                                  THEN
                                  ELSE))

(define-empty-tokens let-keywords (LET))

(define-empty-tokens names-and-values (IDENTIFIER
                                       NUMERIC
                                       BOOLEAN))

(define-empty-tokens end-of-file (EOF))


;Rest of stuff...
(define-empty-tokens keywords (bool-operator math-operators if-keywords comparison-operators))

;Lexer -> takes no args, call function from other module "lexer"
;Takes arguments as parameters (rules) and builds thing to handle our language

;#\ -> single character
;BE CAREFUL ABOUT PARENS INSIDE LEXER
;Think of this as a long if statement...
;Most language making tools have something that works like this
(define mylexer
  (lexer
   [#\(                         (token-LEFTPAREN)]
   [#\)                         (token-RIGHTPAREN)]
   [(:or "And" "and")           (token-AND)]
   [(:or "Or" "or")             (token-OR)]
   [(:or "Not" "not")           (token-NOT)]
   [(:or "Plus" "plus")         (token-PLUS)]
   [(:or "Multiply" "multiply") (token-MULTIPLY)]
   [(:or "Let" "let")           (token-LET)]
   [(:or "Same" "same")         (token-SAME)]
   [(:or "Then" "then")         (token-THEN)]
   [(:or "If" "if")             (token-IF)]
   [(:or "Else" "else")         (token-ELSE)]
   [(:or "True" "true")         (token-BOOLEAN true)]
   [(:or "False" "fasle")       (token-BOOLEAN false)]
   [(:+ numeric)                (token-NUMERIC (string->number lexeme))]
   [(:: (:+ alphabetic) (:* (:or numeric alphabetic)))
             (token-IDENTIFIER lexeme)]
   [whitespace                  (mylexer input-port)] 
   [(eof)                       (token-EOF)]
   ))

;Don't have to worry about getting every single thing

;Helper functions for our lexer
;Produces function that lets us get tokens out of input stream
(define (get-tokenizer in)
  (λ () (mylexer in)))

;Calls lexer one thing at a time, continue doing this until we have nothing left (building a list as we go)
(define (lex in)
  (let ([tokenizer (get-tokenizer in)])
    (define (lex-function)
      (let ((tok (tokenizer)))
        (cond
          [(eq? tok (token-EOF)) null]
          [else (cons tok (lex-function))])))
    (lex-function)))

(define (lexstr str)
  (lex (open-input-string str)))

;Now... take stream of tokens and check if the context of the program is correct -> sentence structure
