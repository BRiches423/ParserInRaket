#lang racket
(require "Scanner.rkt")
; all functions within the parser will expect the following
; a list with the form: boolean boolean/string character_list number
; all functions aside from the error will output a list of the same form
; the error fucntion will expect a list of the same form and will output
; a list of #f, error_type, linenumber, char/string


;if there is an error this function will first check if the error is from further down the tree
;in which case it will just pass the error message up with no change
;or if this is the first call of error it will determine the correct message to pass up 
(define (error stringL)
  
  (cond
    [(not(first stringL))  stringL]
    [(boolean?(first (rest stringL)))  (list #f "Lexical error on line" (number->string(last stringL)) ": offending character:" (third stringL))]
   ; [else (list #f "Parse error on line " (last stringL))])
     [else (list #f "Parse error on line" (number->string(last stringL)) ": offending token:" (first( rest stringL)))])
)

;is the next token an addop
(define (add_op stringL)
  (cond
    [(not(first stringL)) (error stringL)]
    [(or (equal?(first(rest stringL)) "+") (equal?(first(rest stringL)) "-"))   (append (list #t) (scanner (rest (rest stringL))))]
    [ else (error stringL)]
    ))

;is the next token an assignment, technically I didn't need this but it was easier to code this way 
(define (assignment stringL)
  (cond
    [(not(first stringL)) (error stringL)]
    [(equal? (first(rest stringL)) ":=") (append (list #t) (scanner (rest (rest stringL))))]
    [ else (error stringL)]
    ))

;is the next token an ID, again I probably didn't need this but it was easier to have a function for most leaves
(define (isID stringL)
  (cond
    [(not(first stringL)) (error stringL)]
    [(equal? (first(rest stringL)) "ID") (append (list #t) (scanner (rest (rest stringL))))]
    [ else (error stringL)]
    ))

;leaf mult_op function
(define (mult_op stringL)
  (cond
    [(not(first stringL)) (error stringL)]
    [(or (equal?(first(rest stringL)) "*") (equal?(first(rest stringL)) "/")) (append (list #t) (scanner (rest (rest stringL))))]
    [ else (error stringL)]
    ))


;checks if the rest of the next tokens in the stream form a factor
(define (factor stringL)
  (cond
    [(not(first stringL)) (error stringL)]
    [(or (equal?(first(rest stringL)) "NUMBER") (equal?(first(rest stringL)) "ID")) (append (list #t) (scanner (rest (rest stringL))))]
    [(equal? (first(rest stringL)) "(") (endparen (expr (append (list #t) (scanner (rest (rest stringL))))))] ;expr is always the last in a nonterminal but for
    ;consistency we will add this #t to allow it to evaluate, this means that if the second term is a false it is a scanner error!
    [else (error stringL)]))

; this may have not been neccecary but I it was easier to make a function to check for the end parentheses on a factor in another fucntion
(define (endparen stringL)
  (cond
    [(not(first stringL)) (error stringL)]
    [(equal? (first(rest stringL)) ")") (append (list #t) (scanner (rest (rest stringL))))]
    [else (error stringL)]))

;checks to see if the next tokens are an expresson
(define (expr stringL)
  (cond
    [(not(first stringL)) (error stringL)]
    [(equal? (first (rest stringL)) "(") (term_tail(term stringL))]
    [(equal? (first (rest stringL)) "ID") (term_tail(term stringL))]
    [(equal? (first (rest stringL)) "NUMBER") (term_tail(term stringL))]
    [else (error stringL)]
    ))

;tests to see if the next tokens form a term_tail
(define (term_tail stringL)
  (cond
    [(not(first stringL)) (error stringL)]
    [(equal? (first(rest stringL)) "+") (term_tail(term(add_op stringL)))]
    [(equal? (first(rest stringL)) "-") (term_tail(term(add_op stringL)))]
    [(equal? (first(rest stringL)) ")") stringL]
    [(equal? (first(rest stringL)) "ID") stringL]
    [(equal? (first(rest stringL)) "READ") stringL]
    [(equal? (first(rest stringL)) "WRITE") stringL]
    [(equal? (first(rest stringL)) "$$") stringL]
    [else (error stringL)]
    ))

;checks if the next tokens form a term
(define (term stringL)
  (cond
    [(not(first stringL)) (error stringL)]
    [(equal? (first(rest stringL)) "(") (factor_tail(factor stringL))]
    [(equal? (first(rest stringL)) "ID") (factor_tail(factor stringL))]
    [(equal? (first(rest stringL)) "NUMBER") (factor_tail(factor stringL))]
    [ else (error stringL)]
    ))

;checks if the next tokens form a factor_tail
(define (factor_tail stringL)
  (cond
    [(not(first stringL)) (error stringL)]
    [(equal? (first (rest stringL)) "*") (factor_tail(factor(mult_op stringL)))]
    [(equal? (first (rest stringL)) "/") (factor_tail(factor(mult_op stringL)))]
    [(equal? (first (rest stringL)) "+") stringL]
    [(equal? (first (rest stringL)) "-") stringL]
    [(equal? (first (rest stringL)) ")") stringL]
    [(equal? (first (rest stringL)) "ID") stringL]
    [(equal? (first (rest stringL)) "READ") stringL]
    [(equal? (first (rest stringL)) "WRITE") stringL]
    [(equal? (first (rest stringL)) "$$") stringL]
    [else (error stringL)]
    ))

;tests to see if the next tokens form a statment
(define (stmt stringL)
  (cond
    [(not(first stringL)) (error stringL)]
    [(equal? (first(rest stringL)) "ID") (expr(assignment (append (list #t) (scanner (rest(rest stringL))))))]
    [(equal? (first (rest stringL)) "READ") (isID (append (list #t) (scanner (rest (rest stringL)))))]
    [(equal? (first (rest stringL)) "WRITE") (expr (append (list #t) (scanner (rest (rest stringL)))))]
    [else (error stringL)]))

;checks if the next tokens form a statement_list
(define (stmt_list stringL)
  (cond
    [(not(first stringL)) (error stringL)]; this gets repetetive, if I knew how to do templates in racket i would use them here
    [(equal? (first (rest stringL)) "ID") (stmt_list(stmt stringL))]
    [(equal? (first (rest stringL)) "READ") (stmt_list(stmt stringL))]
    [(equal? (first (rest stringL)) "WRITE") (stmt_list(stmt stringL))]
    [(equal? (first (rest stringL)) "$$") stringL ]
    [else (error stringL)]
    ))

;checks if the next tokens form a statment_list
(define (program stringL)
  
  (cond
  [(not(first stringL)) (error stringL)];I don't need this here technically but I really want to maintain consistency at this point
  [(equal? (first (rest stringL)) "ID") (stmt_list stringL)]
  [(equal? (first (rest stringL)) "READ") (stmt_list stringL)]
  [(equal? (first (rest stringL)) "WRITE") (stmt_list stringL)]
  [(equal? (first (rest stringL)) "$$") (list #t)]
  [else (error stringL)]))


;calls the error checking functions of parse3 and passes in the ACTUAL initial parse function: program
(define (parse2 charStream)
   (parse3 (program (append (list #t) (scanner charStream )))))

;opens the file and passes the appended list to the next phase
(define (parse fileName)
  (parse2  (readFi fileName) ))
  
;recieves the output of the parsing and determines if there is an error and what to output to the console
(define (parse3 stringL)
  (if (first stringL) "Accept" (string-join (rest stringL))))




