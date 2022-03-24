#lang racket
(require "Scanner.rkt")

; everything will be passed in the next token and the rest of the list

(define (addop token stringList)
  (if (or (equal? token "+")(equal? token "-")) (append #t stringList) #f))

(define (multop token stringList)
  (if (or (equal? token "+")(equal? token "-")) (append #t stringList) #f))

(define (factor token stringList)
  (cond
    [(equal? token "Number") (append #t stringList)]
    [(equal? token "ID") (append #t stringList)]
    [(equal? token "(") (factorE (expr (first (scanner stringList)) (rest (scanner stringList)))) ]
    [else #f]))

(define (factorE stringList)
  (if (and (equal? (first stringList) #t) (equal? (first(rest stringList)) #\))) (append (list #t) (rest(rest stringList))) (list #f) ) ); you will need to exchange this false with an expression that reutrns false and the line number
;also this up here might give you issues if the rest of the string list is empty, because if it is then you might have an error. 

(define(expr token stringList)
  (if (equal? token "expr") (append (list #t) stringList) (list #f))
    )

(factor "(" (rest (readFi "hi.txt")))
(expr (first (scanner (readFi "hi.txt"))) (rest (scanner (readFi "hi.txt"))))
(factorE (expr (first (scanner (readFi "hi.txt"))) (rest (scanner (readFi "hi.txt")))))


;the question is, when passing something up the chain how do I scrape off the true to see if I should continue without having to call the function
;again to get the data? Answer always calling the nonterminal function inside of the paramater list of the next non-terminal and if it is false
;immediately return flase and the line of the error which will be appended to the end of the scanners list
