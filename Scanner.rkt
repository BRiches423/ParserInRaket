#lang racket

(provide readFi)
(provide scanner)
(define (readFi fileName)
(append (string->list(port->string (open-input-file fileName))) (list "endoflist" 1))) ;turns file into character list with a delimiter and line counter appended at the end

; my delimiter is "endoflist"



(define(scanner slist)
  (cond

    [(boolean? slist) "Activating self destruct, cause im ded"]

    ;[(empty? slist) (list #f)]


    [(eqv? (first slist) #\newline) (scanner(endincrement(rest slist)))] ;ignores newlines but increments a counter at the end of the list
    
    [(eqv? (first slist) #\return)  (scanner(rest slist))] ;ignores carrage returns
    
    [(eqv? (first slist) #\space) (scanner (rest slist))] ;ignores spaces
    
  ;  [(equal? (first slist) "endoflist") (append (list #f) (list "endoflist") (rest slist))]

     [(equal? (first slist) "endoflist") (append (list "endoflist") (rest slist))] ;treats the delimiter as a token the parser will throw the error
     
    [(eqv? (first slist) #\/) (append (list "/") (rest slist))] ; these simbols are relatively easy to check  

    [(eqv? (first slist) #\*) (append (list "*" )(rest slist))]

    [(eqv? (first slist) #\-) (append (list "-") (rest slist))]

    [(eqv? (first slist) #\+) (append(list "+") (rest slist))]

    [(and(eqv? (first slist) #\$) (eqv? (first(rest slist)) #\$)) (append (list "$$") (rest(rest slist)))]

    [(eqv? (first slist) #\() (append(list "(" )(rest slist))]

    [(eqv? (first slist) #\)) (append(list ")" )(rest slist))]

    [(eqv? (first slist) #\=) (append (list #f "=") (rest slist))]

    [(eqv? (first slist) #\:) (if(and(not(empty? (rest slist))) (eqv? (first(rest slist)) #\=)) (append(list ":=" )(rest(rest slist))) (append (list #f ":") (rest slist)))]
    ; do we see a lone if so error if not 

    [(or(char-numeric? (first slist)) (eqv? (first slist) #\.)) (numbers (rest slist))] ;per instructions I didn't worry about a number with multiple decimals in it

    [(char-alphabetic? (first slist)) (words (list) slist)] 

    [else (append (list #f  (~a (first slist))) (rest slist))]  ;if the character finds an invalid character it will return a false, the offending character, and the rest of the list

    ))


(define (numbers slist)
  (if (and(not (empty? slist))(or(eqv? (first slist) #\.)(char-numeric? (first slist)))) (numbers (rest slist)) (append(list "NUMBER")  slist)))
;just deletes any numbers or . from the front of the list till a delimiter is reached then returns the token NUMBER with the rest of the list



(define (specialOrId slist)
  
  (cond
    [(equal? (list->string slist) "read") "READ"]
    [(equal? (list->string slist) "write") "WRITE"]
   
    [else "ID"]
  ));returns the correct token for a list known to contain an alphabetical string

(define (words idList sList)
  (if (cond
        [(eqv? (first sList) "endoflist") #f]
    
        [(char-alphabetic? (first sList)) #t]
        [(char-numeric? (first sList)) #t]
        ;[(eqv? (first sList) #\.) #t] ;wasn't sure if periods should be allowed in IDS but included jic, if so please uncomment
        ;[(eqv? (first sList) #\_) #t] ;wasn't sure if _ should be allowed in IDS but included jic, if so please uncomment
      
        [else #f]
        )
      (words (append idList(list(first sList)) ) (rest sList)) (append (list (specialOrId idList)) sList)) ;seperates a token that starts with alphabet and returns the ID READ or WRITE
      
       
)



(define (scannerTest tokenList stringList id) ;this is a test function that returns the entire token list

  (cond
    [(equal? id 0) (scannerTest tokenList (scanner (append stringList (list "endoflist" 1))) 1)]
    [(and(equal? id 1) (boolean? (first stringList)))  (append tokenList (list(last stringList)))]
    [else  (scannerTest (append tokenList(list (first stringList)) ) (scanner(rest stringList) )1)]
     ))

(define (endincrement sList)
  (reverse(append (list (+ (first (reverse sList)) 1)) (rest (reverse sList))))) ; this appends the line number to the end of the list




