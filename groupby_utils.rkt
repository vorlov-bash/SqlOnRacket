#lang racket

(define lst (list (hash "col" 3 "row" 1 "title" "hello")
                  (hash "col" 3 "row" 4 "title" "lol")
                  (hash "col" 10 "row" 4 "title" "nope")
                  (hash "col" 3 "row" 5 "title" "eke")
                  (hash "col" 10 "row" 5 "title" "pop")))



(define (count-groupby DF column increment)
  (cond
    [(= (length DF) increment) DF]
    [else
     (let ([result (length (filter-map (lambda (x)
                                         (and (equal? (hash-ref x column) (hash-ref (list-ref DF increment) column)) x))
                                       lst))])
       (count-groupby
        (list-set DF increment (hash-set (list-ref lst increment) (string-join (list "COUNT(" column ")") "") result))
        column
        (add1 increment)))]))

