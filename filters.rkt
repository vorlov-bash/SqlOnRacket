#lang racket


(require "table_parser.rkt")
(require "pprinter.rkt")
(require "where_utils.rkt")
(require "polish.rkt")
(require typed-stack)
(provide parse-SQL)


; "20" -> 20; "34"(string) -> 34(number)
(define (int-check hash-tableDF)
  (for-each (lambda (hash-of-table)
              (hash-for-each hash-of-table (lambda (k v)
                        (cond
                          [(string->number v)
                           (hash-set! hash-of-table k (string->number v))]))))
            hash-tableDF)
  hash-tableDF)


(define (find-pair pair-row key i)
  (cond
    [(equal? (car (list-ref pair-row i)) key) (list-ref pair-row i)]
    [else (find-pair pair-row key (+ i 1))]))

     
(define (select pair-tableDF columns)
  (cond
    [(equal? columns '("*")) pair-tableDF]
    [else
     (map (lambda (pair-row)
            (map (lambda (col)
                   (find-pair pair-row col 0))
                 columns))
          pair-tableDF)]))

(define (distinct DF column)
  (remove-duplicates DF (lambda (x y)
                          (equal? (hash-ref y column) (hash-ref x column)))))
    

(define (parse-SQL hash-query)
  (displayln hash-query)
  (define table-name (hash-ref hash-query "from"))
  
  (define hash-tableDF (perform-table-to-hashes (load table-name)))
  (displayln "hash-table")
  
  (define intDF (int-check hash-tableDF))
  (displayln "intDF")
  
  (define first-col (list-ref (string-split (hash-ref hash-query "select") ",") 0))
  (displayln "first-col")
  
  (define whereDF (cond
                    [(hash-has-key? hash-query "where")
                     (where (where-to-polish (string-split (hash-ref hash-query "where") " ") (make-stack) (make-stack)) (make-stack) 0 intDF)]
                    [else intDF]))
  (displayln "where")
  (cond
    [(empty? whereDF) (displayln "No mathes.")]
    [else
     (define selectDF (select (perform-hash-table-to-pair whereDF) (string-split (hash-ref hash-query "select") ",")))
     (pprint (perform-pair-table-to-list selectDF))]))
