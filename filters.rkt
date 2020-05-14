#lang racket


(require "table_parser.rkt")
(require "pprinter.rkt")
(provide parse-SQL)


(define (int-check tableDF)
  (for-each (lambda (hash-of-table)
              (hash-for-each hash-of-table (lambda (k v)
                        (cond
                          [(string->number v)
                           (hash-set! hash-of-table k (string->number v))]))))
            tableDF)
  tableDF)


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
     
      

(define (parse-SQL hash-query)
  (define table-name (hash-ref hash-query "from"))
  (define tableDF (perform-table-to-hashes (load table-name)))
  (define intDF (int-check tableDF))
  (define pair-tableDF (perform-hash-table-to-pair tableDF))
  (define selectDF (select pair-tableDF (string-split (hash-ref hash-query "select") ",")))
  (pprint (perform-pair-table-to-list selectDF)))
