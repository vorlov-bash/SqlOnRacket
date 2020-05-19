#lang racket


(require "table_parser.rkt")
(require "pprinter.rkt")
(require "where_utils.rkt")
(require "orderby_utils.rkt")
(require "agregate_utils.rkt")
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
  
  (define intDF (int-check hash-tableDF))
  
  (define first-col (list-ref (string-split (hash-ref hash-query "select") ",") 0))
  
  (define whereDF (cond
                    [(hash-has-key? hash-query "where")
                     (where (where-to-polish (string-split (hash-ref hash-query "where") " ") (make-stack) (make-stack)) (make-stack) 0 intDF)]
                    [else intDF]))
  
  (cond
    [(empty? whereDF) (displayln "No mathes.")]
    [else
     (define orderbyDF (cond
                         [(hash-has-key? hash-query "orderby")
                          (orderby whereDF (hash-ref hash-query "orderby"))]
                         [else whereDF]))
      
     (define distinctDF (cond
                          [(hash-has-key? hash-query "distinct")
                           (distinct orderbyDF (first (string-split (hash-ref hash-query "select") ",")))]
                          [else orderbyDF]))
     
     (define agregateDF (cond
                          [(hash-has-key? hash-query "agregate")
                           (agregate (hash-ref hash-query "agregate") distinctDF (hash-ref hash-query "select"))]
                          [else
                           distinctDF]))
     
     (cond
       [(hash-has-key? hash-query "agregate") (hash-set! hash-query "select" (return-column-name (hash-ref hash-query "agregate") (hash-ref hash-query "select")))])
     
     (define selectDF (select (perform-hash-table-to-pair agregateDF) (string-split (hash-ref hash-query "select") ",")))
     (pprint (perform-pair-table-to-list selectDF))]))
