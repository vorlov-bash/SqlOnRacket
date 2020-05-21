#lang racket


(require "table_parser.rkt")
(require "pprinter.rkt")
(require "where_utils.rkt")
(require "orderby_utils.rkt")
(require "agregate_utils.rkt")
(require "joins_utils.rkt")
(require "polish.rkt")
(require typed-stack)
(provide parse-SQL)
(provide int-check)


(define (str-hashrow-to-int hash-row)
  (make-hash (hash-map hash-row (lambda (x y)
                                  (cond
                                    [(string->number y)
                                     (cons x (string->number y))]
                                    [else
                                     (cons x y)])))))
; "20" -> 20; "34"(string) -> 34(number)
(define (int-check hash-tableDF)
  (map (lambda (hash-of-table)
         (str-hashrow-to-int hash-of-table))
       hash-tableDF))


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
  
  (define joinDF (cond
                   [(or (hash-has-key? hash-query "inner_join") (hash-has-key? hash-query "full_outer_join") (hash-has-key? hash-query "right_join"))
                    (int-check (join-util hash-query))]
                   [else intDF]))
  
  (define whereDF (cond
                    [(hash-has-key? hash-query "where")
                     (where (where-to-polish (string-split (hash-ref hash-query "where") " ") (make-stack) (make-stack)) (make-stack) 0 joinDF)]
                    [else joinDF]))
  
  (define groupbyDF (cond
                      [(and (hash-has-key? hash-query "groupby") (not (hash-has-key? hash-query "agregate")))
                       (distinct (orderby whereDF (hash-ref hash-query "groupby")) (hash-ref hash-query "groupby"))]
                      [else whereDF]))
  
  (cond
    [(empty? groupbyDF) (displayln "No mathes.")]
    [else
     (define orderbyDF (cond
                         [(hash-has-key? hash-query "orderby")
                          (orderby groupbyDF (hash-ref hash-query "orderby"))]
                         [else groupbyDF]))
      
     (define distinctDF (cond
                          [(hash-has-key? hash-query "distinct")
                           (distinct orderbyDF (first (hash-ref hash-query "select")))]
                          [else orderbyDF]))
     
     (define agregateDF (cond
                          [(hash-has-key? hash-query "agregate")
                           (let ([re-agr (regexp-match #px"(count|med|sum)\\(([\\w\\d]*)\\)" (hash-ref hash-query "agregate"))])
                             (agregate (second re-agr) distinctDF (third re-agr)))]
                          [else
                           distinctDF]))
     (define selectDF (select (perform-hash-table-to-pair agregateDF) (hash-ref hash-query "select")))
     (pprint (perform-pair-table-to-list selectDF))]))
