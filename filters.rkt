#lang racket


(require "table_parser.rkt")
(require "pprinter.rkt")
(require "where_utils.rkt")
(require "orderby_utils.rkt")
(require "agregate_utils.rkt")
(require "joins_utils.rkt")
(require "groupby_utils.rkt")
(require "case_utils.rkt")
(require "polish.rkt")
(require typed-stack)
(provide parse-SQL)
(provide int-check)


(define (str-hashrow-to-int hash-row)
  (make-immutable-hash (hash-map hash-row (lambda (x y)
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
                      [(and (hash-has-key? hash-query "groupby") (hash-has-key? hash-query "agregate"))
                       (distinct (orderby (count-groupby whereDF (second (regexp-match #px"count\\(([\\w\\d]*)\\)" (hash-ref hash-query "agregate"))) 0) (hash-ref hash-query "groupby")) (hash-ref hash-query "groupby"))]
                      [else whereDF]))

  (define havingDF (cond
                     [(and (hash-has-key? hash-query "groupby") (hash-has-key? hash-query "agregate") (hash-has-key? hash-query "having"))
                      (filter-map (lambda (x)
                                    (and (= (hash-ref x (first (string-split (hash-ref hash-query "having") "="))) (string->number (second (string-split (hash-ref hash-query "having") "=")))) x))
                                  groupbyDF)]
                     [else groupbyDF]))
  
  (cond
    [(empty? havingDF) (displayln "No mathes.")]
    [else
     (define orderbyDF (cond
                         [(hash-has-key? hash-query "orderby")
                          (orderby havingDF (hash-ref hash-query "orderby"))]
                         [else havingDF]))
      
     (define distinctDF (cond
                          [(hash-has-key? hash-query "distinct")
                           (distinct orderbyDF (first (hash-ref hash-query "select")))]
                          [else orderbyDF]))
     
     (define agregateDF (cond
                          [(and (hash-has-key? hash-query "agregate") (not (hash-has-key? hash-query "groupby")))
                           (let ([re-agr (regexp-match #px"(count|med|sum)\\(([\\w\\d]*)\\)" (hash-ref hash-query "agregate"))])
                             (agregate (second re-agr) distinctDF (third re-agr)))]
                          [else
                           distinctDF]))
     (define caseDF (cond
                      [(hash-has-key? hash-query "case")
                       (case-util (take (hash-ref hash-query "case") (- (length (hash-ref hash-query "case")) 1)) agregateDF (last (hash-ref hash-query "case")))]
                      [else
                       agregateDF]))
                       
     (define selectDF (select (perform-hash-table-to-pair caseDF) (hash-ref hash-query "select")))
     (pprint (perform-pair-table-to-list selectDF))]))
