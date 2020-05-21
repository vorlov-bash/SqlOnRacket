#lang racket
(provide perform-query)

(define (perform-query query)
  (let ([re-query (regexp-match #px"(select (?:distinct [\\w\\d\\s(),]*|[\\w\\d\\s(),*]*) from [\\w\\d\\/]*\\.(?:csv|tsv))(?:\\s?)(.*)" query)])
    (cond
      [(not re-query) (displayln "Wrong syntaxis")]
      [(not (empty? query))
       (define group1 (group1-query-to-hash (string-split (second re-query) " ") (hash)))
       (define group2 (cond
                        [(non-empty-string? (third re-query))
                         (group2-query-to-hash (string-split (third re-query) "->") group1)]
                        [else group1])) group2]
      [else (displayln "Wrong syntaxis")])))

(define (group1-query-to-hash query query-hash)
  (cond
    [(empty? query) query-hash]
    [(equal? (first query) "select")
     (cond
       ; Distinct
       [(equal? (second query) "distinct")
        (group1-query-to-hash (remove (second query) query) (hash-set query-hash "distinct" #t))]
       
       ; Default
       [else
        (group1-query-to-hash (list-tail query 2) (scan-columns (string-split (second query) ",") query-hash (list)))])]
    [(equal? (first query) "from")
     (group1-query-to-hash (list-tail query 2) (hash-set query-hash "from" (second query)))]))

    
(define (group2-query-to-hash query query-hash)
  (cond
    [(empty? query) query-hash]
    [(and (string-contains? (first query) "where") (not (hash-has-key? query-hash "where")))
     (group2-query-to-hash (remove (first query) query) (hash-set query-hash "where" (second (regexp-match #px"where\\((.*)\\)" (first query)))))]
    
    [(and (string-contains? (first query) "orderby") (not (hash-has-key? query-hash "orderby")))
     (group2-query-to-hash (remove (first query) query) (hash-set query-hash "orderby" (second (regexp-match #px"orderby\\((.*)\\)" (first query)))))]

    [(and (string-contains? (first query) "groupby") (not (hash-has-key? query-hash "groupby")))
     (group2-query-to-hash (remove (first query) query) (hash-set query-hash "groupby" (second (regexp-match #px"groupby\\((.*)\\)" (first query)))))]

    [(and (string-contains? (first query) "having") (not (hash-has-key? query-hash "having")))
     (group2-query-to-hash (remove (first query) query) (hash-set query-hash "having" (second (regexp-match #px"having\\((.*)\\)" (first query)))))]

    [(and (string-contains? (first query) "inner_join") (not (hash-has-key? query-hash "inner_join")))
     (group2-query-to-hash (remove (first query) query) (hash-set query-hash "inner_join" (second (regexp-match #px"inner_join\\((.*)\\)" (first query)))))]

    [(and (string-contains? (first query) "right_join") (not (hash-has-key? query-hash "right_join")))
     (group2-query-to-hash (remove (first query) query) (hash-set query-hash "right_join" (second (regexp-match #px"right_join\\((.*)\\)" (first query)))))]

    [(and (string-contains? (first query) "full_outer_join") (not (hash-has-key? query-hash "full_outer_join")))
     (group2-query-to-hash (remove (first query) query) (hash-set query-hash "full_outer_join" (second (regexp-match #px"full_outer_join\\((.*)\\)" (first query)))))]

    [else
     (displayln "Wrong syntaxis")]))

(define (scan-columns col_list query-hash result)
  (cond
    [(empty? col_list) (hash-set query-hash "select" result)]
    [(equal? (first col_list) "*") (hash-set query-hash "select" (list "*"))]
    [(regexp-match #px"med\\(\\w*\\)|count\\(\\w*\\)|sum\\(\\w*\\)" (first col_list))
     (scan-columns (remove (first col_list) col_list) (hash-set query-hash "agregate" (first (regexp-match #px"med\\(\\w*\\)|count\\(\\w*\\)|sum\\(\\w*\\)" (first col_list)))) (append result (list (first col_list))))]
    [(regexp-match #px"case\\(.*\\)" (first col_list))
     (let ([re-cond (regexp-match #px"case\\(condition\\((when [\\w\\s\\d<>=]* then \"[\\w\\s\\d<>=]*\")(?:;(when [\\w\\s\\d<>=]* then \"[\\w\\s\\d<>=]*\"))*;(else \"[\\w\\d]*\")?\\) end as ([\\w\\d]*)\\)" (first col_list))])
       (scan-columns (remove (first col_list) col_list) (hash-set query-hash "case" (list-tail re-cond 1)) (append result (last re-cond))))]
    [else
     (scan-columns (remove (first col_list) col_list) query-hash (append result (list (first col_list))))]))


;(perform-query "select * from resources/wef.csv where(col>10 and row<44)->groupby(xol)")

;;
  