#lang racket
(provide main-regex)

;; Process main input query
(define (remove-false-from-list regex-list)
  (displayln "remove-false")
  (remove (list-ref (remf* false? regex-list) 0) (remf* false? regex-list)))

(define (main-regex query)
  (query-to-hash (remove-false-from-list (regexp-match #px"(select) (?:(distinct) (.*)|(MED)\\((\\w*)\\)|(COUNT)\\((\\w*)\\)|(SUM)\\((\\w*)\\)|(.*)) (from) (\\w*\\/\\w*.(?:csv|tsv))(?:\\s)?(?:(where)\\(([\\w\\d\\s=,]*)\\)|(orderby)\\(([\\w\\d=,]*(?:\\sASC|\\sDESC)?)\\))?(?:\\s)?(?:(orderby)\\(([\\w\\d=,]*(?:\\sASC|\\sDESC)?)\\))?"
                                                       query)) (make-hash)))

(define (query-to-hash query query-hash)
  (cond
    [(empty? query) query-hash]
    [(equal? (first query) "select")
     (cond
       ; Distinct
       [(equal? (second query) "distinct")
        (hash-set! query-hash "distinct" #t)
        (hash-set! query-hash "select" (third query))
        (query-to-hash (list-tail query 3) query-hash)]
       
       ; AGRF
       [(regexp-match #px"(MED|COUNT|SUM)" (second query))
        (hash-set! query-hash "agregate" (second query))
        (hash-set! query-hash "select" (third query))
        (query-to-hash (list-tail query 3) query-hash)]

       ; Default
       [else
        (hash-set! query-hash "select" (second query))
        (query-to-hash (list-tail query 2) query-hash)])]
    ; From
    [(equal? (first query) "from") (hash-set! query-hash "from" (second query)) (query-to-hash (list-tail query 2) query-hash)]
    [else
     (cond
       ; Where
       [(equal? (first query) "where") (hash-set! query-hash "where" (second query)) (query-to-hash (list-tail query 2) query-hash)]
       ; Orderby
       [(equal? (first query) "orderby") (hash-set! query-hash "orderby" (second query)) (query-to-hash (list-tail query 2) query-hash)])]))


(main-regex "select MED(row) from resources/csv.tsv where(row=1 and col=2) orderby(row,col)")
;;
  