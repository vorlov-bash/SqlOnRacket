#lang racket

(provide orderby)
; :DF: ({...}...{...})
; :query: string
(define (orderby DF query)
  (let ([list-query (string-split query " ")])
    (cond
      [(member "ASC" list-query) (orderby-util-asc DF (string-split (first list-query) ","))]
      [(member "DESC" list-query) (orderby-util-desc DF (string-split (first list-query) ","))]
      [else
       (orderby-util-asc DF (string-split (first list-query) ","))])))


; :DF: ({...}...{...})
; :columns: list
(define (orderby-util-asc DF columns)
  (cond
    [(empty? columns) DF]
    [else
     (orderby-util-asc (sort DF < #:key (lambda (x) (hash-ref x (first columns)))) (remove (first columns) columns))]))

; :DF: ({...}...{...})
; :columns: list
(define (orderby-util-desc DF columns)
  (cond
    [(empty? columns) DF]
    [else
     (orderby-util-desc (sort DF > #:key (lambda (x) (hash-ref x (first columns)))) (remove (first columns) columns))]))