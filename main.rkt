#lang racket

(require "table_parser.rkt")
(require "filters.rkt")
(require "pprinter.rkt")
(require "regexps.rkt")

(let loop ()
  (display ">")
  (define input (read-line (current-input-port) 'any))
  (cond
    [(regexp-match #rx"load\\(\"(.*)\"\\)" input)
     (pprint (load (list-ref (regexp-match #rx"load\\(\"(.*)\"\\)" input) 1)))]
    [(equal? (substring input 0 6) "select")
     (parse-SQL (perform-query input))])
(loop))
