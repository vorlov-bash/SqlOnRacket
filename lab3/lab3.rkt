#lang racket

(require csv-reading)
(require "pprinter.rkt")


(define make-food-csv-reader
  (make-csv-reader-maker
   '((separator-chars #\,)
     (strip-leading-whitespace? #t)
     (strip-trailing-whitespace? #t))))


(define make-food-tsv-reader
  (make-csv-reader-maker
   '((separator-chars #\tab)
     (strip-leading-whitespace? #t)
     (strip-trailing-whitespace? #t))))



(define (perform-row row first_row filter)
  (let ([hash_row (make-hash)])
    (for-each (lambda (j)
                (dict-set! hash_row (list-ref first_row j) (list-ref row j)))
              (range (length row)))
  hash_row))
    


(define (perform-table table filter)
  (map (lambda (i)
         (perform-row (list-ref table i) (list-ref table 0) filter))
       (list-tail (range (length table)) 1)))


(define (load file)
  (cond
    [(string-contains? file ".tsv") (pprint (csv->list (make-food-tsv-reader (open-input-file file))))]
    [(string-contains? file ".csv") (pprint (csv->list (make-food-csv-reader (open-input-file file))))]))


(let loop ()
  (display ">")
  (define input (read-line (current-input-port) 'any))
  (cond
    [(regexp-match #rx"load\\(\"(.*)\"\\)" input)
     (load (list-ref (regexp-match #rx"load\\(\"(.*)\"\\)" input) 1))]
(loop)))

