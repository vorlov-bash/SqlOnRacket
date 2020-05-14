#lang racket

(require csv-reading)
(require "pprinter.rkt")
(provide load)
(provide perform-table-to-hashes)
(provide perform-hash-table-to-pair)
(provide perform-pair-table-to-list)
(provide query-to-hash)

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

(define (query-to-hash split-query)
  (let ([hash-query (make-hash)])
    (map (lambda (i)
           (hash-set! hash-query (list-ref split-query i) (list-ref split-query (+ i 1))))
         (range 0 (length split-query) 2))
    hash-query))



; perform row from (...) to ( {i . j, n . m} )
(define (perform-row-to-hash row first_row)
  (let ([hash_row (make-hash)])
    (for-each (lambda (j)
                (dict-set! hash_row (list-ref first_row j) (list-ref row j)))
              (range (length row)))
  hash_row))
    


(define (perform-table-to-hashes table)
  (map (lambda (i)
         (perform-row-to-hash (list-ref table i) (list-ref table 0)))
       (list-tail (range (length table)) 1)))
;;

; perform row from ( {i . j, n . m} ) to ( (i . j)...(n . m) )
(define (perform-hash-row-to-pair row)
  (hash-map row (lambda (k v)
                  (cons k v))))

(define (perform-hash-table-to-pair table)
  (map (lambda (row)
         (perform-hash-row-to-pair row))
       table))
;;

; perform row from ( (i . j)...(n . m) ) to (...)
(define (first-row one-row)
  (map (lambda (i)
         (car i))
       one-row))

(define (perform-pair-row-to-list row)
  (map (lambda (i)
         (cond
           [(not (string? (cdr i))) (number->string (cdr i))]
           [else
            (cdr i)]))

       row))


                
(define (perform-pair-table-to-list table)
  (append (list (first-row (list-ref table 0))) (map (lambda (row)
                                                       (perform-pair-row-to-list row))
                                                     table)))

;;


(define (load file)
  (cond
    [(string-contains? file ".tsv") (csv->list (make-food-tsv-reader (open-input-file file)))]
    [(string-contains? file ".csv") (csv->list (make-food-csv-reader (open-input-file file)))]))



