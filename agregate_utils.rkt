#lang racket

(provide agregate)
(provide return-column-name)

(define (agregate func DF column)
  (cond
    [(equal? func "count") (list (hash (string-join (list "count(" column ")") "") (count-util DF column)))]
    [(equal? func "sum") (list (hash (string-join (list "sum(" column ")") "") (sum-util DF column 0 0)))]
    [(equal? func "med") (list (hash (string-join (list "med(" column ")") "") (median-util DF column)))]))

(define (return-column-name func column)
  (string-join (list func "(" column ")") ""))


(define (hashes-to-list DF column hash-list i)
  (cond
    [(equal? i (length DF)) hash-list]
    [else
     (hashes-to-list DF column (append hash-list (list (hash-ref (list-ref DF i) column))) (+ i 1))]))

(define (median-util DF column)
  (let ([col-list (hashes-to-list DF column (list) 0)])
    (cond
      [(odd? (length DF)) (first (list-tail col-list (floor (/ (length col-list) 2))))]
      [(even? (length DF)) (floor (/ (+ (first (list-tail col-list (floor (/ (- (length col-list) 1) 2)))) (second (list-tail col-list (floor (/ (- (length col-list) 1) 2))))) 2))])))

(define (sum-util DF column sum i)
  (cond
    [(equal? i (length DF)) sum]
    [else
     (sum-util DF column (+ sum (hash-ref (list-ref DF i) column)) (+ i 1))]))
  
(define (count-util DF column)
  (count (lambda (x) (hash-ref x column)) DF))
  