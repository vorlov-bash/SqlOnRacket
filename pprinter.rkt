#lang racket

(provide pprint)

(define (string-repeat n str)
  (string-append* (make-list n str)))

(define (find-max table)
  (map (lambda (i)
         (first (sort (map (lambda (j)
                             (string-length (list-ref (list-ref table j) i)))
                           (range (length table))) >)))
       (range (length (first table)))))


(define (pprint-row row max_arr i)
  (cond
    [(= i (length row)) (display "\n")]
    [else
     (display (string-append (~a (list-ref row i) #:min-width (list-ref max_arr i) #:max-width 100  #:limit-marker "... ") "|"))
     (pprint-row row max_arr (+ i 1))]))


(define (pprint-separator first_row max_arr i)
    (cond
    [(= i (length first_row)) (display "\n")]
    [else

     (display (string-append (string-repeat (list-ref max_arr i) "-") "+"))
     (pprint-separator first_row max_arr (+ i 1))]))


(define (pprint table)
    (let ([max_arr (map (lambda (i)
                         (cond
                           [(> i 100) 100]
                           [else
                            (+ i 2)]))
                       (find-max table))])
      (display "+")
      (pprint-separator (list-ref table 0) max_arr 0)
      (display "|")
      (pprint-row (list-ref table 0) max_arr 0)
      (display "+")
      (pprint-separator (list-ref table 0) max_arr 0)
      (for-each (lambda (row)
                  (display "|")
                  (pprint-row (list-ref table row) max_arr 0))
                (list-tail (range (length table)) 1))
      (display "+")
      (pprint-separator (list-ref table 0) max_arr 0)))