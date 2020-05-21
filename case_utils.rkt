#lang racket
(require "regexps.rkt")
(provide case-util)


(define (case-util conditions DF col)
  (displayln conditions)
  (cond
    [(empty? conditions) DF]
    [else
     (let ([list-cond (condition-to-list (first conditions))])
       (cond
         [(equal? (second list-cond) "when")
          (let ([main-cond (string-split (third list-cond) "=")])
            (case-util (remove (first conditions) conditions) (map (lambda (x)
                                                                     (if (= (hash-ref x (first main-cond)) (string->number (second main-cond)))
                                                                         (hash-set x col (fourth list-cond))
                                                                         x))
                                                                   DF) col))]))]))
                                                                              
                                                                              
     