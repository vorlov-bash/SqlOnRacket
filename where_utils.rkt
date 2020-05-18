#lang racket

(require "polish.rkt")
(require typed-stack)
(provide where)

; :one-cond: regexp
; :DF: ({...}...{...})
(define (process one_cond DF)
  (cond
    [(equal? (third one_cond) "=")
     (let ([new-DF (filter-map (lambda (x)
                                 (and (equal?
                                       (hash-ref x (second one_cond))
                                       (string->number (fourth one_cond)))x))
                               DF)])
       (cond
         [(empty? new-DF) '()]
         [else new-DF]))]
    [(equal? (third one_cond) ">")
     ]
    )
  )
     
  

; :polish-conds: <#Stack>
; :stack: <#Stack>
; :DF: ({...}...{...})
(define (where polish_conds stack increment DF)
  (displayln (stack->list stack))
  (displayln (stack->list polish_conds))
  (cond
    [(stack-empty? polish_conds) (top stack)]
    [(regexp-match #px"(AND|OR|NOT)" (top polish_conds))
     (cond
       [(equal? "AND" (top polish_conds))
        (where (pop polish_conds) (push stack (and-util (top stack) (top (pop stack)))) (+ increment 1) DF)])]
       
    [else
     (cond
       [(= increment 0) (where (pop polish_conds) (push stack (process (regexp-match #px"(\\w+)(=|>|<)(\\w+)" (top polish_conds)) DF)) (+ increment 1) DF)]
       [else
        (where (pop polish_conds) (push stack (top polish_conds)) (+ increment 1) DF)])]))
  



; :cond1: string
; :cond2: string
; :DF: ({...}...{...})
(define (and-util op DF)
  (displayln op)
  (process (regexp-match #px"(\\w+)(=|>|<)(\\w+)" op) DF))
  