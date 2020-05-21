#lang racket

(require "polish.rkt")
(require typed-stack)
(provide where)

; :one-cond: regexp
; :DF: ({...}...{...})
(define (process one_cond DF not-cond)
  (cond
    [(not (not not-cond))
     (cond
       ; =
       [(equal? (third one_cond) "=")
        (cond
          [(string->number (fourth one_cond))
           (let ([new-DF (filter-map (lambda (x)
                                       (and (not (equal?
                                                  (hash-ref x (second one_cond))
                                                  (string->number (fourth one_cond))))x))
                                     DF)])
             (cond
               [(empty? new-DF) '()]
               [else new-DF]))]
          [else
           (let ([new-DF (filter-map (lambda (x)
                                       (and (not (equal?
                                                  (string-length (hash-ref x (second one_cond)))
                                                  (string-length (fourth one_cond))))x))
                                     DF)])
             (cond
               [(empty? new-DF) '()]
               [else new-DF]))])]
       ; >
       [(equal? (third one_cond) ">")
        (cond
          [(string->number (fourth one_cond))
           (let ([new-DF (filter-map (lambda (x)
                                       (and (not (>
                                                  (hash-ref x (second one_cond))
                                                  (string->number (fourth one_cond))))x))
                                     DF)])
             (cond
               [(empty? new-DF) '()]
               [else new-DF]))]
          [else
           (let ([new-DF (filter-map (lambda (x)
                                       (and (not (>
                                                  (string-length (hash-ref x (second one_cond)))
                                                  (string-length (fourth one_cond))))x))
                                     DF)])
             (cond
               [(empty? new-DF) '()]
               [else new-DF]))])]
       ; <
       [(equal? (third one_cond) "<")
        (cond
          [(string->number (fourth one_cond))
           (let ([new-DF (filter-map (lambda (x)
                                       (and (not (<
                                                  (hash-ref x (second one_cond))
                                                  (string->number (fourth one_cond))))x))
                                     DF)])
             (cond
               [(empty? new-DF) '()]
               [else new-DF]))]
          [else
           (let ([new-DF (filter-map (lambda (x)
                                       (and (not (<
                                                  (string-length (hash-ref x (second one_cond)))
                                                  (string-length (fourth one_cond))))x))
                                     DF)])
             (cond
               [(empty? new-DF) '()]
               [else new-DF]))])])]
    [else
     (cond
       ; =
       [(equal? (third one_cond) "=")
        (cond
          [(string->number (fourth one_cond))
           (let ([new-DF (filter-map (lambda (x)
                                       (and (equal?
                                             (hash-ref x (second one_cond))
                                             (string->number (fourth one_cond)))x))
                                     DF)])
             (cond
               [(empty? new-DF) '()]
               [else new-DF]))]
          [else
           (let ([new-DF (filter-map (lambda (x)
                                       (and (equal?
                                             (string-length (hash-ref x (second one_cond)))
                                             (string-length (fourth one_cond))) x))
                                     DF)])
             (cond
               [(empty? new-DF) '()]
               [else new-DF]))])]
       ; >
       [(equal? (third one_cond) ">")
        (cond
          [(string->number (fourth one_cond))
           (let ([new-DF (filter-map (lambda (x)
                                       (and (>
                                             (hash-ref x (second one_cond))
                                             (string->number (fourth one_cond)))x))
                                     DF)])
             (cond
               [(empty? new-DF) '()]
               [else new-DF]))]
          [else
           (let ([new-DF (filter-map (lambda (x)
                                       (and (>
                                             (string-length (hash-ref x (second one_cond)))
                                             (string-length (fourth one_cond))) x))
                                     DF)])
             (cond
               [(empty? new-DF) '()]
               [else new-DF]))])]

       ; <
       [(equal? (third one_cond) "<")
        (cond
          [(string->number (fourth one_cond))
           (let ([new-DF (filter-map (lambda (x)
                                       (and (<
                                             (hash-ref x (second one_cond))
                                             (string->number (fourth one_cond)))x))
                                     DF)])
             (cond
               [(empty? new-DF) '()]
               [else new-DF]))]
          [else
           (let ([new-DF (filter-map (lambda (x)
                                       (and (<
                                             (string-length (hash-ref x (second one_cond)))
                                             (string-length (fourth one_cond))) x))
                                     DF)])
             (cond
               [(empty? new-DF) '()]
               [else new-DF]))])])]))
  

; :polish-conds: <#Stack>
; :stack: <#Stack>
; :increment: number
; :DF: ({...}...{...})
(define (where polish_conds stack increment DF)
  (cond
    [(stack-empty? polish_conds) (top stack)]
    [(regexp-match #px"(^and$|^or$|^not$)" (top polish_conds))
     (cond
       [(equal? "and" (top polish_conds))
        (where (pop polish_conds) (push (pop (pop stack)) (and-util (top stack) (top (pop stack)) DF)) (+ increment 1) DF)]
       [(equal? "or" (top polish_conds))
        (where (pop polish_conds) (push (pop (pop stack)) (or-util (top stack) (top (pop stack)) DF)) (+ increment 1) DF)])]
    
    [else
     (cond
       [(= increment 0) (where (pop polish_conds) (push stack (process (regexp-match #px"(\\w+)(=|>|<)(\\w+)" (top polish_conds)) DF #f)) (+ increment 1) DF)]
       [else
        (where (pop polish_conds) (push stack (top polish_conds)) (+ increment 1) DF)])]))
  


; :op: string
; :DF: ({...}...{...}) or string
; :raw_DF: ({...}...{...})
(define (and-util op DF raw_DF)
  (cond
    [(string? DF) (process (regexp-match #px"(\\w+)(=|>|<)(\\w+)" DF) (process (regexp-match #px"(\\w+)(=|>|<)(\\w+)" op) raw_DF #f) #f)]
    [else
     (process (regexp-match #px"(\\w+)(=|>|<)(\\w+)" op) DF #f)]))

(define (or-util op DF raw_DF)
  (cond
    [(and (list? op) (list? DF))
     (cond
       [(empty? op) DF]
       [(empty? DF) op])]
    [(empty? DF) (process (regexp-match #px"(\\w+)(=|>|<)(\\w+)" op) raw_DF #f)]
    [else DF]))

; :op: string
; :DF: ({...}...{...})


  