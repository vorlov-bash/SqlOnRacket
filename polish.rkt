#lang racket
(require typed-stack)
(require "stack.rkt")
(provide where-to-polish)
 
(define precedence (hash "not" 3 "and" 2 "or" 1))
; :where_conditions: list
; :stack: <#Stack>
; :output: <#Stack>
; :return: <#Stack>
(define (where-to-polish where_conditions stack output)
  (cond
    [(empty? where_conditions) (push-to-other-stack stack output)]
    [(regexp-match #px"(^and$|^or$|^not$)" (first where_conditions))
     (cond
       [(stack-empty? stack) (where-to-polish
                              (remove (first where_conditions) where_conditions)
                              (push stack (first where_conditions))
                              output)]
       [(> (hash-ref precedence (top stack)) (hash-ref precedence (first where_conditions)))
        (where-to-polish
         (remove (first where_conditions) where_conditions)
         (push-with-precedence stack (first where_conditions))
         (return-push-with-precedence stack output (first where_conditions)))]
       
       [(< (hash-ref precedence (top stack)) (hash-ref precedence (first where_conditions)))
        (where-to-polish
         (remove (first where_conditions) where_conditions)
         (push stack (first where_conditions))
         output)]

       [(= (hash-ref precedence (top stack)) (hash-ref precedence (first where_conditions)))
        (where-to-polish
         (remove (first where_conditions) where_conditions)
         (push-with-precedence stack (first where_conditions))
         (return-push-with-precedence stack output (first where_conditions)))])]
    [else
     (where-to-polish
      (remove (first where_conditions) where_conditions)
      stack
      (push output (first where_conditions)))]))
