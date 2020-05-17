#lang racket
(require typed-stack)
(require "stack.rkt")

(define precedence (hash "NOT" 3 "AND" 2 "OR" 1))

; :where_conditions: list
; :stack: <#Stack>
; :output: <#Stack>
; :return: list
(define (where-to-polish where_conditions stack output)
  (display "stack: ")
  (displayln (stack->list stack))
  (display "output: ")
  (displayln (stack->list output))
  (cond
    [(empty? where_conditions) (push-to-other-stack stack output)]
    [(regexp-match #px"(AND|OR|NOT)" (first where_conditions))
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
         (push-to-other-stack output stack))])]
    [else
     (where-to-polish
      (remove (first where_conditions) where_conditions)
      stack
      (push output (first where_conditions)))]))

(stack->list (where-to-polish (string-split "ror=1 OR NOT ror=1 AND col=3" " ") (make-stack) (make-stack)))