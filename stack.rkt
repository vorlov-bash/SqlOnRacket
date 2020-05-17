#lang racket
(require typed-stack)
(provide push-to-other-stack)
(provide push-with-precedence)
(provide return-push-with-precedence)


(define precedence (hash "NOT" 3 "AND" 2 "OR" 1))

; :to_stack: <#Stack>
; :from_stack: <#Stack>
; :return <#Stack>
(define (push-to-other-stack to_stack from_stack)
  (cond
    [(stack-empty? from_stack) to_stack]
    [else
     (push-to-other-stack (push to_stack (top from_stack)) (pop from_stack))]))

(define (push-with-precedence stack op)
  (cond
    [(stack-empty? stack) (push stack op)]
    [(< (hash-ref precedence (top stack)) (hash-ref precedence op))
     (push stack op)]
    [else
     (push-with-precedence (pop stack) op)]))

(define (return-push-with-precedence stack output op)
  (cond
    [(stack-empty? stack) output]
    [(< (hash-ref precedence (top stack)) (hash-ref precedence op))
     output]
    [else
     (return-push-with-precedence (pop stack) (push output (top stack)) op)]))