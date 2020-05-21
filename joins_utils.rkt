#lang racket
(require "table_parser.rkt")
(require racket/hash)
(provide join-util)


(define list1 (list (hash "col" 1 "row" 2 "item" "salt")
                    (hash "col" 15 "row" 22 "item" "butter")
                    (hash "col" 2 "row" 45 "item" "potato")
                    (hash "col" 2 "row" 90 "item" "burger")))

(define list2 (list (hash "empID" 2 "emp" 40 "country" "France")
                    (hash "empID" 2 "emp" 500 "country" "selo")
                    (hash "empID" 11 "emp" 50 "country" "Ukraine")))


(define (join-util hash-query)
  (cond
    [(hash-has-key? hash-query "right_join")
     (let ([list-query (string-split (hash-ref hash-query "right_join") " ")])
       (merge-util (inner-join-util
                    (perform-table-to-hashes (load (hash-ref hash-query "from")))
                    (perform-table-to-hashes (load (first list-query)))
                    (string-split (third list-query) "=")
                    (list))
                   (join-exclusive-util
                    (perform-table-to-hashes (load (first list-query)))
                    (perform-table-to-hashes (load (hash-ref hash-query "from")))
                    (reverse (string-split (third list-query) "="))
                    (list))))]

    [(hash-has-key? hash-query "inner_join")
     (let ([list-query (string-split (hash-ref hash-query "inner_join") " ")])
       (inner-join-util
        (perform-table-to-hashes (load (hash-ref hash-query "from")))
        (perform-table-to-hashes (load (first list-query)))
        (string-split (third list-query) "=")
        (list)))]
    [(hash-has-key? hash-query "full_outer_join")
     (let ([list-query (string-split (hash-ref hash-query "full_outer_join") " ")])
       (merge-util (merge-util (inner-join-util
                                (perform-table-to-hashes (load (hash-ref hash-query "from")))
                                (perform-table-to-hashes (load (first list-query)))
                                (string-split (third list-query) "=")
                                (list))
                               (join-exclusive-util
                                (perform-table-to-hashes (load (first list-query)))
                                (perform-table-to-hashes (load (hash-ref hash-query "from")))
                                (reverse (string-split (third list-query) "="))
                                (list)))
                   (join-exclusive-util
                                (perform-table-to-hashes (load (hash-ref hash-query "from")))
                                (perform-table-to-hashes (load (first list-query)))
                                (string-split (third list-query) "=")
                                (list))))]))



;; JOIN-EXCLUSIVE
(define (join-exclusive-util RDF LDF condition result)
  (cond
    [(empty? RDF) result]
    [else
     (let ([filter-check (filter-map (lambda (x)
                                       (and (equal? (hash-ref (first RDF) (first condition)) (hash-ref x (second condition))) x))
                                     LDF)])
       (cond
         [(empty? filter-check) (join-exclusive-util (remove (first RDF) RDF) LDF condition (append result (list (first RDF))))]
         [else
          (join-exclusive-util (remove (first RDF) RDF) LDF condition result)]))]))


;; MERGE
(define (merge-util table1 table2)
  (cond
    [(empty? table2) table1]
    [(hash-keys-subset? (list-ref table1 0) (list-ref table2 0))
     (append table1 table2)]
    [else
     (append table1 (map (lambda (x)
                           (hash-union (make-immutable-hash (row-to-nil-vals (hash->list (list-ref table1 0)))) x #:combine/key (lambda (k v1 v2) v2)))
                         table2))]))

(define (row-to-nil-vals row)
  (map (lambda (x)
         (cons (car x) ""))
       row))
     

;; INNER_JOIN
(define (inner-join-helper hash-row DF2 condition)
  (filter-map (lambda (x)
                (and (equal? (hash-ref hash-row (first condition)) (hash-ref x (second condition))) x))
                DF2))

(define (inner-join-util DF1 DF2 condition joinedDF)
  (cond
    [(empty? DF1) joinedDF]
    [else
     (inner-join-util (remove (first DF1) DF1) DF2 condition (append joinedDF (map (lambda (y)
                                                                                     (hash-union (make-immutable-hash (hash->list (first DF1))) y))
                                                                                   (inner-join-helper (first DF1) DF2 condition))))]))
;;
