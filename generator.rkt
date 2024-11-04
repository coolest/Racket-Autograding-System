#lang racket

(require "solutions.rkt")

(provide (all-defined-out))

; Q1
(define (sequence-test-case-generator n)
    (for/list ([i (in-range n)])
        (let* ([spacing (random 1 5)]
            [low (random 0 10)]
            [high (+ (random 10 20) low)])
        (list spacing low high))))

; Q2
(define (string-append-map-test-case-generator n)
    (for/list ([i (in-range n)])
        (let ([random-list (generate-random-string-list (random 0 11) 25)]
            [suffix (random-string (random 0 5))])
        (list random-list suffix))))

; Q3
(define (list-nth-mod-test-case-generator n)
    (for/list ([i (in-range n)])
        (let ([random-list (generate-random-string-list (random 1 11) 25)]
            [n-val (random 0 88)])
        (list random-list n-val))))

; Q4
(define (stream-for-k-steps-test-case-generator n)
    (for/list ([i (in-range n)])
        (let ([k (random 1 11)])
        (list funny-number-stream k))))

; Q5 (stream) => stream-test-case-generator
; Q6 (stream) => stream-test-case-generator
; Q7 (stream) => stream-test-case-generator

; Q8
(define (cycles-lists-test-case-generator n)
    (for/list ([i (in-range n)])
        (let ([random-list-1 (generate-random-string-list (random 1 11) 25)]
            [random-list-2 (generate-random-int-list (random 1 50))])
        (list (random 3 99) random-list-1 random-list-2))))

; Q9
(define (vector-assoc-test-case-generator n)
    (for/list ([i (in-range n)])
        (let* ([random-list (generate-random-int-list (random 3 25))]
                [elem (list-ref random-list (random (length random-list)))])
            (list elem (list->vector random-list)))))

; Q10
(define (caching-assoc-test-case-generator n)
    (for/list ([i (in-range n)])
        (list (random -100 100))))

; Q11
(define (while-greater-test-case-generator n)
    (for/list ([i (in-range n)])
        (let ([x 1])
            (list (random 10 50) (begin (set! x (+ x 1)) x)))))

; Stream generator
(define (stream-test-case-generator n)
    (for/list ([i (in-range n)])
        (list (random 0 77))))

; Helper Functions
(define (random-string length)
  (let* ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (list->string
     (for/list ([i (in-range length)])
       (string-ref chars (random (string-length chars)))))))

(define (generate-random-string-list count max-length)
  (for/list ([i (in-range count)])
    (random-string (random 1 max-length))))

(define (generate-random-int-list n)
  (for/list ((i n))
    (random -100 100)))

(define (generate-random-assoc-int-list n)
  (for/list ((i n))
    (cons (random -100 100) (random -100 100))))