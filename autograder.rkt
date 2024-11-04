#lang racket

; (provide (all-defined-out))
(require json)
(require rackunit)
(require racket/file)
(require "generator.rkt")
(require (prefix-in sol: "solutions.rkt")
         (prefix-in hw: "homework3.rkt"))

;
(define results-file "../results/results.json")
(define total-submissions-allowed 5)

; limit submissions via metadata provided
(define previous-submissions 
    (let* 
        [(submission-metadata (read-json (open-input-file "../submission_metadata.json" #:mode 'text)))]
    (hash-ref submission-metadata 'previous_submissions null)))
    
(define (score->number s)
    (cond
        [(number? s) s]
        [(string? s) (let ([n (string->number s)])
                        (if n n 0))]
        [#t 0]))

(define previous-submission-count
    (length 
        (filter (lambda (sub)
            (> (score->number (hash-ref sub 'score "0.0")) 0)) previous-submissions)))


; json data for the results-file
(define (starter-data-entry name) 
  (make-hash `((score . 0) (max_score . 5) (output . "Will be manually graded.") (name . ,name))))

(define results-data
  (make-hash `(
    (output . ,(format "You have ~a submissions left!" (- total-submissions-allowed previous-submission-count)))
    (tests . ,(for/list ([i (in-range 1 14)])
                                 (starter-data-entry (format "question~a" i)))))))

; function to write to the json file
(define (write-json-file filepath data)
    (call-with-output-file filepath
        (lambda (out)
            (write-json data out))
        #:exists 'replace))

; main idea; grade each question
(define (grade-question fn-hw fn-sol input-cases id max-score)
  (let* ([results
          (for/fold ([acc (cons 0 0)]) ; acc is a pair (total-tests . passed-tests), got too many errors trying to do values
                    ([input (in-list input-cases)])
            (let* ([hw-result (apply fn-hw input)]
                   [sol-result (apply fn-sol input)]
                   [total-tests (car acc)]
                   [passed-tests (cdr acc)])
              (if (equal? hw-result sol-result)
                  (cons (+ total-tests 1) (+ passed-tests 1))
                  (cons (+ total-tests 1) passed-tests))))]
         [total-tests (car results)]
         [passed-tests (cdr results)]
         [score (floor (* max-score (/ passed-tests total-tests)))]
         [output (format "Passed ~a out of ~a test cases." passed-tests total-tests)])
    (hash-update! results-data 'tests
        (lambda (tests)
            (map (lambda (test)
                (if (equal? (hash-ref test 'name) id)
                    (hash 'name id 'score score 'max_score max-score 'output output)
                    test))
                tests)))))

; wrap macro in lambda function (able to use grade-question)
(define-syntax-rule (create-while-greater-wrapper macro)
  (lambda (e1 e2)
    (macro e1 do e2)))

(define (protected-call f args id max-score)
    (with-handlers ([(lambda (_) #t)
                (lambda (err) 
                    (hash-update! results-data 'tests
                        (lambda (tests)
                            (map (lambda (test)
                                (if (equal? (hash-ref test 'name) id)
                                    (hash 'name id 'score 0 'max_score max-score 'output (exn-message err))
                                    test))
                                tests))))])
    (apply f (append args (list id max-score)))))

(define (grade)
    (protected-call grade-question (list hw:sequence sol:sequence (sequence-test-case-generator 50)) "question1" 7)
    (protected-call grade-question (list hw:string-append-map sol:string-append-map (string-append-map-test-case-generator 50)) "question2" 7)
    (protected-call grade-question (list hw:list-nth-mod sol:list-nth-mod (list-nth-mod-test-case-generator 50)) "question3" 7)
    (protected-call grade-question (list hw:stream-for-k-steps sol:stream-for-k-steps (stream-for-k-steps-test-case-generator 25)) "question4" 7)
    (protected-call grade-question
        (list (lambda (n) (sol:stream-for-k-steps hw:funny-number-stream n))
            (lambda (n) (sol:stream-for-k-steps sol:funny-number-stream n))
            (stream-test-case-generator 10)) "question5" 7)
    (protected-call grade-question
        (list (lambda (n) (sol:stream-for-k-steps hw:zoran-then-bentley n))
            (lambda (n) (sol:stream-for-k-steps sol:zoran-then-bently n))
            (stream-test-case-generator 10)) "question6" 7)
    (protected-call grade-question
        (list (lambda (n) (sol:stream-for-k-steps (hw:stream-add-one sol:zoran-then-bently) n))
            (lambda (n) (sol:stream-for-k-steps (sol:stream-add-one sol:zoran-then-bently) n))
            (stream-test-case-generator 10)) "question7" 7)
    (protected-call grade-question
        (list (lambda (n list1 list2) (sol:stream-for-k-steps (hw:cycle-lists list1 list2) n))
            (lambda (n list1 list2) (sol:stream-for-k-steps (sol:cycles-lists list1 list2) n))
            (cycles-lists-test-case-generator 25)) "question8" 7)
    (protected-call grade-question (list hw:vector-assoc sol:vector-assoc (vector-assoc-test-case-generator 25)) "question9" 8)

    (define xs (generate-random-assoc-int-list 100))
    (define slots (random 3 8))
    (protected-call grade-question 
        (list (hw:caching-assoc xs slots) 
            (sol:caching-assoc xs slots) 
            (caching-assoc-test-case-generator 100)) "question10" 8)
    (protected-call grade-question 
        (list (create-while-greater-wrapper hw:while-greater)
            (create-while-greater-wrapper sol:while-greater)
            (while-greater-test-case-generator 20)) "question11" 8)

    (write-json-file results-file results-data)
)

; grade the homework
(if (> 5 previous-submission-count)
    (grade)
    (let* ([prev-data (car previous-submissions)]
            [new-output "You have used all your submissions, last submission used."]
            [new-data (hash-update prev-data 'output (lambda (_) new-output) new-output)])
        (write-json-file results-file new-data)))