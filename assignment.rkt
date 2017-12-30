#lang racket/base

(provide assignment?)

(require (only-in racket/string
                  non-empty-string?)
         (only-in (file "variable.rkt")
                  variable?))

(module+ test
  (require rackunit))

(define (assignment-value? v)
  (or (non-empty-string? v)
      (and (hash? v)
           (andmap variable?
                   (hash-keys v))
           (andmap non-empty-string?
                   (hash-values v)))))

(module+ test
  (check-false (assignment-value? ""))
  (check-false (assignment-value? 0))
  (check-false (assignment-value? #\a))
  (check-true (assignment-value? "butterick"))
  (check-true (assignment-value? (hash)))
  (check-false (assignment-value? (hash 'hi "butterick")))
  (check-true (assignment-value? (hash "hi" "matthew")))
  (check-false (assignment-value? (hash "" "no")))
  (check-false (assignment-value? (hash "no" ""))))

(define (assignment? x)
  (and (hash? x)
       (andmap variable? (hash-keys x))
       (andmap assignment-value? (hash-values x))))

(module+ test
  (check-true (assignment? (hash "hi" "there")))
  (check-true (assignment? (hash)))
  (check-false (assignment? (cons "hi" "there")))
  (check-false (assignment? 0))
  (check-false (assignment? "hi:there"))
  (check-true (assignment? (hash "stop" "go"
                                 "yes" (hash "no" "way")))))
