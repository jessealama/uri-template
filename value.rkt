#lang racket/base

(provide value?
         list-value?
         string-value?
         null-value?
         associative-array-value?
         associative-array-ref
         associative-array-keys
         associative-array->list
         empty-value?)

(require racket/contract
         racket/list
         (only-in racket/string
                  non-empty-string?)
         (only-in (file "variable.rkt")
                  variable?
                  variable-name))

(module+ test
  (require rackunit))

(define (null-value? x)
  (and (symbol? x)
       (eq? x 'null)))

(module+ test
  (check-true (null-value? 'null))
  (check-false (null-value? void))
  (check-false (null-value? 0))
  (check-false (null-value? "")))

(define/contract (string-value? x)
  (-> any/c boolean?)
  (string? x))

(module+ test
  (check-true (string-value? ""))
  (check-false (string-value? 0))
  (check-false (string-value? #\a))
  (check-true (string-value? "butterick")))

(define (atomic-value? x)
  (or (null-value? x)
      (string-value? x)))

(define/contract (list-value? x)
  (-> any/c boolean?)
  (and (list? x)
       (andmap atomic-value? x)))

(module+ test
  (check-true (list-value? (list "")))
  (check-true (list-value? (list)))
  (check-true (list-value? (list 'null)))
  (check-true (list-value? (list "hi" 'null "")))
  (check-false (list-value? "a,b,c"))
  (check-false (list-value? 42)))

(define (associative-array-value? x)
  (-> any/c boolean?)
  (and (list? x)
       (andmap pair? x)
       (andmap string?
               (map first x))
       (andmap atomic-value?
               (map second x))
       (andmap empty?
               (map cddr x))
       (eq? #f
            (check-duplicates x
                              string=?
                              #:key first))))

(define/contract (value? x)
  (-> any/c boolean?)
  (or (atomic-value? x)
      (list-value? x)
      (associative-array-value? x)))

(module+ test
  (check-true (associative-array-value? (list)))
  (check-false (associative-array-value? (hash)))
  (check-false (associative-array-value? (list (list 'hi "butterick"))))
  (check-true (associative-array-value? (list (list "hi" "matthew"))))
  (check-true (associative-array-value? (list (list "" "no"))))
  (check-true (associative-array-value? (list (list "no" ""))))
  (check-true (associative-array-value? (list (list "woah" 'null))))
  (check-false (associative-array-value? (list (list "deeply" (list "nested" "vested"))))))

(module+ test
  ;; examples from the RFC
  (check-true (value? '("one" "two" "three")))
  (check-true (value? '("example" "com")))
  (check-true (value? "me/too"))
  (check-true (value? "Hello World!"))
  (check-true (value? "50%"))
  (check-true (value? "value"))
  (check-true (value? "fred"))
  (check-true (value? "http://example.com/home/"))
  (check-true (value? "/foo/bar"))
  (check-true (value? '("red" "green" "blue")))
  (check-true (value? (list (list "semi" ";") (list "dot" ".") (list "comma" ","))))
  (check-true (value? "6"))
  (check-true (value? "1024"))
  (check-true (value? "768"))
  (check-true (value? ""))
  (check-true (value? (list)))
  (check-true (value? 'null)))

(define/contract (associative-array-ref arr x)
  (associative-array-value? string? . -> . atomic-value?)
  (define v (assoc x arr string=?))
  (if (pair? v)
      (second v)
      'null))

(define/contract (associative-array-keys arr)
  (-> associative-array-value? (listof string?))
  (map first arr))

(define/contract (associative-array->list arr)
  (associative-array-value? . -> . list-value?)
  (cond ((empty? arr)
         empty)
        (else
         (append (list (first (first arr))
                       (second (first arr)))
                 (associative-array->list (rest arr))))))

(define/contract (empty-value? value)
  (value? . -> . boolean?)
  (cond ((string-value? value)
         (string=? "" value))
        ((list-value? value)
         (empty? value))
        ((associative-array-value? value)
         (empty? value))))
