#lang racket/base

(require racket/contract
         racket/list
         racket/string
         (file "template.rkt")
         (file "assignment.rkt")
         (file "parse.rkt"))

(module+ test
  (require rackunit))

;; obviously wrong
(define (extend-assignment ass)
  ass)

(define (match-template-with-assignment template uri pos assignment)
  ((or/c string? template?)
   string?
   exact-positive-integer?
   assignment?
   . -> .
   (or/c false/c assignment?))
  (cond ((empty? template)
         assignment)
        ((string? (first template))
         (define x (first template))
         (define xs (rest template))
         (cond ((string-prefix? (substring uri pos) x)
                (match-template-with-assignment xs
                                                uri
                                                (+ pos (string-length x))
                                                assignment))
               (else
                #f)))
        ((empty? (rest template))
         (define x (first template))
         (define v (substring uri pos))
         (extend-assignment assignment x v))
        ((template? (second template))
         (match-template-with-assignment (rest template)
                                         uri
                                         pos
                                         assignment))
        ((not (string-contains? (substring uri pos)
                                (second template)))
         #f)
        (else
         #f)))

(define/contract (match-template template uri)
  (string? string? . -> . (or/c assignment? false/c))
  (define parsed (parse-template template))
  (list))

#|

Functionality not yet ready.

(module+ test
  (check-true (assignment? (match-template "http://example.com/~{username}/"
                                           "http://example.com/~bob/")))
  (check-true (assignment? (match-template "http://example.com/~{username}"
                                           "http://example.com/~bob")))
  (check-false (assignment? (match-template "http://example.com/~{username}/"
                                            "http://example.com/~bob")))
  (check-true (assignment? (match-template "http://example.com/~{username}"
                                           "http://example.com/~bob/"))))

|#
