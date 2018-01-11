#lang racket/base

(provide variable
         variable?
         variable-name
         variable-modifier
         acceptable-variable-name?
         exploded?)

(require (only-in racket/list
                  empty?
                  first
                  second
                  third
                  rest)
         racket/contract)

(module+ test
  (require rackunit))

(struct variable
  (modifier name)
  #:transparent)

(define hex-chars
  (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
        #\a #\b #\c #\d #\e #\f
        #\A #\B #\C #\D #\E #\F))

(define (hex-char? c)
  (list? (member c hex-chars)))

(module+ test
  ;; all positive checks (10 + 6 + 6 = 22 cases)
  (check-true (hex-char? #\0))
  (check-true (hex-char? #\1))
  (check-true (hex-char? #\2))
  (check-true (hex-char? #\3))
  (check-true (hex-char? #\4))
  (check-true (hex-char? #\5))
  (check-true (hex-char? #\6))
  (check-true (hex-char? #\7))
  (check-true (hex-char? #\8))
  (check-true (hex-char? #\9))
  (check-true (hex-char? #\a))
  (check-true (hex-char? #\b))
  (check-true (hex-char? #\c))
  (check-true (hex-char? #\d))
  (check-true (hex-char? #\e))
  (check-true (hex-char? #\f))
  (check-true (hex-char? #\A))
  (check-true (hex-char? #\B))
  (check-true (hex-char? #\C))
  (check-true (hex-char? #\D))
  (check-true (hex-char? #\E))
  (check-true (hex-char? #\F))

  ;; strings are not allowed
  (check-false (hex-char? "a"))

  ;; characters as integers are not allowed
  ;; 48 is the ASCII code point for the character 0
  (check-false (hex-char? 48)))

(define (characters-ok? chars)
  (cond ((empty? chars)
         #t)
        ((char=? #\% (first chars))
         (and (not (empty? (cdr chars)))
              (not (empty? (cddr chars)))
              (hex-char? (second chars))
              (hex-char? (third chars))
              (characters-ok? (cdddr chars))))
        (else
         (characters-ok? (rest chars)))))

(define (acceptable-variable-name? x)
  (cond ((not (string? x))
         #f)
        ((not (regexp-match-exact? #px"[0-9a-zA-Z_%]+" x))
         #f)
        (else
         (characters-ok? (string->list x)))))

(module+ test
  (check-false (acceptable-variable-name? ""))
  (check-true (acceptable-variable-name? "var"))
  (check-true (acceptable-variable-name? "semi"))
  (check-false (acceptable-variable-name? "?"))
  (check-true (acceptable-variable-name? "hi%ae"))
  (check-false (acceptable-variable-name? "no%"))
  (check-false (acceptable-variable-name? "no%a"))
  (check-false (acceptable-variable-name? "var%0r"))
  (check-false (acceptable-variable-name? "var%x0")))

(define/contract (exploded? var)
  (variable?  . -> . boolean?)
  (define m (variable-modifier var))
  (and (string? m)
       (string=? "*" m)))
