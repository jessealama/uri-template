#lang racket/base

(provide assignment?
         assignment-ref
         assignment-has-variable?)

(require racket/contract
         racket/list
         (only-in racket/string
                  non-empty-string?)
         (only-in (file "value.rkt")
                  value?)
         (only-in (file "variable.rkt")
                  variable?
                  variable-name))

(module+ test
  (require rackunit))

(define (assignment? x)
  (and (hash? x)
       (andmap string? (hash-keys x))
       (andmap value? (hash-values x))
       (eq? #f
            (check-duplicates (hash-keys x)
                              string=?))))

(module+ test
  (check-true (assignment? (hash "hi" "there")))
  (check-true (assignment? (hash)))
  (check-false (assignment? (cons "hi" "there")))
  (check-false (assignment? 0))
  (check-false (assignment? "hi:there"))
  (check-true (value? (list (list "no" "way"))))
  (check-true (assignment? (hash "stop" "go"
                                 "yes" (list (list "no" "way"))))))

(module+ test
  ;; example from the spec
  (let ([count '("one" "two" "three")]
        [dom '("example" "com")]
        [dub "me/too"]
        [hello "Hello World!"]
        [half "50%"]
        [var "value"]
        [who "fred"]
        [base "http://example.com/home/"]
        [path "/foo/bar"]
        [list '("red" "green" "blue")]
        [keys (list (list "semi" ";")
                    (list "dot" ".")
                    (list "comma" ","))]
        [v "6"]
        [x "1024"]
        [y "768"]
        [empty ""]
        [empty_keys (list)])
    (let ([assignment (hash "count" count
                            "dom" dom
                            "dub" dub
                            "hello" hello
                            "half" half
                            "var" var
                            "who" who
                            "base" base
                            "path" path
                            "list" list
                            "keys" keys
                            "x" x
                            "y" y
                            "empty" empty
                            "empty_keys" empty_keys)])
      (check-true (assignment? assignment)))))

(define/contract (assignment-ref assignment var)
  (assignment? (or/c string? variable?) . -> . value?)
  (cond ((string? var)
         (hash-ref assignment var 'null))
        ((variable? var)
         (hash-ref assignment (variable-name var) 'null))))

(define/contract (assignment-has-variable? assignment var)
  (assignment? (or/c string? variable?) . -> . boolean?)
  (cond ((string? var)
         (hash-has-key? assignment var))
        ((variable? var)
         (hash-has-key? assignment (variable-name var)))))
