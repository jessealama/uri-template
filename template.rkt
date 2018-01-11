#lang racket/base

(require racket/contract
         (only-in (file "variable.rkt")
                  variable?))

(provide
 (contract-out
  (struct template
    ((operator (or/c false/c string?))
     (variables (listof variable?))))))

(struct template
  (operator
   variables)
  #:transparent)

(module+ test
  (require rackunit))
