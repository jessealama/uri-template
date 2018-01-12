#lang racket/base

(provide uri-template?)

(require (only-in (file "parse.rkt")
                  uri-template?)
         (only-in (file "expand.rkt")
                  expand-template)
         (only-in (file "assignment.rkt")
                  assignment?)
         (only-in (file "value.rkt")
                  value?))
