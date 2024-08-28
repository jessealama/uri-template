#lang info

(define collection "uri-template")

(define version "0.6")

(define deps
  '("base"
    "brag"
    "beautiful-racket-lib"
    "br-parser-tools-lib"
    "rackunit-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"))

(define pkg-desc "Processing URI Templates (IETF RFC 6570)")

(define pkg-authors '("jesse@serverracket.com"))

(define scribblings '(("scribblings/uri-template.scrbl" ())))
