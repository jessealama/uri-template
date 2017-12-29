#lang racket/base

(provide lex-uri-template)

(module+ test
  (require rackunit)
  (require (only-in (file "util.rkt")
                    urls-equal?
                    let-test)))

(require net/url-structs)
(require br-parser-tools/lex)
(require brag/support)

(define uri-template-lexer
  (lexer
   [(eof)
    eof]
   [(from/to "{" "}")
    (token 'EXPRESSION lexeme)]
   [(repetition 1 +inf.0
                (union #\!                         ; %21
                       (char-range #\# #\$)        ; %x23-24
                       #\&                         ; %x26
                       (char-range "(" ";")        ; %x28-3B
                       #\=                         ; %x3D
                       (char-range #\? "[")        ; %x3F-5B
                       "]"                         ; %x5D
                       #\_                         ; %x5F
                       (char-range #\a #\z)        ; %x61-7A
                       #\~                         ; %x7E

                       ;; ucschar production
                       (char-range #\u00a0 #\ud7ff)
                       (char-range #\uf900 #\ufdcf)
                       (char-range #\ufdf0 #\uffef)
                       (char-range #\U10000 #\U1FFFD)
                       (char-range #\U20000 #\U2FFFD)
                       (char-range #\U30000 #\U3FFFD)
                       (char-range #\U40000 #\U4FFFD)
                       (char-range #\U50000 #\U5FFFD)
                       (char-range #\U60000 #\U6FFFD)
                       (char-range #\U70000 #\U7FFFD)
                       (char-range #\U80000 #\U8FFFD)
                       (char-range #\U90000 #\U9FFFD)
                       (char-range #\UA0000 #\UAFFFD)
                       (char-range #\UB0000 #\UBFFFD)
                       (char-range #\UC0000 #\UCFFFD)
                       (char-range #\UD0000 #\UDFFFD)
                       (char-range #\UE1000 #\UEFFFD)

                       ;; iprivate production
                       (char-range #\uE000 #\uF8FF)
                       (char-range #\UF0000 #\UFFFFD)
                       (char-range #\U100000 #\U10FFFD)))
    (token 'TEXT lexeme)]))

(define (lex-uri-template s)
  (unless (string? s)
    (error "Cannot lex a non-string:" s))
  (apply-lexer uri-template-lexer s))

(module+ test
  (check-exn exn:fail:read? (lambda () (lex-uri-template "hi{")))
  (let-test ([template "http://example.com/~{username}/"])
    (check-not-exn (lambda () (lex-uri-template template)))
    (let-test ([lexed (lex-uri-template template)])
      (check-= 3 (length lexed) 0 (format "~s" lexed))))
  (let-test ([template "http://example.com/dictionary/{term:1}/{term}"])
    (check-not-exn (lambda () (lex-uri-template template)))
    (let-test ([lexed (lex-uri-template template)])
      (check-= 4 (length lexed) 0)))
  (let-test ([template "http://example.com/search{?q,lang}"])
    (check-not-exn (lambda () (lex-uri-template template)))
    (let-test ([lexed (lex-uri-template template)])
      (check-= 2 (length lexed) 0))))
