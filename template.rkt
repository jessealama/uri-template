#lang racket/base

(provide uri-template-expression?
         uri-template?)

(module+ test
  (require rackunit)
  (require (only-in (file "util.rkt")
                    urls-equal?
                    let-test)))

(require net/url-structs)
(require br-parser-tools/lex)
(require brag/support)
(require (only-in racket/list
                  empty?
                  first
                  second
                  rest))
(require (only-in (file "expression-parser.brag")
                  parse))
(require (only-in (file "template-lexer.rkt")
                  lex-uri-template))

(define (expression-token? token)
  (eq? 'EXPRESSION (token-struct-type token)))

(define uri-template-expression-lexer
  (lexer
   [(eof)
    eof]
   ["{"
    (token "{" "{")]
   [#\+
    (token "+" "+")]
   [#\#
    (token "#" "#")]
   [#\.
    (token "." ".")]
   [#\/
    (token "/" "/")]
   [#\;
    (token ";" ";")]
   [#\?
    (token "?" "?")]
   [#\&
    (token "&" "&")]
   [#\=
    (token "=" "=")]
   [#\,
    (token "," ",")]
   [#\!
    (token "!" "!")]
   [#\@
    (token "@" "@")]
   ["|"
    (token "|" "|")]
   [#\_
    (token "_" "_")]
   [#\*
    (token "*" "*")]
   [#\:
    (token ":" ":")]
   [(:seq "%" (:= 2 (union (char-range "0" "9")
                           (char-range "a" "f")
                           (char-range "A" "F"))))
    (token 'PCT-ENCODED lexeme)]
   [(:seq (char-range "1" "9")
          (:* (char-range "0" "9")))
    (token 'NUMBER lexeme)]
   [(char-range "A" "z")
    (token 'LETTER lexeme)]
   ["}"
    (token "}" "}")]))

(define (lex-uri-template-expression s)
  (unless (string? s)
    (error "Cannot lext a non-string:" s))
  (apply-lexer uri-template-expression-lexer
               (open-input-string s)))

(module+ test
  (check-not-exn (lambda () (lex-uri-template-expression "{username}")))
  (check-not-exn (lambda () (lex-uri-template-expression "{term:1}")))
  (check-not-exn (lambda () (lex-uri-template-expression "{?q,lang}")))
  (check-not-exn (lambda () (lex-uri-template-expression "{?query,number}"))))

(define (make-expression-tokenizer port)
  (define (next-token)
    (uri-template-expression-lexer port))
  next-token)

(define (parse-uri-template-expression s)
  (syntax->datum (parse (make-expression-tokenizer (open-input-string s)))))

(define (uri-template-expression? x)
  (and (string? x)
       (with-handlers ([exn:fail:parsing? (lambda (e) #f)])
         (begin0
             #t
           (parse-uri-template-expression x)))))

(define (uri-template? x)
  (and (string? x)
       (with-handlers ([exn:fail:read? (lambda (e) #f)])
         (let ([lexed-template (lex-uri-template x)])
           (let ([expression-tokens (filter expression-token? lexed-template)])
             (let ([expressions (map token-struct-val expression-tokens)])
               (andmap uri-template-expression? expressions)))))))

(module+ test
  (check-true (uri-template? "http://example.com/~{username}/"))
  (check-true (uri-template? "http://example.com/dictionary/{term:1}/{term}"))
  (check-true (uri-template? "http://example.com/search{?q,lang}"))
  (check-true (uri-template? "http://www.example.com/foo{?query,number}"))
  (check-false (uri-template? "hi{"))
  (check-true (uri-template? "")))
