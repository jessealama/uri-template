#lang racket/base

(provide uri-template-expression?
         uri-template?
         parse-template)

(module+ test
  (require rackunit)
  (require (only-in (file "util.rkt")
                    urls-equal?
                    let-test)))

(require net/url-structs)
(require br-parser-tools/lex)
(require brag/support)
(require racket/match)
(require (only-in racket/list
                  empty?
                  empty
                  first
                  second
                  rest
                  index-of
                  split-at
                  index-where))
(require (only-in (file "expression-parser.brag")
                  parse))
(require (only-in (file "template-lexer.rkt")
                  lex-uri-template)
         (only-in (file "variable.rkt")
                  variable
                  variable?)
         (only-in (file "template.rkt")
                  template
                  template?))
(require racket/contract)

(define (expression-token? token)
  (eq? 'EXPRESSION (token-struct-type token)))

(define (text-token? token)
  (eq? 'TEXT (token-struct-type token)))

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

(define/contract (render-varchars varchars)
  ((listof (and/c (listof (or/c symbol? string?))
                  (not/c empty?)))
   . -> .
   string?)
  (cond ((empty? varchars)
         "")
        ((empty? (rest (first varchars)))
         (error (format "Unexpected element (too few members): ~a" (first varchars))))
        ((eq? 'varchar (first (first varchars)))
         (string-append (format "~a" (second (first varchars)))
                        (render-varchars (rest varchars))))
        (else
         (error (format "Unexpected element (does not match expected format): ~a" (first varchars))))))

(define/contract (simplify-varspec varspec)
  (list? . -> . variable?)
  (match varspec
    [(list 'varspec (cons 'varname more))
     (variable #f (render-varchars more))]
    [(list 'varspec (cons 'varname more) (list 'modifier-level4 modifier))
     (define name (render-varchars more))
     (match modifier
       [(list 'explode "*")
        (variable "*" name)]
       [(list 'prefix ":" (list 'max-length max-level))
        (variable (list 'max-length (string->number max-level))
                  name)]
       [else
        (error (format "Don't know how to handle modifier ~a" modifier))])]
    [else
     (error (format "Unable to handle varspec ~a" varspec))]))

(define/contract (simplify-variables varlist)
  (list? . -> . (listof variable?))
  (define no-commas (remove* (list ",") varlist))
  (map simplify-varspec no-commas))

(define/contract (simplify parsed-expression)
  (list? . -> . template?)
  (match parsed-expression
    [(list)
     empty]
    [(list 'expression "{" (cons 'variable-list l) "}")
     (template #f (simplify-variables l))]
    [(list 'expression "{" (list 'operator operator) (cons 'variable-list l) "}")
     (template operator (simplify-variables l))]
    [else
     (error (format "Cannot handle parsed expression ~a" parsed-expression))]))

(define/contract (parse-template/list chars)
  (-> (listof char?) (listof (or/c char? template?)))
  (cond ((empty? chars)
         empty)
        ((char=? #\{ (first chars))
         (define i (index-of chars #\} char=?))
         (cond ((integer? i)
                (define-values (up-to remainder)
                  (split-at chars (+ i 1)))
                (define s (list->string up-to))
                (define datum (syntax->datum
                               (parse
                                (make-expression-tokenizer (open-input-string s)))))
                (cons (simplify datum)
                      (parse-template/list remainder)))
               (else
                (error "open curly brace found without matching closing curly brace"))))
        (else
         (cons (first chars)
               (parse-template/list (rest chars))))))

(define/contract (render-parse parsed)
  ((listof (or/c char? template?)) . -> . (listof (or/c string? template?)))
  (cond ((empty? parsed)
         empty)
        ((template? (first parsed))
         (cons (first parsed)
               (render-parse (rest parsed))))
        ((char? (first parsed))
         (define i (index-where parsed template?))
         (cond ((integer? i)
                (define-values (before after)
                  (split-at parsed i))
                (cons (list->string before)
                      (render-parse after)))
               (else
                (list (list->string parsed)))))))

(define/contract (parse-template template)
  (string? . -> . (listof (or/c string? template?)))
  (render-parse (parse-template/list (string->list template))))

(module+ test
  (check-equal? '("hi")
                (parse-template "hi")))
