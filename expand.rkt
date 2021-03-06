#lang racket

(provide (rename-out [eval-template
                      expand-template]))

(require racket/contract
         racket/list
         racket/match
         racket/string
         net/uri-codec
         br/define
         (only-in (file "parse.rkt")
                  parse-template)
         (only-in (file "assignment.rkt")
                  assignment?
                  assignment-ref
                  assignment-has-variable?)
         (only-in (file "template.rkt")
                  template?
                  template-variables
                  template-operator)
         (only-in (file "variable.rkt")
                  variable?
                  variable-name
                  variable-modifier
                  exploded?)
         (only-in (file "value.rkt")
                  value?
                  list-value?
                  string-value?
                  associative-array-value?
                  associative-array-keys
                  associative-array-ref
                  associative-array->list
                  empty-value?))

(module+ test
  (require rackunit)
  (define-macro (check-template TMPL ASSIGNMENT EXPECTED)
    #'(check-equal? (eval-template TMPL ASSIGNMENT)
                    EXPECTED
                    TMPL)))

(define/contract (encode-value val)
  (string? . -> . string?)
  (uri-unreserved-encode val))

(define/contract (encode/reserved str)
  (-> string? string?)
  (string-replace (string-replace str "%" "%25")
                  " "
                  "%20"))

(define/contract (explode value)
  (value? . -> . (or/c string-value? list-value?))
  value)

(define/contract (apply-max-length-modifier str-value max-length)
  (string-value? exact-integer? . -> . string?)
  (substring str-value
             0
             (min (string-length str-value)
                  max-length)))

(define/contract (apply-modifier modifier value)
  ((or/c false/c string? list?)
   value?
   . -> .
   value?)
  (match modifier
    [#f
     value]
    [(list 'max-length max-length)
     (apply-max-length-modifier value max-length)]
    ["*"
     (explode value)]
    [else
     (error (format "Don't know how to apply modifier ~a to value ~a" modifier value))]))

(module+ test
  (check-exn exn:fail:contract?
             (lambda ()
               (apply-modifier (list 'max-length 5)
                               (list "boom")))))

(define/contract (render-items items separator encoder)
  ((listof string?) string? (-> string? string?) . -> . string?)
  (cond ((empty? items)
         "")
        ((empty? (rest items))
         (encoder (first items)))
        (else
         (format "~a~a~a"
                 (encoder (first items))
                 separator
                 (render-items (rest items)
                               separator
                               encoder)))))

(define/contract (render-associations associations separator encoder)
  ((listof (listof string?)) string? (-> string? string?) . -> . string?)
  (match associations
    [(list)
     ""]
    [(list (list a ""))
     (encoder a)]
    [(list (list a b))
     (format "~a=~a"
             (encoder a)
             (encoder b))]
    [(cons (list a "") more)
     (format "~a~a~a"
             (encoder a)
             separator
             (render-associations more
                                  separator
                                  encoder))]
    [(cons (list a b) more)
     (format "~a=~a~a~a"
             (encoder a)
             (encoder b)
             separator
             (render-associations more
                                  separator
                                  encoder))]))

(define/contract (render-associations/allow-empty associations separator encoder)
  ((listof (listof string?)) string? (-> string? string?) . -> . string?)
  (match associations
    [(list)
     ""]
    [(list (list a b))
     (format "~a=~a"
             (encoder a)
             (encoder b))]
    [(cons (list a b) more)
     (format "~a=~a~a~a"
             (encoder a)
             (encoder b)
             separator
             (render-associations/allow-empty more
                                              separator
                                              encoder))]))

(define/contract (render-value-for-variable/default var val)
  (variable? value? . -> . string?)
  (cond ((string-value? val)
         val)
        ((list-value? val)
         (render-items val "," encode-value))
        ((associative-array-value? val)
         (cond ((exploded? var)
                (render-associations val "," encode-value))
               (else
                (render-items (associative-array->list val)
                              ","
                              encode-value))))))

(define/contract (expand-variable/default var assignment)
  (variable?
   assignment?
   . -> .
   string?)
  (define val (assignment-ref assignment var))
  (define modifier (variable-modifier var))
  (cond ((string-value? val)
         (encode-value (apply-modifier modifier val)))
        ((list-value? val)
         (render-items val "," encode-value))
        ((associative-array-value? val)
         (cond ((exploded? var)
                (render-associations val "," encode-value))
               (else
                (render-items (associative-array->list val) "," encode-value))))))

(define/contract (expand-variable/path var assignment)
  (variable?
   assignment?
   . -> .
   string?)
  (define val (assignment-ref assignment var))
  (define modifier (variable-modifier var))
  (cond ((string-value? val)
         (encode-value (apply-modifier modifier val)))
        ((list-value? val)
         (cond ((exploded? var)
                (render-items val "/" encode-value))
               (else
                (render-items val "," encode-value))))
        ((associative-array-value? val)
         (cond ((exploded? var)
                (render-associations val "/" encode-value))
               (else
                (render-items (associative-array->list val) "," encode-value))))))

(define/contract (expand-variable/path-style var assignment)
  (variable?
   assignment?
   . -> .
   string?)
  (define val (assignment-ref assignment var))
  (define modifier (variable-modifier var))
  (cond ((string-value? val)
         (render-associations (list (list (variable-name var)
                                                      (apply-modifier modifier val)))
                                          ";"
                                          encode-value))
        ((list-value? val)
         (cond ((exploded? var)
                (render-associations (map (lambda (x)
                                                        (list (variable-name var) x))
                                                      val)
                                                 ";"
                                                 encode-value))
               (else
                (format "~a=~a"
                        (variable-name var)
                        (render-items val "," encode-value)))))
        ((associative-array-value? val)
         (cond ((exploded? var)
                (render-associations val ";" encode-value))
               (else
                (format "~a=~a"
                        (variable-name var)
                        (render-items (associative-array->list val) "," encode-value)))))))

(define/contract (expand-variable/form-style var assignment)
  (variable?
   assignment?
   . -> .
   string?)
  (define val (assignment-ref assignment var))
  (define modifier (variable-modifier var))
  (cond ((string-value? val)
         (render-associations/allow-empty (list (list (variable-name var)
                                          (apply-modifier modifier val)))
                              "&"
                              encode-value))
        ((list-value? val)
         (cond ((exploded? var)
                (render-associations/allow-empty (map (lambda (x)
                                            (list (variable-name var) x))
                                          val)
                                     "&"
                                     encode-value))
               (else
                (format "~a=~a"
                        (variable-name var)
                        (render-items val "," encode-value)))))
        ((associative-array-value? val)
         (cond ((exploded? var)
                (render-associations/allow-empty val "&" encode-value))
               (else
                (format "~a=~a"
                        (variable-name var)
                        (render-items (associative-array->list val) "," encode-value)))))))

(define/contract (expand-variable/form-continuation-style var assignment)
  (variable?
   assignment?
   . -> .
   string?)
  (define val (assignment-ref assignment var))
  (define modifier (variable-modifier var))
  (cond ((string-value? val)
         (render-associations/allow-empty (list (list (variable-name var)
                                                      (apply-modifier modifier val)))
                                          "&"
                                          encode-value))
        ((list-value? val)
         (cond ((exploded? var)
                (render-associations/allow-empty (map (lambda (x)
                                            (list (variable-name var) x))
                                          val)
                                     "&"
                                     encode-value))
               (else
                (format "~a=~a"
                        (variable-name var)
                        (render-items val "," encode-value)))))
        ((associative-array-value? val)
         (cond ((exploded? var)
                (render-associations/allow-empty val "&" encode-value))
               (else
                (format "~a=~a"
                        (variable-name var)
                        (render-items (associative-array->list val) "," encode-value)))))))

(define/contract (expand-variable/reserved var assignment)
  (variable?
   assignment?
   . -> .
   string?)
  (define val (assignment-ref assignment var))
  (define modifier (variable-modifier var))
  (cond ((string-value? val)
         (encode/reserved (apply-modifier modifier val)))
        ((list-value? val)
         (render-items val "," encode/reserved))
        ((associative-array-value? val)
         (cond ((exploded? var)
                (render-associations val "," encode/reserved))
               (else
                (render-items (associative-array->list val) "," encode/reserved))))))

(define/contract (expand-variable/fragment var assignment)
  (variable?
   assignment?
   . -> .
   string?)
  (define val (assignment-ref assignment var))
  (define modifier (variable-modifier var))
  (cond ((string-value? val)
         (encode/reserved (apply-modifier modifier val)))
        ((list-value? val)
         (render-items val "," encode/reserved))
        ((associative-array-value? val)
         (cond ((exploded? var)
                (render-associations val "," encode/reserved))
               (else
                (render-items (associative-array->list val) "," encode/reserved))))))

(define/contract (expand-variable/label var assignment)
  (variable?
   assignment?
   . -> .
   string?)
  (define val (assignment-ref assignment var))
  (define modifier (variable-modifier var))
  (cond ((string-value? val)
         (encode-value (apply-modifier modifier val)))
        ((list-value? val)
         (cond ((exploded? var)
                (render-items val "." encode-value))
               (else
                (render-items val "," encode-value))))
        ((associative-array-value? val)
         (cond ((exploded? var)
                (render-associations val "." encode-value))
               (else
                (render-items (associative-array->list val) "," encode-value))))))

(define/contract (expand-variable var assignment operator)
  (variable?
   assignment?
   (or/c false/c string?)
   . -> .
   string?)
  (match operator
    [#f
     (expand-variable/default var assignment)]
    ["/"
     (expand-variable/path var assignment)]
    [";"
     (expand-variable/path-style var assignment)]
    ["?"
     (expand-variable/form-style var assignment)]
    ["&"
     (expand-variable/form-continuation-style var assignment)]
    ["+"
     (expand-variable/reserved var assignment)]
    ["#"
     (expand-variable/fragment var assignment)]
    ["."
     (expand-variable/label var assignment)]
    [else
     (error (format "Don't know how to deal with operator ~a" operator))]))

(define separators
  (hash
   #f ","
   "+" ","
   "#" ","
   "." "."
   "/" "/"
   ";" ";"
   "?" "&"
   "&" "&"))

(define/contract (separator-for-operator operator)
  ((or/c false/c string?) . -> . string?)
  (unless (hash-has-key? separators operator)
    (error (format "Do not know separator for operator ~a." operator)))
  (hash-ref separators operator))

(define/contract (expand-variables variables assignment operator)
  ((listof variable?)
   assignment?
   (or/c false/c string?)
   . -> .
   string?)
  (cond ((empty? variables)
         "")
        ((empty? (rest variables))
         (expand-variable (first variables) assignment operator))
        (else
         (define expansion (expand-variable (first variables) assignment operator))
         (cond ((empty-value? expansion)
                (expand-variables (rest variables)
                                  assignment
                                  operator))
               (else
                (define expanded-tail (expand-variables (rest variables)
                                                        assignment
                                                        operator))
                (if (string=? "" expanded-tail)
                    (string-append expansion (separator-for-operator operator))
                    (format "~a~a~a"
                            expansion
                            (separator-for-operator operator)
                            expanded-tail)))))))

(define/contract (expand-template tmpl assignment)
  (template? assignment? . -> . string?)
  (define variables (template-variables tmpl))
  (define operator (template-operator tmpl))
  (define (has-value? var)
    (assignment-has-variable? assignment var))
  (define (is-empty? var)
    (define val (assignment-ref assignment var))
    (cond ((string-value? val)
           #f)
          ((list-value? val)
           (empty? val))
          ((associative-array-value? val)
           (empty? val))))
  (define variables-with-values (filter has-value? variables))
  (define variables/non-empty-value (filter-not is-empty? variables-with-values))
  (define expanded (expand-variables variables/non-empty-value
                    assignment
                    (template-operator tmpl)))
  (cond ((empty? variables/non-empty-value)
         "")
        (else
         (match operator
           [#f
            expanded]
           ["&"
            (string-append "&" expanded)]
           ["/"
            (string-append "/" expanded)]
           [";"
            (string-append ";" expanded)]
           ["?"
            (string-append "?" expanded)]
           ["+"
            expanded]
           ["#"
            (string-append "#" expanded)]
           ["."
            (string-append "." expanded)]
           [else
            (error (format "Unknown operator ~a" operator))]))))

(define/contract (expand-string str assignment)
  (string? assignment? . -> . string?)
  str)

(define/contract (expand-expressions exprs assignment)
  ((listof (or/c string? template?)) assignment? . -> . (listof string?))
  (cond ((empty? exprs)
         empty)
        ((string? (first exprs))
         (cons (expand-string (first exprs)
                              assignment)
               (expand-expressions (rest exprs)
                                   assignment)))
        ((template? (first exprs))
         (cons (expand-template (first exprs)
                                assignment)
               (expand-expressions (rest exprs)
                                   assignment)))))

(define/contract (eval-template template assignment)
  (string? assignment? . -> . string?)
  (apply string-append
         (expand-expressions (parse-template template)
                             assignment)))

(module+ test
  (check-exn exn:fail?
             (lambda ()
               (eval-template "hi{woah" (hash)))))

(module+ test
  ;; examples coming from the RFC (page 18)
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
                            "v" v
                            "x" x
                            "y" y
                            "empty" empty
                            "empty_keys" empty_keys)])
      ;; this had better be true!
      (check-true (assignment? assignment))

      ;; examples from page 20 of the RFC
      (check-template "{count}"
                      assignment
                      "one,two,three")
      (check-template "{count*}"
                      assignment
                      "one,two,three")
      (check-template "{/count}"
                      assignment
                      "/one,two,three")
      (check-template "{/count*}"
                      assignment
                      "/one/two/three")
      (check-template "{;count}"
                      assignment
                      ";count=one,two,three")
      (check-template "{;count*}"
                      assignment
                      ";count=one;count=two;count=three")
      (check-template "{?count}"
                      assignment
                      "?count=one,two,three")
      (check-template "{?count*}"
                      assignment
                      "?count=one&count=two&count=three")
      (check-template "{&count*}"
                      assignment
                      "&count=one&count=two&count=three")

      ;; examples from page 21 (simple string expansion)
      (check-template "{var}"
                      assignment
                      "value")
      (check-template "{hello}"
                      assignment
                      "Hello%20World%21")
      (eval-template "{half}"
                     assignment)
      (eval-template "O{empty}X"
                     assignment)
      (eval-template "O{undef}X"
                     assignment)
      (check-template "{x,y}"
                      assignment
                      "1024,768")
      (check-template "{x,hello,y}"
                      assignment
                      "1024,Hello%20World%21,768")
      (check-template "?{x,empty}"
                      assignment
                      "?1024,")
      (check-template "?{x,undef}"
                      assignment
                      "?1024")
      (check-template "?{undef,y}"
                      assignment
                      "?768")
      (check-template "{var:3}"
                      assignment
                      "val")
      (check-template "{var:30}"
                      assignment
                      "value")
      (check-template "{list}"
                      assignment
                      "red,green,blue")
      (check-template "{list*}"
                      assignment
                      "red,green,blue")
      (check-template "{keys}"
                      assignment
                      "semi,%3B,dot,.,comma,%2C")
      (check-template "{keys*}"
                      assignment
                      "semi=%3B,dot=.,comma=%2C")

      ;; examples from page 22 (reserved expansion)
      (check-template "{+var}"
                      assignment
                      "value")
      (check-template "{+hello}"
                      assignment
                      "Hello%20World!")
      (check-template "{+half}"
                      assignment
                      "50%25")
      (check-template "{base}index"
                      assignment
                      "http%3A%2F%2Fexample.com%2Fhome%2Findex")
      (check-template "{+base}index"
                      assignment
                      "http://example.com/home/index")
      (check-template "O{+empty}X"
                      assignment
                      "OX")
      (check-template "O{+undef}X"
                      assignment
                      "OX")
      (check-template "{+path}/here"
                      assignment
                      "/foo/bar/here")
      (check-template "here?ref={+path}"
                      assignment
                      "here?ref=/foo/bar")
      (check-template "up{+path}{var}/here"
                      assignment
                      "up/foo/barvalue/here")
      (check-template "{+x,hello,y}"
                      assignment
                      "1024,Hello%20World!,768")
      (check-template "{+path,x}/here"
                      assignment
                      "/foo/bar,1024/here")

      (check-template "{+path:6}/here"
                      assignment
                      "/foo/b/here")
      (check-template "{+list}"
                      assignment
                      "red,green,blue")
      (check-template "{+list*}"
                      assignment
                      "red,green,blue")
      (check-template "{+keys}"
                      assignment
                      "semi,;,dot,.,comma,,")
      (check-template "{+keys*}"
                      assignment
                      "semi=;,dot=.,comma=,")

      ;; fragment expansion
      (check-template "{#var}"
                      assignment
                      "#value")
      (check-template "{#hello}"
                      assignment
                      "#Hello%20World!")
      (check-template "{#half}"
                      assignment
                      "#50%25")
      (check-template "foo{#empty}"
                      assignment
                      "foo#")
      (check-template "foo{#undef}"
                      assignment
                      "foo")
      (check-template "{#x,hello,y}"
                      assignment
                      "#1024,Hello%20World!,768")
      (check-template "{#path,x}/here"
                      assignment
                      "#/foo/bar,1024/here")
      (check-template "{#path:6}/here"
                      assignment
                      "#/foo/b/here")
      (check-template "{#list}"
                      assignment
                      "#red,green,blue")
      (check-template "{#list*}"
                      assignment
                      "#red,green,blue")
      (check-template "{#keys}"
                      assignment
                      "#semi,;,dot,.,comma,,")
      (check-template "{#keys*}"
                      assignment
                      "#semi=;,dot=.,comma=,")

      ;; label expansion
      (check-template "{.who}"
                      assignment
                      ".fred")
      (check-template "{.who,who}"
                      assignment
                      ".fred.fred")
      (check-template "{.half,who}"
                      assignment
                      ".50%25.fred")
      (check-template "www{.dom*}"
                      assignment
                      "www.example.com")
      (check-template "X{.var}"
                      assignment
                      "X.value")
      (check-template "X{.empty}"
                      assignment
                      "X.")
      (check-template "X{.undef}"
                      assignment
                      "X")
      (check-template "X{.var:3}"
                      assignment
                      "X.val")
      (check-template "X{.list}"
                      assignment
                      "X.red,green,blue")
      (check-template "X{.list*}"
                      assignment
                      "X.red.green.blue")
      (check-template "X{.keys}"
                      assignment
                      "X.semi,%3B,dot,.,comma,%2C")
      (check-template "X{.keys*}"
                      assignment
                      "X.semi=%3B.dot=..comma=%2C")
      (check-template "X{.empty_keys}"
                      assignment
                      "X")
      (check-template "X{.empty_keys*}"
                      assignment
                      "X")

      ;; examples from page 24 (path segment expansion)
      (check-template "{/who}"
                      assignment
                      "/fred")
      (check-template "{/who,who}"
                      assignment
                      "/fred/fred")
      (check-template "{/half,who}"
                      assignment
                      "/50%25/fred")
      (check-template "{/who,dub}"
                      assignment
                      "/fred/me%2Ftoo")
      (check-template "{/var}"
                      assignment
                      "/value")
      (check-template "{/var,empty}"
                      assignment
                      "/value/")
      (check-template "{/var,undef}"
                      assignment
                      "/value")
      (check-template "{/var,x}/here"
                      assignment
                      "/value/1024/here")
      (check-template "{/var:1,var}"
                      assignment
                      "/v/value")
      (check-template "{/list}"
                      assignment
                      "/red,green,blue")
      (check-template "{/list*}"
                      assignment
                      "/red/green/blue")
      (check-template "{/list*,path:4}"
                      assignment
                      "/red/green/blue/%2Ffoo")
      (check-template "{/keys}"
                      assignment
                      "/semi,%3B,dot,.,comma,%2C")
      (check-template "{/keys*}"
                      assignment
                      "/semi=%3B/dot=./comma=%2C")

      ;; examples from page 25 (path-style parameter expansion)
      (check-template "{;who}"
                      assignment
                      ";who=fred")
      (check-template "{;half}"
                      assignment
                      ";half=50%25")
      (check-template "{;empty}"
                      assignment
                      ";empty")
      (check-template "{;v,empty,who}"
                      assignment
                      ";v=6;empty;who=fred")
      (check-template "{;v,bar,who}"
                      assignment
                      ";v=6;who=fred")
      (check-template "{;x,y}"
                      assignment
                      ";x=1024;y=768")
      (check-template "{;x,y,empty}"
                      assignment
                      ";x=1024;y=768;empty")
      (check-template "{;x,y,undef}"
                      assignment
                      ";x=1024;y=768")
      (check-template "{;hello:5}"
                      assignment
                      ";hello=Hello")
      (check-template "{;list}"
                      assignment
                      ";list=red,green,blue")
      (check-template "{;list*}"
                      assignment
                      ";list=red;list=green;list=blue")
      (check-template "{;keys}"
                      assignment
                      ";keys=semi,%3B,dot,.,comma,%2C")
      (check-template "{;keys*}"
                      assignment
                      ";semi=%3B;dot=.;comma=%2C")

      ;; form-style query expansion
      (check-template "{?who}"
                      assignment
                      "?who=fred")
      (check-template "{?half}"
                      assignment
                      "?half=50%25")
      (check-template "{?x,y}"
                      assignment
                      "?x=1024&y=768")
      (check-template "{?x,y,empty}"
                      assignment
                      "?x=1024&y=768&empty=")
      (check-template "{?x,y,undef}"
                      assignment
                      "?x=1024&y=768")
      (check-template "{?var:3}"
                      assignment
                      "?var=val")
      (check-template "{?list}"
                      assignment
                      "?list=red,green,blue")
      (check-template "{?list*}"
                      assignment
                      "?list=red&list=green&list=blue")
      (check-template "{?keys}"
                      assignment
                      "?keys=semi,%3B,dot,.,comma,%2C")
      (check-template "{?keys*}"
                      assignment
                      "?semi=%3B&dot=.&comma=%2C")

      ;; form-style query continuation (page 26)
      (check-template "{&who}"
                      assignment
                      "&who=fred")
      (check-template "{&half}"
                      assignment
                      "&half=50%25")
      (check-template "?fixed=yes{&x}"
                      assignment
                      "?fixed=yes&x=1024")
      (check-template "{&x,y,empty}"
                      assignment
                      "&x=1024&y=768&empty=")
      (check-template "{&x,y,undef}"
                      assignment
                      "&x=1024&y=768")
      (check-template "{&var:3}"
                      assignment
                      "&var=val")
      (check-template "{&list}"
                      assignment
                      "&list=red,green,blue")
      (check-template "{&list*}"
                      assignment
                      "&list=red&list=green&list=blue")
      (check-template "{&keys}"
                      assignment
                      "&keys=semi,%3B,dot,.,comma,%2C")
      (check-template "{&keys*}"
                      assignment
                      "&semi=%3B&dot=.&comma=%2C")
      )))
