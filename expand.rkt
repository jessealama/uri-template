#lang racket

(provide expand-template)

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
  (log-error "exploding value ~a" value)
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
  (log-error "applying modifier ~a to value ~a" modifier value)
  (match modifier
    [#f
     value]
    [(list 'max-length max-length)
     (apply-max-length-modifier value max-length)]
    ["*"
     (explode value)]
    [else
     (error (format "Don't know how to apply modifier ~a to value ~a" modifier value))]))

(define/contract (render-items items separator)
  ((listof string?) string? . -> . string?)
  (cond ((empty? items)
         "")
        ((empty? (rest items))
         (first items))
        (else
         (format "~a~a~a"
                 (first items)
                 separator
                 (render-items (rest items)
                               separator)))))

(define/contract (expand-variable var assignment operator)
  (variable?
   assignment?
   (or/c false/c string?)
   . -> .
   string?)
  (log-error "expanding variable ~a" var)
  (log-error "modifier is ~a" (variable-modifier var))
  (log-error "operator is ~a" operator)
  (define val (assignment-ref assignment var))
  (define modifier (variable-modifier var))
  (cond ((string-value? val)
         (apply-modifier modifier val))
        ((list-value? val)
         (cond ((exploded? var)
                (render-items val (separator-for-operator operator)))
               (else
                (render-items val ","))))
        ((associative-array-value? val)
         (match modifier
           ["*"
            val]
           [else
            (log-error "turning assoc val into a list ~a" val)
            (apply-modifier modifier
                            (associative-array->list val))]))
        (else
         (error (format "Unexpected value: ~a" val)))))

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

(define/contract (apply-operator/string operator str-value)
  ((or/c false/c string?) string-value? . -> . string?)
  (log-error "applying operator ~a to string ~a" operator str-value)
  (match operator
    [#f
     (encode-value str-value)]
    ["+"
     (encode/reserved str-value)]
    ["#"
     (encode/reserved str-value)]
    ["."
     (encode/reserved str-value)]
    ["/"
     (encode/reserved str-value)]
    [";"
     (encode/reserved str-value)]
    ["?"
     (encode/reserved str-value)]
    ["&"
     (encode/reserved str-value)]
    [else
     (error "Unhandled operator ~a" operator)]))

(define/contract (apply-operator/list operator list)
  ((or/c false/c string?) list-value? . -> . string?)
  (log-error "applying operator ~a to list value ~a" operator list)
  (cond ((empty? list)
         "")
        ((empty? (rest list))
         (apply-operator/item operator (first list)))
        (else
         (format "~a~a~a"
                 (apply-operator/item operator (first list))
                 (separator-for-operator operator)
                 (apply-operator/list operator (rest list))))))

(define/contract (apply-operator/associative operator assoc)
  ((or/c false/c string?)
   associative-array-value?
   . -> .
   string?)
  (log-error "applying operator ~a to assoc value ~a" operator assoc)
  (cond ((empty? assoc)
         "")
        ((empty? (rest assoc))
         (define k (first (first assoc)))
         (define v (second (first assoc)))
         (if (string=? "" v)
             k
             (format "~a=~a" k v)))
        (else
         (define k (first (first assoc)))
         (define v (second (first assoc)))
         (if (string=? "" v)
             (format "~a~a~a"
                     k
                     (separator-for-operator operator)
                     (apply-operator/associative operator (rest assoc)))
             (format "~a=~a~a~a"
                     k
                     v
                     (separator-for-operator operator)
                     (apply-operator/associative operator (rest assoc)))))))

(define/contract (apply-operator/item operator item)
  ((or/c false/c string?)
   (or/c string-value?
         list-value?
         associative-array-value?)
   . -> . string?)
  (log-error "applying operator ~a to item ~a" operator item)
  (cond ((string-value? item)
         (apply-operator/string operator item))
        ((list-value? item)
         (apply-operator/list operator item))
        (else
         (apply-operator/associative operator item))))

(define/contract (apply-operator operator expansions)
  ((or/c false/c string?)
   (listof (or/c string?
                 (listof string?)
                 (listof (listof string?))))
   . -> .
   string?)
  (log-error "applying operator ~a to expanded values ~a" operator expansions)
  (cond ((empty? expansions)
         "")
        ((empty? (rest expansions))
         (let ([h (first expansions)])
           (apply-operator/item operator h)))
        (else
         (let ([h (first expansions)]
               [t (rest expansions)])
           (format "~a~a~a"
                   (apply-operator/item operator h)
                   (separator-for-operator operator)
                   (apply-operator/list operator t))))))

(define/contract (expand-variables variables assignment operator)
  ((listof variable?)
   assignment?
   (or/c false/c string?)
   . -> .
   string?)
  (log-error "expanding variables ~a" variables)
  (log-error "operator = ~a" operator)
  (cond ((empty? variables)
         "")
        ((not (assignment-has-variable? assignment (first variables)))
         (expand-variables (rest variables)
                           assignment
                           operator))
        (else
         (define expansion (expand-variable (first variables) assignment operator))
         (cond ((empty-value? expansion)
                (expand-variables (rest variables)
                                  assignment
                                  operator))
               (else
                (match operator
                  [#f
                   expansion]
                  ["#"
                   (string-append "#" expansion)]
                  ["+"
                   expansion]
                  ["."
                   (string-append "." expansion)]
                  ["/"
                   (string-append "/" expansion)]
                  [";"
                   (string-append ";" expansion)]
                  ["?"
                   (string-append "?" expansion)]
                  ["&"
                   (string-append "&" expansion)]
                  [else
                   (error (format "wtf operator is ~a" operator))]))))))

(define/contract (expand-template tmpl assignment)
  (template? assignment? . -> . string?)
  (expand-variables (template-variables tmpl)
                    assignment
                    (template-operator tmpl)))

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
      ;; (check-template "{;count*}"
      ;;                 assignment
      ;;                 ";count=one;count=two;count=three")
      ;; (check-template "{?count}"
      ;;                 assignment
      ;;                 "?count=one,two,three")
      ;; (check-template "{?count*}"
      ;;                 assignment
      ;;                 "?count=one&count=two&count=three")
      ;; (check-template "{&count*}"
      ;;                 assignment
      ;;                 "&count=one&count=two&count=three")

      ;; examples from page 21 (simple string expansion)
      ;; (check-equal? (eval-template "{var}" assignment)
      ;;               "value")
      ;; (check-equal? (eval-template "{hello}" assignment)
      ;;               "Hello%20World%21")
      ;; (check-equal? "50%25"
      ;;               (eval-template "{half}" assignment))
      ;; (check-equal? "OX"
      ;;               (eval-template "O{empty}X" assignment))
      ;; (check-equal? "OX"
      ;;               (eval-template "O{undef}X" assignment))
      ;; (check-equal? (eval-template "{x,y}" assignment)
      ;;               "1024,768")
      ;; (check-equal? (eval-template "{x,hello,y}" assignment)
      ;;               "1024,Hello%20World%21,768")
      ;; (check-equal? (eval-template "?{x,empty}" assignment)
      ;;               "?1024,")
      ;; (check-equal? (eval-template "?{x,undef}" assignment)
      ;;               "?1024")
      ;; (check-equal? (eval-template "?{undef,y}" assignment)
      ;;               "?768")
      ;; (check-equal? (eval-template "{var:3}" assignment)
      ;;               "val")
      ;; (check-equal? (eval-template "{var:30}" assignment)
      ;;               "value")
      ;; (check-equal? (eval-template "{list}" assignment)
      ;;               "red,green,blue")
      ;; (check-equal? (eval-template "{list*}" assignment)
      ;;               "red,green,blue")
      ;; (check-equal? (eval-template "{keys}" assignment)
      ;;               "semi,%3B,dot,.,comma,%2C")
      ;; (check-equal? (eval-template "{keys*}" assignment)
      ;;               "semi=%3B,dot=.,comma=%2C")

      ;; ;; ;; examples from page 22 (reserved expansion)
      ;; (check-equal? (eval-template "{+var}" assignment)
      ;;               "value")
      ;; (check-equal? (eval-template "{+hello}" assignment)
      ;;               "Hello%20World!")
      ;; (check-equal? (eval-template "{+half}" assignment)
      ;;               "50%25")

      ;; (check-equal? (eval-template "{base}index" assignment)
      ;;               "http%3A%2F%2Fexample.com%2Fhome%2Findex")
      ;; (check-equal? (eval-template "{+base}index" assignment)
      ;;               "http://example.com/home/index")
      ;; (check-equal? "OX"
      ;;               (eval-template "O{+empty}X" assignment))
      ;; (check-equal? "OX"
      ;;               (eval-template "O{+undef}X" assignment))

      ;; (check-equal? "/foo/bar/here"
      ;;               (eval-template "{+path}/here" assignment))
      ;; (check-equal? "here?ref=/foo/bar"
      ;;               (eval-template "here?ref={+path}" assignment))
      ;; (check-equal? "up/foo/barvalue/here"
      ;;               (eval-template "up{+path}{var}/here" assignment))
      ;; (check-equal? "1024,Hello%20World!,768"
      ;;               (eval-template "{+x,hello,y}" assignment))
      ;; (check-equal? "/foo/bar,1024/here"
      ;;               (eval-template "{+path,x}/here" assignment))

      ;; (check-equal? (eval-template "{+path:6}/here" assignment)
      ;;               "/foo/b/here")
      ;; (check-equal? (eval-template "{+list}" assignment)
      ;;               "red,green,blue")
      ;; (check-equal? (eval-template "{+list*}" assignment)
      ;;               "red,green,blue")
      ;; (check-equal? (eval-template "{+keys}" assignment)
      ;;               "semi,;,dot,.,comma,,")
      ;; (check-equal? (eval-template "{+keys*}" assignment)
      ;;               "semi=;,dot=.,comma=,")

      ;; ;; ;; fragment expansion
      ;; (check-equal? (eval-template "{#var}" assignment)
      ;;               "#value")
      ;; (check-equal? (eval-template "{#hello}" assignment)
      ;;               "#Hello%20World!")
      ;; (check-equal? (eval-template "{#half}" assignment)
      ;;               "#50%25")
      ;; (check-equal? (eval-template "foo{#empty}" assignment)
      ;;               "foo#")
      ;; (check-equal? (eval-template "foo{#undef}" assignment)
      ;;               "foo")
      ;; (check-equal? (eval-template "{#x,hello,y}" assignment)
      ;;               "#1024,Hello%20World!,768")
      ;; (check-equal? (eval-template "{#path,x}/here" assignment)
      ;;               "#/foo/bar,1024/here")
      ;; (check-equal? (eval-template "{#path:6}/here" assignment)
      ;;               "#/foo/b/here")
      ;; (check-equal? (eval-template "{#list}" assignment)
      ;;               "#red,green,blue")
      ;; (check-equal? (eval-template "{#list*}" assignment)
      ;;               "#red,green,blue")
      ;; (check-equal? (eval-template "{#keys}" assignment)
      ;;               "#semi,;,dot,.,comma,,")
      ;; (check-equal? (eval-template "{#keys*}" assignment)
      ;;               "#semi=;,dot=.,comma=,")

      ;; ;; ;; label expansion
      ;; (check-equal? (eval-template "{.who}" assignment)
      ;;               ".fred")
      ;; (check-equal? (eval-template "{.who,who}" assignment)
      ;;               ".fred.fred")
      ;; (check-equal? (eval-template "{.half,who}" assignment)
      ;;               ".50%25.fred")
      ;; (check-equal? (eval-template "www{.dom*}" assignment)
      ;;               "www.example.com")
      ;; (check-equal? (eval-template "X{.var}" assignment)
      ;;               "X.value")
      ;; (check-equal? (eval-template "X{.empty}" assignment)
      ;;               "X.")
      ;; (check-equal? (eval-template "X{.undef}" assignment)
      ;;               "X")
      ;; (check-equal? (eval-template "X{.var:3}" assignment)
      ;;               "X.val")
      ;; (check-equal? (eval-template "X{.list}" assignment)
      ;;               "X.red,green,blue")
      ;; (check-equal? (eval-template "X{.list*}" assignment)
      ;;               "X.red.green.blue")
      ;; (check-equal? (eval-template "X{.keys}" assignment)
      ;;               "X.semi,%3B,dot,.,comma,%2C")
      ;; (check-equal? (eval-template "X{.keys*}" assignment)
      ;;               "X.semi=%3B.dot=..comma=%2C")
      ;; (check-equal? "X"
      ;;               (eval-template "X{.empty_keys}" assignment))
      ;; (check-equal? "X"
      ;;               (eval-template "X{.empty_keys*}" assignment))

      ;; ;; examples from page 24 (path segment expansion)
      ;; (check-equal? "/fred"
      ;;               (eval-template "{/who}" assignment))
      ;; (check-equal? "/fred/fred"
      ;;               (eval-template "{/who,who}" assignment))
      ;; (check-equal? "/50%25/fred"
      ;;               (eval-template "{/half,who}" assignment))
      ;; (check-equal? "/fred/me%2Ftoo"
      ;;               (eval-template "{/who,dub}" assignment))
      ;; (check-equal? "/value"
      ;;               (eval-template "{/var}" assignment))
      ;; (check-equal? "/value/"
      ;;               (eval-template "{/var,empty}" assignment))
      ;; (check-equal? "/value"
      ;;               (eval-template "{/var,undef}" assignment))
      ;; (check-equal? "/value/1024/here"
      ;;               (eval-template "{/var,x}/here" assignment))
      ;; (check-equal? "/v/value"
      ;;               (eval-template "{/var:1,var}" assignment))
      ;; (check-equal? "/red,green,blue"
      ;;               (eval-template "{/list}" assignment))
      ;; (check-equal? "/red/green/blue"
      ;;               (eval-template "{/list*}" assignment))
      ;; (check-equal? "/red/green/blue/%2Ffoo"
      ;;               (eval-template "{/list*,path:4}" assignment))
      ;; (check-equal? "/semi,%3B,dot,.,comma,%2C"
      ;;               (eval-template "{/keys}" assignment))
      ;; (check-equal? "/semi=%3B/dot=./comma=%2C"
      ;;               (eval-template "{/keys*}" assignment))

      ;; ;; examples from page 25 (path-style parameter expansion)
      ;; (check-equal? ";who=fred"
      ;;               (eval-template "{;who}" assignment))
      ;; (check-equal? ";half=50%25"
      ;;               (eval-template "{;half}" assignment))
      ;; (check-equal? ";empty"
      ;;               (eval-template "{;empty}" assignment))
      ;; (check-equal? ";v=6;empty;who=fred"
      ;;               (eval-template "{;v,empty,who}" assignment))
      ;; (check-equal? ";v=6;who=fred"
      ;;               (eval-template "{;v,bar,who}" assignment))
      ;; (check-equal? ";x=1024;y=768"
      ;;               (eval-template "{;x,y}" assignment))
      ;; (check-equal? ";x=1024;y=768;empty"
      ;;               (eval-template "{;x,y,empty}" assignment))
      ;; (check-equal? ";x=1024;y=768"
      ;;               (eval-template "{;x,y,undef}" assignment))
      ;; (check-equal? ";hello=Hello"
      ;;               (eval-template "{;hello:5}" assignment))
      ;; (check-equal? ";list=red,green,blue"
      ;;               (eval-template "{;list}" assignment))
      ;; (check-equal? ";list=red;list=green;list=blue"
      ;;               (eval-template "{;list*}" assignment))
      ;; (check-equal? ";keys=semi,%3B,dot,.,comma,%2C"
      ;;               (eval-template "{;keys}" assignment))
      ;; (check-equal? ";semi=%3B;dot=.;comma=%2C"
      ;;               (eval-template "{;keys*}" assignment))

      ;; ;; form-style query expansion
      ;; (check-equal? "?who=fred"
      ;;               (eval-template "{?who}" assignment))
      ;; (check-equal? "?half=50%25"
      ;;               (eval-template "{?half}" assignment))
      ;; (check-equal? "?x=1024&y=768"
      ;;               (eval-template "{?x,y}" assignment))
      ;; (check-equal? "?x=1024&y=768&empty="
      ;;               (eval-template "{?x,y,empty}" assignment))
      ;; (check-equal? "?x=1024&y=768"
      ;;               (eval-template "{?x,y,undef}" assignment))
      ;; (check-equal? "?var=val"
      ;;               (eval-template "{?var:3}" assignment))
      ;; (check-equal? "?list=red,green,blue"
      ;;               (eval-template "{?list}" assignment))
      ;; (check-equal? "?list=red&list=green&list=blue"
      ;;               (eval-template "{?list*}" assignment))
      ;; (check-equal? "?keys=semi,%3B,dot,.,comma,%2C"
      ;;               (eval-template "{?keys}" assignment))
      ;; (check-equal? "?semi=%3B&dot=.&comma=%2C"
      ;;               (eval-template "{?keys*}" assignment))

      ;; ;; form-style query continuation (page 26)
      ;; (check-equal? "&who=fred"
      ;;               (eval-template "{&who}" assignment))
      ;; (check-equal? "&half=50%25"
      ;;               (eval-template "{&half}" assignment))
      ;; (check-equal? "?fixed=yes&x=1024"
      ;;               (eval-template "?fixed=yes{&x}" assignment))
      ;; (check-equal? "&x=1024&y=768&empty="
      ;;               (eval-template "{&x,y,empty}" assignment))
      ;; (check-equal? "&x=1024&y=768"
      ;;               (eval-template "{&x,y,undef}" assignment))
      ;; (check-equal? "&var=val"
      ;;               (eval-template "{&var:3}" assignment))
      ;; (check-equal? "&list=red,green,blue"
      ;;               (eval-template "{&list}" assignment))
      ;; (check-equal? "&list=red&list=green&list=blue"
      ;;               (eval-template "{&list*}" assignment))
      ;; (check-equal? "&keys=semi,%3B,dot,.,comma,%2C"
      ;;               (eval-template "{&keys}" assignment))
      ;; (check-equal? "&semi=%3B&dot=.&comma=%2C"
      ;;               (eval-template "{&keys*}" assignment))
      )))
