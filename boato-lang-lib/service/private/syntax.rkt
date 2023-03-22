#lang racket/base

(require (for-syntax json
                     racket/base
                     racket/match
                     racket/sequence
                     racket/syntax)
         gregor
         racket/contract
         syntax/parse/define)

(provide (all-defined-out))

(define (string/c #:min [n #f] #:max [m #f])
  (define name
    `(string/c ,@(if n `(#:min ,n) null)
               ,@(if m `(#:max ,m) null)))
  (flat-named-contract name
                       (and/c string?
                              (property/c string-length
                                          (integer-in n m)))))

(begin-for-syntax
  (provide (all-defined-out))
  (define-syntax-class hash-table
    [pattern v
      #:do [(define ht (syntax->datum #'v))]
      #:when (hash? ht)
      #:attr items (datum->syntax #'v (hash->list ht) #'v)])

  (define (id->keyword i)
    (define kw
      (string->keyword
       (symbol->string
        (syntax->datum i))))
    (datum->syntax i kw i))

  (define (same-id? a b)
    (equal? (syntax->datum a)
            (syntax->datum b)))

  (define-syntax-class simple-shape-type
    [pattern "integer"]
    [pattern "string"]
    [pattern "timestamp"])

  (struct shape-info (ctc) #:transparent)

  (define (shape-contract v)
    (shape-info-ctc (syntax-local-value v)))

  (define-syntax-class shape
    [pattern v
      #:declare v (static shape-info? "shape identifier")])

  (struct static-service-metadata (tbl) #:transparent)

  (define-syntax-class service
    [pattern v
      #:declare v (static static-service-metadata? "service identifier")
      #:with tbl  (static-service-metadata-tbl (syntax-local-value #'v))])

  (struct static-operation-metadata (tbl) #:transparent)

  (define-syntax-class operation
    [pattern v
      #:declare v (static static-operation-metadata? "operation identifier")
      #:with tbl  (static-operation-metadata-tbl (syntax-local-value #'v))])
  )

(define-syntax-parser ht-expand
  [(_ k:id (kargs ...) #:keywords? kw?:boolean ht:hash-table)
   #:with ([keys . vals] ...) #'ht.items
   #:with (kws ...) (for/list ([k (in-syntax #'(keys ...))]) (id->keyword k))
   (if (syntax->datum #'kw?)
       #'(k kargs ... (~@ kws vals) ...)
       #'(k kargs ... [keys vals] ...))])

;; debugging
(define-syntax qls
  (syntax-rules () [(_ . vs) 'vs]))

(define-syntax-parser define-service-schema
  [(_ filename:string)
   #:with schema
   (datum->syntax #'filename
                  (with-input-from-file(syntax->datum #'filename) read-json)
                  #'filename)
   #'(define-service schema)])

(define-syntax-parser define-service
  [(_ v:hash-table) #'(ht-expand define-service () #:keywords? #t v)]
  [(_ {~alt {~once {~seq #:operations    ops}}
            {~once {~seq #:metadata      metadata:hash-table}}
            {~once {~seq #:version       _}}
            {~once {~seq #:shapes        shapes}}
            {~once {~seq #:documentation _}}} ...)
   #:do [(define service-id-str
           (string-downcase
            (hash-ref (syntax->datum #'metadata) 'serviceId)))]
   #:with service-id (format-id #'metadata "~a-service" service-id-str)
   #'(begin
       (provide service-id)
       (define-syntax service-id
         (static-service-metadata 'metadata))
       (define-service-shapes shapes))])

(define-syntax-parser define-service-shapes
  [(_ v:hash-table) #'(ht-expand define-service-shapes () #:keywords? #f v)]
  [(_ [shape-name shape-descr] ...)
   #'(begin (define-service-shape shape-name shape-descr) ...)])

(define-syntax-parser define-service-shape
  [(_ shape-name v:hash-table)
   #'(ht-expand define-service-shape (shape-name) #:keywords? #t v)]
  [(_ shape-name
      {~alt {~once {~seq #:type "structure"}}
            {~once {~seq #:exception #t}}
            (~optional {~seq #:documentation _})
            rest
            } ...)
   #'(begin)
   #;#'(define-service-exception-shape shape-name rest ...)]
  [(_ shape-name
      {~alt {~once {~seq #:type "structure"}}
            (~optional {~seq #:documentation _})
            rest} ...)
   #'(define-service-structure-shape shape-name rest ...)]
  [(_ shape-name
      {~alt {~once {~seq #:type "list"}}
            rest} ...)
   #'(define-service-list-shape shape-name rest ...)]
  [(_ shape-name
      {~alt {~once {~seq #:type type-name:simple-shape-type}}
            rest} ...)
   #'(define-service-simple-shape shape-name type-name rest ...)])

(define-syntax-parser define-service-structure-shape
  [(_ name:id
      {~alt {~once {~seq #:members members:hash-table}}
            {~optional {~seq #:required required-members}
                       #:defaults ([required-members #'()])}
            } ...)
   #:with ([member-names . member-tbl] ...) #'members.items
   #:with (member-shapes ...)
   (for/list ([t (in-syntax #'(member-tbl ...))])
     (datum->syntax t (string->symbol (hash-ref (syntax->datum t) 'shape)) t))
   #:with (reqd ...)
   (for/list ([n (in-syntax #'required-members)])
     (datum->syntax n (string->symbol (syntax->datum n)) n))
   #'(define-service-structure-shape name
       #:members ([member-names member-shapes] ...)
       #:required (reqd ...))]
  [(_ name:id
      {~alt {~once {~seq #:members ([member-names member-shapes] ...)}}
            {~once {~seq #:required (required-members ...)}}} ...)
   #:with struct-name (format-id #'name "struct:~a" #'name)
   #:with name? (format-id #'name "~a?" #'name)
   #:with num-fields
   (datum->syntax this-syntax (length (syntax->datum #'(member-names ...))))
   #:with make-name (format-id #'name "make-~a" #'name)
   #:with (kws ...)
   (for/list ([n (in-syntax #'(member-names ...))]) (id->keyword n))
   #:with (kwargs ...)
   (let ([reqd (syntax->list #'(required-members ...))])
     (for/list ([n (in-syntax #'(member-names ...))])
       (if (member n reqd same-id?)
           #`#,n
           #`[#,n (void)])))
   #:with ((pos acc-names) ...)
   (for/list ([i (in-naturals)]
              [n (in-syntax #'(member-names ...))])
     (list (datum->syntax this-syntax i) (format-id n "~a-~a" #'name n)))
   #:do [(syntax-local-lift-module-end-declaration
          #'(provide-shape-maker make-name name?
                                ([kws member-names member-shapes] ...)
                                (required-members ...)))]
   #'(begin
       ;; TODO compile-time information for serde
       (define-values (struct-name constr name? acc mut)
         (make-struct-type 'name #f num-fields 0))
       (define (make-name {~@ kws kwargs} ...)
         (constr member-names ...))
       (define-values (acc-names ...)
         (values (make-struct-field-accessor acc pos 'member-names) ...))
       (define-syntax name (shape-info #'name?)))])

(define-syntax-parser provide-shape-maker
  #;[(_ . xs) #'(begin)]
  [(_ name:id
      name?:id
      ([kws:keyword member-names:id shapes:id] ...)
      (required-members:id ...))
   #:do [(define reqd (syntax->list #'(required-members ...)))]
   #:with ([mkw mdom] ...)
   (for/list ([kw (in-syntax #'(kws ...))]
              [n  (in-syntax #'(member-names ...))]
              [s  (in-syntax #'(shapes ...))]
              #:when (member n reqd same-id?))
     (list kw (shape-contract s)))
   #:with ([okw odom] ...)
   (for/list ([kw (in-syntax #'(kws ...))]
              [n  (in-syntax #'(member-names ...))]
              [s  (in-syntax #'(shapes ...))]
              #:when (not (member n reqd same-id?)))
     (list kw (shape-contract s)))
   #'(provide
      (contract-out
       [name (->* ({~@ mkw mdom} ...) ({~@ okw odom} ...) name?)]))])

(define-syntax-parser define-service-list-shape
  [(_ name . xs)
   #:do [(displayln #'(name xs))]
   #'(define-syntax name (shape-info #'any/c))])

(define-syntax-parser define-service-simple-shape
  ;; XXX eat the pattern argument.  Needs a conversion function from the
  ;; regexes used in the service document to Racket regexes
  [(_ name "string"
      {~alt {~optional {~seq #:pattern _}}
            rest} ...)
   #'(define-syntax name (shape-info #'(string/c rest ...)))]
  [(_ name "integer"
      {~alt {~optional {~seq #:min n}
                       #:defaults ([n #'#f])}
            {~optional {~seq #:max m}
                       #:defaults ([m #'#f])}} ...)
   #'(define-syntax name (shape-info #'(integer-in n m)))]
  [(_ name "timestamp")
   #'(define-syntax name (shape-info #'datetime-provider?))])

