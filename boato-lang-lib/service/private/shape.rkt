#lang racket/base

(require (for-syntax racket/base
                     racket/promise
                     racket/sequence
                     racket/syntax
                     "shape-support.rkt")
         gregor
         racket/contract
         syntax/parse/define
         "contract.rkt"
         "util.rkt")

(provide (all-defined-out))

(define-syntax-parser define-service-shapes
  [(_ v:hash-table)
   #'(ht-expand define-service-shapes () #:keywords? #f v)]
  [(_ [shape-name shape-descr] ...)
   #'(begin
       (define-service-shape shape-name shape-descr) ...)])

(define-syntax-parser define-service-shape
  [(_ shape-name v:hash-table)
   #'(ht-expand define-service-shape (shape-name) #:keywords? #t v)]
  [(_ shape-name
      {~alt
       {~once {~seq #:type "structure"}}
       {~once {~seq #:exception #t}}
       (~optional {~seq #:documentation _})
       rest} ...)
   #'(begin)
   #;#'(define-service-exception-shape shape-name rest ...)]
  [(_ shape-name
      {~alt
       {~once {~seq #:type "structure"}}
       (~optional {~seq #:documentation _})
       rest} ...)
   #'(define-service-structure-shape shape-name rest ...)]
  [(_ shape-name {~alt {~once {~seq #:type "list"}} rest} ...)
   #'(define-service-list-shape shape-name rest ...)]
  [(_ shape-name {~alt {~once {~seq #:type "map"}} rest} ...)
   #'(define-service-map-shape shape-name rest ...)]
  [(_ shape-name
      {~alt {~once {~seq #:type type-name:simple-shape-type}} rest} ...)
   #'(define-service-simple-shape shape-name type-name rest ...)]
  [(_ shape-name {~alt {~once {~seq #:type type-name:string}} rest} ...)
   (raise-syntax-error 'define-service-shape
                       "unknown type name"
                       this-syntax
                       #'type-name)])

(define-syntax-parser define-service-structure-shape
  [(_ name:id
      {~alt
       {~once {~seq #:members members:hash-table}}
       {~optional
        {~seq #:required required-members}
        #:defaults ([required-members #'()])}} ...)
   #:with ([member-names . member-tbl] ...) #'members.items
   #:with (member-shapes ...)
   (for/list ([t (in-syntax #'(member-tbl ...))])
     (datum->syntax t
                    (string->symbol
                      (hash-ref (syntax->datum t) 'shape))
                    t))
   #:with
   (reqd ...)
   (for/list ([n (in-syntax #'required-members)])
     (datum->syntax n (string->symbol (syntax->datum n)) n))
   #'(define-service-structure-shape
      name
      #:members ([member-names member-shapes] ...)
      #:required (reqd ...))]

  [(_ name:id
      {~alt
       {~once {~seq #:members ([member-names member-shapes] ...)}}
       {~once {~seq #:required (required-members ...)}}} ...)
   #:with struct-name (format-id #'name "struct:~a" #'name)
   #:with name? (format-id #'name "~a?" #'name)
   #:with num-fields (datum->syntax
                      this-syntax
                      (length (syntax->datum #'(member-names ...))))
   #:with make-name (format-id #'name "make-~a" #'name)
   #:with (kws ...) (for/list ([n (in-syntax #'(member-names ...))])
                      (id->keyword n))
   #:with (kwargs
           ...) (let ([reqd (syntax->list #'(required-members ...))])
                  (for/list ([n (in-syntax #'(member-names ...))])
                    (if (member n reqd same-id?) #`#,n #`[#,n (void)])))
   #:with ((pos acc-names)
           ...) (for/list ([i (in-naturals)]
                           [n (in-syntax #'(member-names ...))])
                  (list (datum->syntax this-syntax i)
                        (format-id n "~a-~a" #'name n)))
   #:do [(syntax-local-lift-module-end-declaration
          #'(provide-structure-shape-maker
             make-name
             name?
             ([kws member-names member-shapes] ...)
             (required-members ...)))]
   #'(begin
       (define-values (struct-name constr name? acc mut)
         (make-struct-type 'name #f num-fields 0))
       (define (make-name {~@ kws kwargs} ...)
         (constr member-names ...))
       (define-values (acc-names ...)
         (values (make-struct-field-accessor acc pos 'member-names) ...))
       (define-syntax name (struct-shape-info #'name?)))])

(define-syntax-parser provide-structure-shape-maker
  [(_ name:id
      name?:id
      ([kws:keyword member-names:id shapes:id] ...)
      (required-members:id ...))
   #:do [(define reqd (syntax->list #'(required-members ...)))]
   #:with ([mkw mdom]
           ...) (for/list ([kw (in-syntax #'(kws ...))]
                           [n (in-syntax #'(member-names ...))]
                           [s (in-syntax #'(shapes ...))]
                           #:when (member n reqd same-id?))
                  (list kw (shape-contract s)))
   #:with ([okw odom]
           ...) (for/list ([kw (in-syntax #'(kws ...))]
                           [n (in-syntax #'(member-names ...))]
                           [s (in-syntax #'(shapes ...))]
                           #:when (not (member n reqd same-id?)))
                  (list kw (shape-contract s)))
   #'(provide (contract-out
               [name
                (->* ({~@ mkw mdom} ...) ({~@ okw odom} ...) name?)]))])

(define-syntax-parser define-service-list-shape
  [(_ name
      {~alt {~once {~seq #:member member-info:hash-table}} rest} ...)
   #:with member-data #'member-info.items
   #'(define-service-list-shape name #:member member-data rest ...)]
  [(_ name {~alt {~once {~seq #:member member:shape-member}} _} ...)
   #'(define-syntax name
       (list-shape-info
        (delay
         (with-syntax ([member-ctc (shape-contract #'member.shape)])
           #'(listof member-ctc)))))])

(define-syntax-parser define-service-map-shape
  [(_ name
      {~alt
       {~once {~seq #:key key-info:hash-table}}
       {~once {~seq #:value value-info:hash-table}}
       rest} ...)
   #:with key-data #'key-info.items
   #:with value-data #'value-info.items
   #'(define-service-map-shape name
                               #:key key-data
                               #:value value-data
                               rest ...)]
  [(_ name
      {~alt
       {~once {~seq #:key key-member:shape-member}}
       {~once {~seq #:value val-member:shape-member}}
       _} ...)
   #'(define-syntax name
       (map-shape-info
        (delay
         (with-syntax ([key-ctc (shape-contract #'key-member.shape)]
                       [val-ctc (shape-contract #'val-member.shape)])
           #'(hash/c key-ctc val-ctc #:flat? #t)))))])

(define-syntax-parser define-service-simple-shape
  [(_ name "blob") #'(define-syntax name (shape-info #'bytes?))]
  [(_ name "boolean") #'(define-syntax name (shape-info #'boolean?))]
  [(_ name
      "integer"
      {~alt
       {~optional {~seq #:min n} #:defaults ([n #'#f])}
       {~optional {~seq #:max m} #:defaults ([m #'#f])}
       ;; #:box doesn't appear to be used in botocore so ignore it
       {~optional {~seq #:box _}}} ...)
   #'(define-syntax name (shape-info #'(integer-in n m)))]
  ;; XXX eat the pattern argument.  Needs a conversion function from the
  ;; regexes used in the service document to Racket regexes
  [(_ name "string" {~alt {~optional {~seq #:pattern _}} rest} ...)
   #'(define-syntax name (shape-info #'(string/c rest ...)))]
  [(_ name "timestamp")
   #'(define-syntax name (shape-info #'datetime-provider?))])
