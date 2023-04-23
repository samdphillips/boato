#lang racket/base

(require (for-syntax racket/base
                     racket/promise
                     racket/list
                     racket/sequence
                     racket/syntax
                     syntax/parse/lib/function-header
                     syntax/struct
                     "shape-support.rkt"
                     "support.rkt")
         gregor
         racket/contract
         syntax/parse/define
         "contract.rkt")

(provide (all-defined-out))

(define-syntax-parser define-shape-syntax
  ([_ name:id rhs]
   (quasisyntax/loc this-syntax
     (define-syntax #,(in-shape-space #'name) rhs))))

(define-syntax-parser define-service-shapes
  [(_ ht:hash-table)
   #'(define-service-shapes ht.kv-pairs ...)]
  [(_ [shape-name:id . shape-descr] ...)
   #'(begin
       (define-service-shape shape-name shape-descr) ...)])

(define-syntax-parser define-service-shape
  [(_ shape-name:id ht:hash-table)
   #'(define-service-shape shape-name ht.kw-seq ...)]
  [(_ shape-name:id
      {~alt
       {~once {~seq #:type "structure"}}
       {~once {~seq #:exception #t}}
       (~optional {~seq #:documentation _})
       rest} ...)
   #'(begin)
   #;#'(define-service-exception-shape shape-name rest ...)]
  [(_ shape-name:id
      {~alt
       {~once {~seq #:type "structure"}}
       (~optional {~seq #:documentation _})
       rest} ...)
   #'(define-service-structure-shape shape-name rest ...)]
  [(_ shape-name:id {~alt {~once {~seq #:type "list"}} rest} ...)
   #'(define-service-list-shape shape-name rest ...)]
  [(_ shape-name:id {~alt {~once {~seq #:type "map"}} rest} ...)
   #'(define-service-map-shape shape-name rest ...)]
  [(_ shape-name:id
      {~alt {~once {~seq #:type type-name:simple-shape-type}} rest} ...)
   #'(define-service-simple-shape shape-name type-name rest ...)]
  [(_ shape-name:id {~alt {~once {~seq #:type type-name:string}} rest} ...)
   (raise-syntax-error 'define-service-shape
                       "unknown type name"
                       this-syntax
                       #'type-name)])

(begin-for-syntax
  (define (build-required-list ids reqd)
    (for/list ([n (in-list ids)])
       (and (member n reqd same-id?) #t)))

  (define (generate-kw-formals ids reqd)
    (append*
      (for/list ([id   (in-list ids)]
                 [req? (in-list reqd)])
        (define kw (id->keyword id))
        (define arg (generate-temporary id))
        (if req?
            (list kw arg)
            (list kw #`[#,arg (void)]))))))

(define-syntax-parser define-service-structure-shape
  [(_ name:id
      {~alt
       {~once {~seq #:members members:hash-table}}
       {~optional {~seq #:required required-members}
                  #:defaults ([required-members #'()])}} ...)
   #:with (member-shapes ...)
   (for/list ([t (in-syntax #'(members.vs ...))])
     (datum->syntax t
                    (string->symbol
                      (hash-ref (syntax->datum t) 'shape))
                    t))
   #:with (reqd ...)
   (for/list ([n (in-syntax #'required-members)])
     (datum->syntax n (string->symbol (syntax->datum n)) n))
   #'(define-service-structure-shape
      name
      #:members ([members.ks member-shapes] ...)
      #:required (reqd ...))]

  [(_ name:id
      {~alt
       {~once {~seq #:members ([member-names member-shapes] ...)}}
       {~once {~seq #:required (required-members ...)}}} ...)
   #:do [(define member-names-stx (syntax-e #'(member-names ...)))
         (define reqd
           (build-required-list member-names-stx
                                (syntax-e #'(required-members ...))))]
   #:with (member-requireds ...) (datum->syntax #'(required-members ...) reqd)
   #:with ctor (format-id #'name "mk-~a" #'name)
   #:with make (format-id #'name "make-~a" #'name)
   #:with struct-names
   (build-struct-names #'name member-names-stx #:constructor-name #'ctor #f #t)
   #:with (_ _ name? member-refs ...) #'struct-names
   #:with mk-struct
   (datum->syntax #'name
                  (build-struct-generation #'name
                                           member-names-stx
                                           #:constructor-name #'ctor
                                           #f #t))
   #:with kw+args:formals
   (generate-kw-formals member-names-stx reqd)
   #'(begin
       (define-values struct-names mk-struct)
       (define (make . kw+args) (ctor . kw+args.params))
       (define-shape-syntax name
         (struct-shape-info #'name?
                            (list
                             (struct-shape-member-info 'member-names
                                                       member-requireds
                                                       #'member-shapes
                                                       #'member-refs)
                             ...))))])

;; XXX: add this back in
#;
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
   #'(define-service-list-shape name
       #:member (member-info.kv-pairs ...) rest ...)]
  [(_ name {~alt {~once {~seq #:member member:shape-member}} _} ...)
   #'(define-shape-syntax name
       (list-shape-info
        (delay
         (with-syntax ([member-ctc (shape-contract #'member.shape)])
           #'(listof member-ctc)))
        #'member.shape))])

(define-syntax-parser define-service-map-shape
  [(_ name
      {~alt
       {~once {~seq #:key key-info:hash-table}}
       {~once {~seq #:value value-info:hash-table}}
       rest} ...)
   #'(define-service-map-shape name
                               #:key   (key-info.kv-pairs ...)
                               #:value (value-info.kv-pairs ...)
                               rest ...)]
  [(_ name
      {~alt
       {~once {~seq #:key key-member:shape-member}}
       {~once {~seq #:value val-member:shape-member}}
       _} ...)
   #'(define-shape-syntax name
       (map-shape-info
        (delay
         (with-syntax ([key-ctc (shape-contract #'key-member.shape)]
                       [val-ctc (shape-contract #'val-member.shape)])
           #'(hash/c key-ctc val-ctc #:flat? #t)))))])

(define-syntax-parser define-service-simple-shape
  [(_ name "blob")
   #'(define-shape-syntax name (simple-shape-info #'bytes? 'blob))]
  [(_ name "boolean")
   #'(define-shape-syntax name (simple-shape-info #'boolean? 'boolean))]
  [(_ name
      "integer"
      {~alt
       {~optional {~seq #:min n} #:defaults ([n #'#f])}
       {~optional {~seq #:max m} #:defaults ([m #'#f])}
       ;; #:box doesn't appear to be used in botocore so ignore it
       {~optional {~seq #:box _}}} ...)
   #'(define-shape-syntax name
       (simple-shape-info #'(integer-in n m) 'integer))]
  ;; XXX eat the pattern argument.  Needs a conversion function from the
  ;; regexes used in the service document to Racket regexes
  [(_ name "string" {~alt {~optional {~seq #:pattern _}} rest} ...)
   #'(define-shape-syntax name
       (simple-shape-info #'(string/c rest ...) 'string))]
  [(_ name "timestamp")
   #'(define-shape-syntax name
       (simple-shape-info #'datetime-provider? 'timestamp))])
