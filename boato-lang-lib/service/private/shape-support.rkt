#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (for-template racket/base)
         racket/promise
         syntax/parse)

(provide (all-defined-out))

(define (same-id? a b)
    (equal? (syntax->datum a)
            (syntax->datum b)))

(struct shape-info (ctc))
(struct simple-shape-info shape-info (type))
(struct struct-shape-info shape-info (members))
(struct struct-shape-member-info (name required? shape accessor))
(struct list-shape-info shape-info (member-shape))
(struct map-shape-info shape-info (key-shape value-shape))

(define (shape-contract v)
  (force (shape-info-ctc (syntax-shape-local-value v))))

(define shape-space-introducer (make-interned-syntax-introducer 'boato/shape))

(define (in-shape-space id [mode 'add])
  (shape-space-introducer id mode))

(define-syntax shape-quote
  (syntax-parser
    [(_ name:id)
     #`(quote-syntax
        #,((make-interned-syntax-introducer 'boato/shape) #'name))]))

(define (syntax-shape-local-value id
                                  [missing (λ ()
                                             (raise-syntax-error
                                              'syntax-shape-local-value
                                              "name not bound in shape space"
                                              id))])
  (syntax-local-value (in-shape-space id) missing))

(define-syntax-class simple-shape-type
  [pattern "blob"]
  [pattern "boolean"]
  [pattern "integer"]
  [pattern "string"]
  [pattern "timestamp"])

(define-syntax-class shape
  [pattern v:id
    #:do [(define lvalue (syntax-shape-local-value #'v (λ () #f)))]
    #:when (and lvalue (shape-info? lvalue))
    #:attr value lvalue])

(define-syntax-class simple-shape
  [pattern v:shape
    #:fail-unless (simple-shape-info? (attribute v.value)) "simple shape name"
    #:attr type (simple-shape-info-type (attribute v.value))])

(define-syntax-class struct-shape
  [pattern v:shape
    #:fail-unless (struct-shape-info? (attribute v.value)) "struct shape name"
    #:attr (members 1)
    (for/list ([m (in-list (struct-shape-info-members (attribute v.value)))])
      #`(#,(symbol->string (struct-shape-member-info-name m))
         #,(struct-shape-member-info-required? m)
         #,(struct-shape-member-info-shape m)
         #,(struct-shape-member-info-accessor m)))])

(define-syntax-class list-shape
  [pattern v:shape
    #:fail-unless (list-shape-info? (attribute v.value)) "list shape name"
    #:attr member-shape (list-shape-info-member-shape (attribute v.value))])

(define-syntax-class map-shape
  [pattern v:shape
    #:fail-unless (map-shape-info? (attribute v.value)) "map shape name"
    #:attr key-shape (map-shape-info-key-shape (attribute v.value))
    #:attr value-shape (map-shape-info-value-shape (attribute v.value))])

(define-syntax-class shape-member
  [pattern [{~alt {~once ({~literal shape} . shape-str:string)} _} ...]
    #:with shape
    (datum->syntax #'shape-str (string->symbol (syntax->datum #'shape-str)))])
