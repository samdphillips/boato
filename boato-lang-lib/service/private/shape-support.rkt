#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
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
(struct list-shape-info shape-info ())
(struct map-shape-info shape-info ())

(define (shape-contract v)
  (force (shape-info-ctc (syntax-shape-local-value v))))

(define shape-space-introducer (make-interned-syntax-introducer 'boato/shape))

(define (in-shape-space id [mode 'add])
  (shape-space-introducer id mode))

(define-syntax shape-quote
  (syntax-parser
    [(name:id)
     #`(quote-syntax
        #,((make-interned-syntax-introducer 'boato/shape #'name)))]))

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
    #:attr members (struct-shape-info-members (attribute v.value))])

(define-syntax-class shape-member
  [pattern [{~alt {~once ({~literal shape} . shape-str:string)} _} ...]
    #:with shape
    (datum->syntax #'shape-str (string->symbol (syntax->datum #'shape-str)))])
