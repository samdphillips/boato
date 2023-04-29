#lang racket/base

(require (for-syntax racket/base)
         syntax/parse
         syntax/parse/define)

(provide (all-defined-out))

(define service-space-introducer
  (make-interned-syntax-introducer 'boato/service))

(define (in-service-space id [mode 'add])
  (service-space-introducer id mode))

(define-syntax service-quote
  (syntax-parser
    [(_ name:id)
     #`(quote-syntax
        #,((make-interned-syntax-introducer 'boato/service) #'name))]))

(define (syntax-service-local-value id
                                    [missing (λ ()
                                               (raise-syntax-error
                                                'syntax-service-local-value
                                                "name not bound in service space"
                                                id))])
  (syntax-local-value (in-service-space id) missing))

(struct service-info (tbl endpoint-resolver) #:transparent)

(define-syntax-class service
  [pattern v:id
    #:do [(define lvalue (syntax-service-local-value #'v (λ () #f)))]
    #:fail-unless (and lvalue (service-info? lvalue)) "service name"
    #:with tbl (service-info-tbl lvalue)
    #:with endpoint-resolver (service-info-endpoint-resolver lvalue)])
