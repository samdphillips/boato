#lang racket/base

(require (for-syntax racket/base
                     "service-support.rkt"
                     "support.rkt")
         net/url-string
         racket/pretty
         syntax/parse/define
         "ser.rkt"
         "util.rkt")

(provide (all-defined-out))

(define-syntax-parser define-service-operations
  [(_ service:id ops:hash-table)
   #'(define-service-operations service ops.kv-pairs ...)]
  [(_ service:id [op-name:id . op-descr] ...)
   #'(begin
       (define-service-operation service op-name op-descr) ...)])

(define-syntax-parser define-service-operation
  [(_ service:id op-name:id op-descr:hash-table)
   #'(define-service-operation service op-name op-descr.kw-seq ...)]
  [(_ service:id op-name:id
      {~alt {~once {~seq #:name name:string}}
            {~once {~seq #:http http:hash-table}}
            {~once {~seq #:input input:hash-table}}
            {~once {~seq #:output output:hash-table}}
            {~optional {~seq #:errors (errors:hash-table ...)}}
            {~optional {~seq #:documentation _}}
            rest} ...)
   #'(define-service-operation service op-name
       #:name name
       #:http (http.kw-seq ...)
       #:input (input.kw-seq ...)
       #:output (output.kw-seq ...)
       #:errors {~? ((errors.kw-seq ...) ...) ()})]
  [(_ service:service op-name:id
      {~alt {~once {~seq #:name name:string}}
            {~once {~seq #:http http}}
            {~once {~seq #:input input:part}}
            (~once (~seq #:output _))
            {~once {~seq #:errors _}}} ...)
   #'(define (op-name inv #:endpoint [endpoint (service.endpoint-resolver)])
       (unless (or (string? endpoint)
                   (url? endpoint))
         (raise-arguments-error 'op-name
                                "invalid endpoint for service"
                                "endpoint" endpoint))
       (define-values
         (op-path op-method op-hdrs op-body)
         (serialize service (name http) input.shape inv))
       (define op-url (combine-url/relative (->url endpoint) op-path))
       (values op-url op-method op-hdrs op-body))])