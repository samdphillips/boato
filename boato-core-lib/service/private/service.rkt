#lang racket/base

(require (for-syntax json
                     racket/base
                     racket/match
                     racket/sequence
                     racket/syntax
                     "service-support.rkt"
                     "support.rkt")
         net/url-string
         syntax/parse/define
         boato/endpoint
         "operation.rkt"
         "shape.rkt")

(provide (all-defined-out))

(define-syntax-parser define-service-schema
  [(_ filename:string)
   #:with schema
   (datum->syntax #'filename
                  (with-input-from-file(syntax->datum #'filename) read-json)
                  #'filename)
   #'(define-service schema)])

(define-syntax-parser define-service-syntax
  ([_ name:id rhs]
   (quasisyntax/loc this-syntax
     (define-syntax #,(in-service-space #'name) rhs))))

(define-syntax-parser define-service
  [(_ ht:hash-table) #'(define-service ht.kw-seq ...)]
  [(_ {~alt {~once {~seq #:metadata      metadata:hash-table}}
            {~once {~seq #:operations    ops}}
            {~once {~seq #:shapes        shapes}}
            {~once {~seq #:version       _}}
            {~once {~seq #:documentation _}}} ...)
   #:do [(define service-id-str
           (string-downcase
            (hash-ref (syntax->datum #'metadata) 'serviceId)))]
   #:with service-name-sym (string->symbol service-id-str)
   #:with service-id (format-id #'metadata "~a-service" service-id-str)
   #:with service-endpoint (format-id #'metadata "~a-endpoint" service-id-str)
   #'(begin
       (define service-endpoint (make-parameter #f))
       (define (endpoint-resolver)
         (define u
           (or (service-endpoint)
               (resolve-endpoint 'service-name-sym (aws-region))))
         (cond
           [(or (not u) (url? u)) u]
           [else
            (string->url u)]))
       (define-service-syntax service-id
         (service-info 'metadata #'endpoint-resolver))
       (define-service-shapes shapes)
       (define-service-operations service-id ops))])
