#lang racket/base

(require (for-syntax json
                     racket/base
                     racket/match
                     racket/sequence
                     racket/syntax
                     "service-support.rkt"
                     "support.rkt")
         syntax/parse/define
         "shape.rkt")

(provide (all-defined-out))

(define-syntax-parser define-service-schema
  [(_ filename:string)
   #:with schema
   (datum->syntax #'filename
                  (with-input-from-file(syntax->datum #'filename) read-json)
                  #'filename)
   #'(define-service schema)])

(define-syntax-parser define-service
  [(_ ht:hash-table) #'(define-service ht.kw-seq ...)]
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

