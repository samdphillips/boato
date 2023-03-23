#lang racket/base

(require (for-syntax json
                     racket/base
                     racket/match
                     racket/sequence
                     racket/syntax)
         syntax/parse/define
         "shape.rkt"
         "util.rkt")

(provide (all-defined-out))

(begin-for-syntax
  (provide (all-defined-out))

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

