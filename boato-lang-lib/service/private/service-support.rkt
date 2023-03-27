#lang racket/base

(require syntax/parse)

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
