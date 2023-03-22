#lang racket/base

#|

(module sts racket/base
  (require "service-syntax.rkt")
  (define-service-schema "sts.json")

  (require (for-syntax racket/base)
           syntax/parse/define)

  (provide serialize)

  (define-syntax-parser serialize
    [(_ svc:service op shp v)
     #:with protocol (hash-ref (syntax->datum (attribute svc.tbl)) 'protocol)
     #'(serialize protocol svc op shp v)]
    [(_ "query" svc op shp v)
     #'(serialize/query-top svc op shp v)])

  (define-syntax-parser serialize/query-top
    [(_ svc:service op:operation shape v)
     #:with version (hash-ref (syntax->datum (attribute svc.tbl)) 'apiVersion)
     #:with action (hash-ref (syntax->datum (attribute op.tbl)) 'name)
     #'(let ([ser '((Action . action) (Version . version))])
         (serialize/query shape v ser))])

  (define-syntax-parser serialize/query
    [(_ shp:shape ser v)
     #'(begin)]
    )
  )

(require (for-syntax racket/base)
         "service-syntax.rkt"
         'sts)

(define-syntax AssumeRole
  (static-operation-metadata (hash 'name "AssumeRole")))
#;
(serialize sts-service AssumeRole AssumeRoleRequest v)

|#

(require boato/service/private/syntax)
(define-service-schema "sts.json")
