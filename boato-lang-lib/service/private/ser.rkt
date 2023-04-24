#lang racket/base

(require (for-syntax boato/service/private/service-support
                     racket/base)
         boato/service/private/ser/query
         syntax/parse/define)

(provide serialize)

(define-syntax-parser serialize
  [(_ svc:service op shp v)
   #:with protocol (hash-ref (syntax->datum (attribute svc.tbl)) 'protocol)
   #'(serialize protocol svc op shp v)]
  [(_ "query" svc op shp v)
   #'(serialize/query-top svc op shp v)])
