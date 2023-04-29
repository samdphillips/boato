#lang racket/base

(provide aws-region
         endpoint-resolver
         resolve-endpoint)

(define aws-region
  (make-parameter "us-east-1"))

(define endpoint-resolver
  (make-parameter (lambda (service region) #f)))

(define (resolve-endpoint service region)
  ((endpoint-resolver) service region))
