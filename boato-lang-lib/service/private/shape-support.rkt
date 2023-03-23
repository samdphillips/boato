#lang racket/base

(require syntax/parse)

(provide (all-defined-out))

(define (same-id? a b)
    (equal? (syntax->datum a)
            (syntax->datum b)))

(define-syntax-class simple-shape-type
  [pattern "integer"]
  [pattern "string"]
  [pattern "timestamp"])

(struct shape-info (ctc) #:transparent)

(define (shape-contract v)
  (shape-info-ctc (syntax-local-value v)))

(define-syntax-class shape
  [pattern v
    #:declare v (static shape-info? "shape identifier")])
