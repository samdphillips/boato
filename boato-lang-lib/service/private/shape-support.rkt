#lang racket/base

(require racket/promise
         syntax/parse)

(provide (all-defined-out))

(define (same-id? a b)
    (equal? (syntax->datum a)
            (syntax->datum b)))

(define-syntax-class simple-shape-type
  [pattern "blob"]
  [pattern "boolean"]
  [pattern "integer"]
  [pattern "string"]
  [pattern "timestamp"])

(struct shape-info (ctc))
(struct struct-shape-info shape-info ())
(struct list-shape-info shape-info ())
(struct map-shape-info shape-info ())

(define (shape-contract v)
  (force (shape-info-ctc (syntax-local-value v))))

(define-syntax-class shape
  [pattern v
    #:declare v (static shape-info? "shape identifier")])

(define-syntax-class shape-member
  [pattern [{~alt {~once ({~literal shape} . shape-str:string)} _} ...]
           #:with shape (datum->syntax #'shape-str (string->symbol (syntax->datum #'shape-str)))])
