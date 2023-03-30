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
(struct simple-shape-info shape-info (type))
(struct struct-shape-info shape-info (members))
(struct struct-shape-member-info (name required? shape accessor))
(struct list-shape-info shape-info ())
(struct map-shape-info shape-info ())

(define (shape-contract v)
  (force (shape-info-ctc (syntax-local-value v))))

(define-syntax-class shape
  [pattern v
    #:declare v (static shape-info? "shape")])

(define-syntax-class simple-shape
  [pattern v
    #:declare v (static simple-shape-info? "simple shape")
    #:attr type (simple-shape-info-type (attribute v.value))])

(define-syntax-class struct-shape
  [pattern v
    #:declare v (static struct-shape-info? "struct shape")
    #:attr members (struct-shape-info-members (attribute v.value))])

(define-syntax-class shape-member
  [pattern [{~alt {~once ({~literal shape} . shape-str:string)} _} ...]
    #:with shape (datum->syntax #'shape-str (string->symbol (syntax->datum #'shape-str)))])
