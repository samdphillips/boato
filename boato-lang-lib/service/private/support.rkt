#lang racket/base

(require racket/sequence
         syntax/parse)

(provide (all-defined-out))

(define-syntax-class hash-table
  [pattern v
    #:do [(define ht (syntax->datum #'v))]
    #:when (hash? ht)
    #:with (kv-pairs ...) (datum->syntax #'v (hash->list ht) #'v)
    #:with ([ks . vs] ...) #'(kv-pairs ...)
    #:with (kws ...) (for/list ([k (in-syntax #'(ks ...))]) (id->keyword k))
    #:with (kw-seq ...) #'({~@ kws vs} ...)])

(define-syntax-class part
  [pattern ({~alt {~once {~seq #:shape shape-str:string}} _} ...)
    #:with shape
    (datum->syntax #'shape-str (string->symbol (syntax->datum #'shape-str)))])

(define (id->keyword i)
  (define kw
    (string->keyword
     (symbol->string
      (syntax->datum i))))
  (datum->syntax i kw i))
