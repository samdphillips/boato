#lang racket/base

(require (for-syntax racket/base
                     racket/sequence)
         syntax/parse/define)

(provide ht-expand)

(begin-for-syntax
  (provide hash-table
           id->keyword)
  (define-syntax-class hash-table
    [pattern v
      #:do [(define ht (syntax->datum #'v))]
      #:when (hash? ht)
      #:attr items (datum->syntax #'v (hash->list ht) #'v)])

  (define (id->keyword i)
    (define kw
      (string->keyword
       (symbol->string
        (syntax->datum i))))
    (datum->syntax i kw i)))

(define-syntax-parser ht-expand
  [(_ k:id (kargs ...) #:keywords? kw?:boolean ht:hash-table)
   #:with ([keys . vals] ...) #'ht.items
   #:with (kws ...) (for/list ([k (in-syntax #'(keys ...))]) (id->keyword k))
   (if (syntax->datum #'kw?)
       #'(k kargs ... (~@ kws vals) ...)
       #'(k kargs ... [keys vals] ...))])
