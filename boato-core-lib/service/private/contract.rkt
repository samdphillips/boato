#lang racket/base

(require racket/contract)

(provide string/c)

(define (string/c #:min [n #f] #:max [m #f])
  (define name
    `(string/c ,@(if n `(#:min ,n) null) ,@(if m `(#:max ,m) null)))
  (flat-named-contract
   name
   (and/c string? (property/c string-length (integer-in n m)))))
