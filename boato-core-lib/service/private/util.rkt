#lang racket/base

(require net/url-string)

(provide (all-defined-out))

(define (->url u)
  (cond
    [(string? u) (string->url u)]
    [(url? u) u]
    [else
     (raise-argument-error '->url "(or/c string? url?)" u)]))
