#lang racket/base

(require (for-syntax boato/service/private/service-support
                     boato/service/private/shape-support
                     racket/base
                     racket/match
                     racket/string)
         net/uri-codec
         racket/string
         syntax/parse/define)

(provide serialize/query-top)

(define-syntax-parser serialize/query-top
  [(_ svc:service [op-name:string http] shape v)
   #:with version (hash-ref (syntax->datum (attribute svc.tbl)) 'apiVersion)
   #:with [http-method-str http-path]
   (syntax-parse #'http
     [({~alt {~once {~seq #:method method:string}}
             {~once {~seq #:requestUri path:string}}} ...)
      #'(method path)])
   #:with http-method (string->symbol (syntax-e #'http-method-str))
   #'(let* ([ser '((Action . op-name) (Version . version))]
            [ser (serialize/query shape () ser v)])
       (values http-path
               'http-method
               #hasheq([content-type . "application/x-www-form-urlencoded; charset=utf-8"])
               (string->bytes/utf-8 (alist->form-urlencoded ser))))])

(define-for-syntax (type->string type)
  (match type
    ['string  #'#%expression]
    ['integer #'number->string]))

(define-syntax-parser serialize/query
  [(_ shp:simple-shape prefix ser v)
   #:with convert (type->string (attribute shp.type))
   #'(cons (cons (make-prefix . prefix) (convert v)) ser)]
  [(_ shp:list-shape prefix ser v)
   #'(let ([list-v v])
       (if (null? list-v)
         (cons (cons (make-prefix . prefix) "") ser)
         (serialize/query-list shp prefix ser list-v)))]
  [(_ shp:map-shape prefix ser v)
   #'(serialize/query-map shp prefix ser v)]
  [(_ shp:struct-shape prefix ser v)
   #'(let ([struct-v v])
       (serialize/query-struct-members (shp.members ...) prefix ser struct-v))])

(define-syntax-parser serialize/query-struct-members
  [(_ () prefix ser v) #'ser]
  [(_ ([name #f shp ref] . members) (prefix ...) ser v)
   #'(let ([cur-ser ser])
       (serialize/query-struct-members members
                                       (prefix ...)
                                       (let ([member-v (ref v)])
                                         (if (void? member-v)
                                              cur-ser
                                              (serialize/query shp
                                                               (prefix ... name)
                                                               cur-ser
                                                               member-v)))
                                       v))]
  [(_ ([name #t shp:shape ref] . members) (prefix ...) ser v)
   #'(serialize/query-struct-members members
                                     (prefix ...)
                                     (serialize/query shp (prefix ... name) ser (ref v))
                                     v)])

(define-syntax-parser serialize/query-list
  [(_ shp:list-shape (prefix ...) ser v)
   ;; XXX: handle flattened and/or serializations with named members.  Would
   ;; need to add information to list-shape-info.
   #'(for/fold ([nser ser])
               ([n (in-naturals)]
                [elem (in-list v)])
       (define i (number->string n))
       (serialize/query shp.member-shape (prefix ... "member" i) nser elem))])

(define-syntax-parser serialize/query-map
  [(_ shp:map-shape (prefix ...) ser v)
   ;; XXX: handle flattened and/or serializations with named members.  Would
   ;; need to add information to map-shape-info.
   #'(for/fold ([nser ser])
               ([n (in-naturals)]
                [(keyv valv) (in-hash v)])
       (define i (number->string n))
       (define kser
         (serialize/query shp.key-shape (prefix ... "entry" i "key") nser keyv))
       (serialize/query shp.value-shape (prefix ... "entry" i "value") kser valv))])

(define-syntax-parser make-prefix
  [(_ parts ...) #'(make-prefix-aux () (parts ...))])

(define-syntax-parser make-prefix-aux
  [(_ (done:string) ())
   #`'#,(string->symbol (syntax-e #'done))]
  [(_ (done) ()) #'(string->symbol done)]
  [(_ (done ...) ()) #'(make-prefix-aux ((string-join (list done ...) ".")) ())]
  [(_ done (a:string b:string . rest))
   #:with new
   #`#,(string-append (syntax-e #'a) "." (syntax-e #'b))
   #'(make-prefix-aux done (new . rest))]
  [(_ (done ...) (a . rest)) #'(make-prefix-aux (done ... a) rest)])
