#lang racket/base

#;#;
(require boato/service/private/service)
(define-service-schema "sts.json")

(require (for-syntax racket/base
                     racket/format
                     racket/match
                     racket/string
                     boato/service/private/service-support
                     boato/service/private/shape-support)
         boato/service/private/shape
         racket/format
         racket/string
         syntax/parse/define)


(define-for-syntax (type->string type)
  (match type
    ['string  #'values]
    ['integer #'number->string]))

(define-syntax-parser serialize
  [(_ svc:service op shp v)
   #:with protocol (hash-ref (syntax->datum (attribute svc.tbl)) 'protocol)
   #'(serialize protocol svc op shp v)]
  [(_ "query" svc op shp v)
   #'(serialize/query-top svc op shp v)])

(define-syntax-parser serialize/query-top
  [(_ svc:service op:operation shape v)
   #:with version (hash-ref (syntax->datum (attribute svc.tbl)) 'apiVersion)
   #:with action (hash-ref (syntax->datum (attribute op.tbl)) 'name)
   #'(let ([ser '((Action . action) (Version . version))])
       (serialize/query shape v ser))])

(define-syntax-parser serialize/query
  [(_ shp:simple-shape prefix ser v)
   #:with convert (type->string (attribute shp.type))
   #'(cons (cons (make-prefix . prefix) (convert v)) ser)]
  [(_ shp:list-shape prefix ser v)
   #'(if (null? v)
         (cons (cons (make-prefix . prefix) "") ser)
         (serialize/query-list shp prefix ser v))]
  [(_ shp:struct-shape (prefix ...) ser v)
   #:with (members ...)
   (for/list ([m (in-list (attribute shp.members))])
     (list #`#,(symbol->string (struct-shape-member-info-name m))
           (struct-shape-member-info-shape m)
           (struct-shape-member-info-accessor m)))
   #'(serialize/query-struct-members (members ...) (prefix ...) ser v)])

(define-syntax-parser serialize/query-struct-members
  [(_ () prefix ser v) #'ser]
  [(_ ([name shp:shape ref] members ...) (prefix ...) ser v)
   ;; XXX: if a member is required this can omit the `void?` check
   #'(let ([nser (let ([mv (ref v)])
                   (if (void? mv)
                       ser
                       (serialize/query shp (prefix ... name) ser mv)))])
       (serialize/query-struct-members (members ...)
                                       (prefix ...)
                                       nser
                                       v))])

(define-syntax-parser serialize/query-list
  [(_ shp:list-shape (prefix ...) ser v)
   ;; XXX: handle flattened and/or serializations with named members.  Would
   ;; need to add information to list-shape-info.
   #'(for/fold ([nser ser])
               ([n (in-naturals)]
                [elem (in-list v)])
       (define i (~a n))
       (serialize/query shp.member-shape (prefix ... "member" i) nser elem))])

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

#|
(define-syntax AssumeRole
  (static-operation-metadata (hash 'name "AssumeRole")))

(serialize sts-service AssumeRole AssumeRoleRequest v)
|#

(define-service-simple-shape tagKeyType "string")
(define-service-simple-shape tagValueType "string")

(define-service-structure-shape Tag
  #:members ([Key tagKeyType] [Value tagValueType])
  #:required (Key Value))

(define-service-list-shape tagListType
  #:member [(shape . "Tag")])

(define tag (make-Tag #:Key "mykey" #:Value "myvalue"))

(let ([ser null])
  (serialize/query Tag ("tag") ser tag))

(define tag*
  (list tag
        (make-Tag #:Key "name" #:Value "sam")
        (make-Tag #:Key "lang" #:Value "Racket")))

(let ([ser null])
  (serialize/query tagListType ("tags") ser tag*))
