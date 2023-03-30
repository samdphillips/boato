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
         racket/string
         syntax/parse/define)


(begin-for-syntax
  (define (type->string type)
    (match type
      ['string  #'values]
      ['integer #'number->string])))

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
   #'(let ([nser (let ([mv (ref v)])
                   (if (void? mv)
                       ser
                       (serialize/query shp (prefix ... name) ser mv)))])
       (serialize/query-struct-members (members ...)
                                       (prefix ...)
                                       nser
                                       v))])
#;
(define-syntax-parser make-prefix
  [(_ vs:string ...)
   #`'#,(string->symbol (string-join (syntax->datum #'(vs ...)) "."))])

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

#;#;#;
(define i "0")
(make-prefix "a" "b" i "c")
(make-prefix "a" "b" "c")

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

#|
(define-values (struct:Tag mk-Tag Tag? Tag-acc Tag-mut)
  (make-struct-type 'Tag #f 2 0))
(define (make-Tag #:Key key #:Value value) (mk-Tag key value))
(define-values (Tag-Key Tag-Value)
  (values (make-struct-field-accessor Tag-acc 0 'Key)
          (make-struct-field-accessor Tag-acc 1 'Value)))
(define-syntax Tag
  (struct-shape-info #'Tag?
                     (list
                      (struct-shape-member-info 'Key #t #'tagKeyType #'Tag-Key)
                      (struct-shape-member-info 'Value #t #'tagValueType #'Tag-Value))))
|#


(define tag (make-Tag #:Key "mykey" #:Value "myvalue"))

(let ([ser null])
  (serialize/query Tag ("tag") ser tag))

#;#;
(define tag* (list tag))
(let ([ser null])
  (serialize/query tagListType (tags) ser tag*))

