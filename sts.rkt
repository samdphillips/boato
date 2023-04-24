#lang racket/base

(require boato/service/private/service
         boato/service/private/ser)
(define-service-schema "sts.json")

(require (for-syntax racket/base
                     boato/service/private/service-support))

(define-syntax AssumeRole
  (static-operation-metadata (hash 'name "AssumeRole")))

(define (doit v)
  (serialize sts-service AssumeRole AssumeRoleRequest v))

#|
(define-service-simple-shape tagKeyType "string")
(define-service-simple-shape tagValueType "string")

(define-service-structure-shape Tag
  #:members ([Key tagKeyType] [Value tagValueType])
  #:required (Key Value))

(define-service-list-shape tagListType
  #:member [(shape . "Tag")])

(define-service-map-shape tagMapType
  #:key ([shape . "tagKeyType"])
  #:value ([shape . "tagValueType"]))

(define tag (make-Tag #:Key "mykey" #:Value "myvalue"))

(let ([ser null])
  (serialize/query Tag ("tag") ser tag))

(define tag*
  (list tag
        (make-Tag #:Key "name" #:Value "sam")
        (make-Tag #:Key "lang" #:Value "Racket")))

(let ([ser null])
  (serialize/query tagListType ("tags") ser tag*))

(define tag**
  (hash "mykey" "myvalue"
        "name" "sam"
        "lang" "Racket"))

(let ([ser null])
  (serialize/query tagMapType ("tags") ser tag**))
|#
