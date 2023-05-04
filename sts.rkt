#lang racket/base

(require boato/service/private/service)

(require aws/ext/credential
         aws/ext/credential/file
         net/http-easy)

(define-service-schema "sts.json")

(define cred (load-credential-from-file "credentials" "profile"))

(define (oof)
  (define rsp (GetCallerIdentity #f
                                 #:auth cred
                                 #:endpoint "https://sts.amazonaws.com"))
  (values (response-status-line rsp)
          (response-xml rsp)))
