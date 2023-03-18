#lang racket/base

(require (for-syntax racket/base
                     racket/exn
                     racket/format
                     racket/match
                     racket/port
                     racket/file
                     racket/runtime-path
                     syntax/parse)
         boato/sigv4/private/sigv4
         net/url-string
         rackunit)

(begin-for-syntax
  (define-runtime-path testsuite-dir "/home/sam/projects/botocore/tests/unit/auth/aws4_testsuite")

  (define (get-test-filenames test-name)
    (define (mktest ext)
      (build-path testsuite-dir test-name (~a test-name ext)))
    (values (mktest ".req")
            (mktest ".creq")
            (mktest ".sts")
            (mktest ".authz")))

  (define (req-file->req-syntax stx filename)
    (define (mkstx v) (datum->syntax stx v stx))
    (define (read-request-line inp)
      (match (read-line inp)
        [(pregexp #px"^(\\S+)\\s+(\\S+).*"
                  (list _ method-s path-s))
         (values (mkstx (list 'quote (string->symbol method-s)))
                 (mkstx (~a "https://example.amazonaws.com" path-s)))]))
    (define (read-header-lines inp)
      (define hlist
        (for/list ([line (in-lines inp)]
                   #:break (zero? (string-length line)))
          (match line
            [(pregexp #px"^([^:]+):\\s*(.*)"
                      (list _ name value))
             (list (mkstx (list 'quote (string->symbol name)))
                   (mkstx value))])))
      #`(hash #,@(mkstx (apply append hlist))))
    (with-handlers ([exn:fail? (λ (e) (raise-syntax-error
                                       'req-file->req-syntax
                                       (exn->string e)
                                       stx))])
      (call-with-input-file filename
        (λ (inp)
          (define-values (mstx ustx) (read-request-line inp))
          (define hstx (read-header-lines inp))
          (define bstx (mkstx (port->bytes inp)))
          #`(make-request #,mstx (string->url #,ustx) #,hstx #,bstx))))))

(define testsuite-date    #"20150830T123600")
(define testsuite-region  #"us-east-1")
(define testsuite-service #"service")
(define testsuite-access-key #"AKIDEXAMPLE")
(define testsuite-secret-key #"wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY")

(define-syntax define-auth-test
  (syntax-parser
    [(_ name:str)
     #:do [(define-values (req-file creq-file sts-file authz-file)
             (get-test-filenames (syntax->datum #'name)))
           (define (mkstx v) (datum->syntax this-syntax v this-syntax))]
     #:with req-stx   (req-file->req-syntax #'name req-file)
     #:with creq-stx  (mkstx (file->bytes creq-file))
     #:with sts-stx   (mkstx (file->bytes sts-file))
     #:with authz-stx (mkstx (file->bytes authz-file))
     #'(test-case name
         (define the-request req-stx)
         (define the-scope (make-credential-scope testsuite-date
                                                  testsuite-region
                                                  testsuite-service))
         (with-check-info (['request the-request]
                           ['credential-scope the-scope])
           (check-equal? (canonical-request the-request) creq-stx "canonical request")
           (check-equal? (string-to-sign the-request the-scope) sts-stx "string to sign")
           (check-equal? (make-authorization the-request
                                             the-scope
                                             testsuite-access-key
                                             testsuite-secret-key)
                         authz-stx
                         "authorization")))]))

(module* test #f
  #;(define-auth-test "get-header-key-duplicate")
  #;(define-auth-test "get-header-value-multiline")
  #;(define-auth-test "get-header-value-order")
  ;;(define-auth-test "get-header-value-trim")
  (define-auth-test "get-unreserved")
  (define-auth-test "get-utf8")
  (define-auth-test "get-vanilla")
  (define-auth-test "get-vanilla-empty-query-key")
  (define-auth-test "get-vanilla-query")
  (define-auth-test "get-vanilla-query-order-encoded")
  (define-auth-test "get-vanilla-query-order-key")
  (define-auth-test "get-vanilla-query-order-key-case")
  (define-auth-test "get-vanilla-query-order-value")
  (define-auth-test "get-vanilla-query-unreserved")
  (define-auth-test "get-vanilla-utf8-query")
  #;(define-auth-test "get-vanilla-with-session-token")
  #;(define-auth-test "normalize-path")
  (define-auth-test "post-header-key-case")
  (define-auth-test "post-header-key-sort")
  (define-auth-test "post-header-value-case")
  #;(define-auth-test "post-sts-token")
  (define-auth-test "post-vanilla")
  (define-auth-test "post-vanilla-empty-query-value")
  (define-auth-test "post-vanilla-query")
  (define-auth-test "post-x-www-form-urlencoded")
  (define-auth-test "post-x-www-form-urlencoded-parameters"))
