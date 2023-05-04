#lang racket/base

(require boato/credential
         boato/sigv4/private/sigv4
         file/sha1
         gregor
         net/http-easy
         net/url-structs
         racket/contract
         racket/format
         racket/match
         threading)

(provide
 (contract-out [boato-request (-> session? aws-credential? string? string? url? symbol? hash? bytes? response?)]))

(define-logger boato-http)

(define (sha256-bytes-hex b)
  (string->bytes/latin-1 (bytes->hex-string (sha256-bytes b))))

(define (->bytes b)
  (cond
    [(bytes? b) b]
    [(string? b) (string->bytes/utf-8 b)]
    [else (error '->bytes "no converter for value: ~s" b)]))

(define (boato-request http-session cred service region op-url op-method op-hdrs op-body)
  (define body-hash (sha256-bytes-hex op-body))
  (define host
    (string-append (url-host op-url)
                   (match (url-port op-url)
                     [(or #f 80) ""]
                     [port (~a ":" port)])))
  (define date
    (->bytes
     (let ([m (now/moment/utc)])
       (define (d2 v) (~a #:width 2 #:align 'right #:pad-string "0" v))
       (~a (->year m)
           (d2 (->month m))
           (d2 (->day   m))
           "T"
           (d2 (->hours m))
           (d2 (->minutes m))
           (d2 (->seconds m))
           "Z"))))

  (define pre-auth-headers
    (let ([hdrs (~> op-hdrs
                    (hash-set 'Host host)
                    (hash-set 'x-amz-date date)
                    (hash-set 'x-amz-content-sha256 body-hash))])
      (if (aws-credential-security-token cred)
          (hash-set hdrs 'x-amz-security-token (aws-credential-security-token cred))
          hdrs)))

  (define auth
    (let ([req (make-request op-method op-url pre-auth-headers op-body)]
          [scope (make-credential-scope date (->bytes region) (->bytes service))])
      (make-authorization req
                          scope
                          (->bytes (aws-credential-access-key cred))
                          (->bytes (aws-credential-secret-access-key cred)))))

  (define headers+auth
    (hash-set pre-auth-headers 'authorization auth))

  (when (log-level? boato-http-logger 'debug)
    (for ([(k v) (in-hash headers+auth)])
      (log-message boato-http-logger
                   'debug
                   (format "headers+auth: ~a :: ~s" k v)
                   (current-continuation-marks))))

  (session-request http-session
                   op-url
                   #:method op-method
                   #:headers headers+auth
                   #:data op-body))
