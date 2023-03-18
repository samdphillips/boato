#lang typed/racket/shallow

(require typed/file/sha1
         typed/net/url-structs
         typed/net/uri-codec)

(provide make-request
         canonical-request
         make-credential-scope
         string-to-sign
         make-authorization)

(define-type Input-Headers (HashTable Symbol (U String Bytes)))
(define-type Headers (Immutable-HashTable Bytes Bytes))
(define-type Query (Listof (Pair Symbol (U #f String))))

(require/typed racket/dict
  [(dict-keys query-names) (-> Query (Listof Symbol))]
  [(dict-ref  query-ref)   (-> Query Symbol (U #f String))])

(require/typed crypto
  [hmac (-> Symbol Bytes Bytes Bytes)]
  [#:opaque Crypto-Factory crypto-factory?]
  [crypto-factories (Parameter (Listof Crypto-Factory))])

(require/typed crypto/libcrypto
  [libcrypto-factory Crypto-Factory])

(struct Request
  ([method    : Symbol]
   [url       : URL]
   [headers   : Headers]
   [body-hash : Bytes])
  #:transparent)

(struct Credential-Scope
  ([date    : Bytes]
   [region  : Bytes]
   [service : Bytes])
  #:transparent)

(: make-credential-scope : Bytes Bytes Bytes -> Credential-Scope)
(define (make-credential-scope dt region service)
  (Credential-Scope (dt->date dt)
                    region
                    service))

(: dt->date : Bytes -> Bytes)
(define (dt->date dt)
  (match dt
    [(pregexp #px"^(\\d{4}\\d{2}\\d{2})T" (list _ date))
     (assert date bytes?)]))

(: credential-scope-bytes : Credential-Scope -> Bytes)
(define (credential-scope-bytes scope)
  (match-define (Credential-Scope date region service) scope)
  (bytes-append date #"/" region #"/" service #"/aws4_request"))

(: validate-headers : Input-Headers Headers -> Void)
(define (validate-headers orig-h h)
  (define host (hash-has-key? h #"host"))
  (define date (hash-has-key? h #"x-amz-date"))
  (define msg
    (cond
      [(and (not host) (not date)) "'host and 'x-amz-date keys"]
      [(not host) "'host key"]
      [(not date) "'x-amz-date key"]
      [else #f]))
  (when msg
    (raise-argument-error 'make-request (format "hash containing ~a" msg) orig-h)))

(: make-request : Symbol URL Input-Headers Bytes -> Request)
(define (make-request method url headers body)
  (define nheaders (normalize-headers headers))
  (validate-headers headers nheaders)
  (Request method url nheaders (sha256-bytes-hex body)))

;; XXX would be nice to directly convert from bytes to hex-encode bytes
(: sha256-bytes-hex : Bytes -> Bytes)
(define (sha256-bytes-hex b)
  (string->bytes/latin-1 (bytes->hex-string (sha256-bytes b))))

(: normalize-headers : Input-Headers -> Headers)
(define (normalize-headers h)
  (for/hash : Headers ([(n v) (in-hash h)])
    (values (normalize-header-name n) (normalize-header-value v))))

(: normalize-header-name : Symbol -> Bytes)
(define (normalize-header-name n)
  (string->bytes/utf-8 (string-downcase (symbol->string n))))

;; XXX implement sequential space normalization
(: normalize-header-value : (U String Bytes) -> Bytes)
(define (normalize-header-value v)
  (cond
    [(string? v) (string->bytes/utf-8 v)]
    [else (regexp-replace* #px"(^\\s*|\\s*$)" v #"")]))

(: canonical-request : Request -> Bytes)
(define (canonical-request req)
  (bytes-append
   (canonical-method req)
   (canonical-uri-path req)
   (canonical-uri-query req)
   (canonical-headers req)
   ; signed-headers does not add the newline
   (signed-headers req) #"\n"
   (Request-body-hash req)))

(: canonical-method : Request -> Bytes)
(define (canonical-method req)
  (define mb
    (string->bytes/utf-8
     (symbol->string
      (Request-method req))))
  (bytes-append mb #"\n"))

(: canonical-uri-path : Request -> Bytes)
(define (canonical-uri-path req)
  (define p* : (Listof Bytes)
    (for/list ([p (in-list (url-path (Request-url req)))])
      (bytes-append #"/"
                    (uri-encode->bytes
                     (assert (path/param-path p) string?)))))
  (cond
    [(null? p*) #"/\n"]
    [else (bytes-append (bytes-append* p*) #"\n")]))

(: uri-encode->bytes : String -> Bytes)
(define (uri-encode->bytes s)
  (string->bytes/utf-8 (uri-encode s)))

(: canonical-uri-query : Request -> Bytes)
(define (canonical-uri-query req)
  (define q (url-query (Request-url req)))
  (define qn* (sort (query-names q) symbol<?))
  (bytes-append
    (bytes-join
     (for/list ([qn (in-list qn*)])
       (define v (query-ref q qn))
       (if v
           (bytes-append (uri-encode->bytes (symbol->string qn))
                         #"="
                         (uri-encode->bytes v))
           (uri-encode->bytes (string-append (symbol->string qn) "="))))
     #"&")
    #"\n"))

(: canonical-headers : Request -> Bytes)
(define (canonical-headers req)
  (define h (Request-headers req))
  (define hn* (sort (hash-keys h) bytes<?))
  (bytes-append
    (bytes-append*
     (for/list ([hn (in-list hn*)])
      (bytes-append hn #":" (hash-ref h hn) #"\n")))
    #"\n"))

(: signed-headers : Request -> Bytes)
(define (signed-headers req)
  (define h (Request-headers req))
  (define hn* (sort (hash-keys h) bytes<?))
  (bytes-join hn* #";"))

(: string-to-sign : Request Credential-Scope -> Bytes)
(define (string-to-sign req scope)
  (define dt (hash-ref (Request-headers req) #"x-amz-date"))
  (bytes-append #"AWS4-HMAC-SHA256\n"
                dt #"\n"
                (credential-scope-bytes scope) #"\n"
                (sha256-bytes-hex (canonical-request req))))

(: sign-string : Bytes Credential-Scope Bytes -> Bytes)
(define (sign-string key scope ss)
  (define h (λ ([k : Bytes] [i : Bytes])
              (parameterize ([crypto-factories (list libcrypto-factory)])
                (hmac 'sha256 k i))))
  (match-define (Credential-Scope date region service) scope)
  (define kDate    (h (bytes-append #"AWS4" key) date))
  (define kRegion  (h kDate region))
  (define kService (h kRegion service))
  (define kSigning (h kService #"aws4_request"))
  (string->bytes/latin-1
   (bytes->hex-string (h kSigning ss))))

(: make-authorization : Request Credential-Scope Bytes Bytes -> Bytes)
(define (make-authorization req scope access-key secret-key)
  (bytes-append #"AWS4-HMAC-SHA256 "
                #"Credential=" access-key #"/" (credential-scope-bytes scope) #", "
                #"SignedHeaders=" (signed-headers req) #", "
                #"Signature=" (sign-string secret-key scope (string-to-sign req scope))))
