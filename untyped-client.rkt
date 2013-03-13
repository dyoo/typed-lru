#lang racket/base

;; Using the LRU in untyped code is a little awkward.  We can
;; instantiate the module for specific types:
(module string-lru typed/racket/base
  (require (prefix-in poly: "lru.rkt"))
  (provide make-lru lru-ref lru-set! lru-has-key?)
  
  (: make-lru (Natural -> (poly:Lru String String)))
  (define make-lru poly:make-lru)
  
  (: lru-ref ((poly:Lru String String) String -> (U String #f)))
  (define (lru-ref a-lru key)
    (poly:lru-ref a-lru key (lambda () #f)))
  
  (: lru-set! ((poly:Lru String String) String String -> Void))
  (define lru-set! poly:lru-set!)
  
  (: lru-has-key? ((poly:Lru String String) String -> Boolean))
  (define lru-has-key? poly:lru-has-key?))


;; after which we can use the library.
(require (submod "." string-lru))
(define l (make-lru 2))
(lru-set! l "greeting" "hello")
(lru-set! l "program" "lru test")
(lru-set! l "language" "racket")
(lru-ref l "greeting")
(lru-ref l "program")
(lru-ref l "language")


