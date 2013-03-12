#lang typed/racket/base

(require typed/rackunit 
         typed/rackunit/text-ui
         "lru.rkt")
 

(let ()
  (: an-lru (Lru String Pair))
  (define an-lru (make-lru 5))
  (check-false (lru-has-key? an-lru "x"))
  (let ([c (cons 1 1)])
    
    ;; FIXME: I wanted an check-eq? check here instead, but something
    ;; about chaperones appears to be interfering.
    ;;
    ;; I need to check with the rest of the TR developers
    ;; to know what's truly going on here.  I suspect check-eq? is
    ;; interfering since it wasn't written in TR.
    #;(check-eq? (lru-ref an-lru "not there" (lambda () c))
                 c)
    ;; The test here, though, appears to work fine:
    (check-true (eq? (lru-ref an-lru "not there" (lambda () c))
                     c))))


(: small-lru (-> (Lru String String)))
(define (small-lru)
  (make-lru 2))

(let ()
  (define an-lru (small-lru))
  (lru-set! an-lru "greeting" "hello")
  (check-equal? (lru-ref an-lru "greeting" (lambda () #f))
                "hello")
  (lru-set! an-lru "salutation" "hola")
  (check-equal? (lru-ref an-lru "greeting" (lambda () #f))
                "hello")
  (check-equal? (lru-ref an-lru "salutation" (lambda () #f))
                "hola")
  (lru-set! an-lru "welcome" "안녕")
  ;; By this time, greeting should be gone.
  ;; FIXME: typed racket check-false here is failing, but not producing
  ;; good source location.  Why?
  (check-false (lru-has-key? an-lru "greeting")) 
  (check-true (lru-has-key? an-lru "salutation")) 
  (check-true (lru-has-key? an-lru "welcome")))