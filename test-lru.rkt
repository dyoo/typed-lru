#lang typed/racket/base

(require typed/rackunit 
         typed/rackunit/text-ui
         racket/list
         "lru.rkt")
 

(let ()
  (: an-lru (Lru String Pair))
  (define an-lru (make-lru 5))
  (check-equal? (lru-count an-lru) 0)
  (check-equal? (lru-cap an-lru) 5)
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



(let ()
  (: small-lru (-> (Lru String String)))
  (define (small-lru)
    (make-lru 2))
    
  
  (define an-lru (small-lru))
  (check-equal? (lru-count an-lru) 0)
  (check-equal? (lru-cap an-lru) 2)
   
  (lru-set! an-lru "greeting" "hello")
  (check-equal? (lru-count an-lru) 1)
  (check-equal? (lru-ref an-lru "greeting" (lambda () #f))
                "hello")

  (lru-set! an-lru "salutation" "hola")
  (check-equal? (lru-count an-lru) 2)
  (check-equal? (lru-ref an-lru "greeting" (lambda () #f))
                "hello")
  (check-equal? (lru-ref an-lru "salutation" (lambda () #f))
                "hola")
  
  (lru-set! an-lru "welcome" "안녕")
  (check-equal? (lru-count an-lru) 2)
  ;; By this time, greeting should be gone.
  ;; FIXME: typed racket check-false here is failing, but not producing
  ;; good source location.  Why?
  (check-false (lru-has-key? an-lru "greeting")) 
  (check-true (lru-has-key? an-lru "salutation")) 
  (check-true (lru-has-key? an-lru "welcome")))




;; Let's do a fuzzer.  We want a simple-stupid brute implementation
;; to test against the efficient implementation.  We'll do it
;; in terms of a-lists.
(struct: (K V) brute-lru ([cap : Natural]
                          [elts : (Listof (Pair K V))]) 
  #:mutable #:transparent)
(: brute-ref (All (K V X) ((brute-lru K V) K (-> X) -> (U V X))))
(define (brute-ref a-lru k thunk)
  (define lst (brute-lru-elts a-lru))
  (cond [(assoc k lst)
         => 
         (lambda (pair)
           (set-brute-lru-elts! a-lru (cons pair (remq pair lst)))
           (cdr pair))]
        [else
         (thunk)]))

(: brute-set! (All (K V X) ((brute-lru K V) K V -> Void)))
(define (brute-set! a-lru k v)
  (define lst (brute-lru-elts a-lru))
  (cond [(assoc k lst)
         =>
         (lambda (pair)
           (set-brute-lru-elts! a-lru (cons (cons k v) (remq pair lst))))]
        [else
         (define new-lst (cons (cons k v) lst))
         (set-brute-lru-elts! a-lru 
                              (drop-right new-lst 
                                          (max 0 (- (length new-lst)
                                                    (brute-lru-cap a-lru)))))]))

(: brute-remove! (All (K V X) ((brute-lru K V) K -> Void)))
(define (brute-remove! a-lru k)
  (define lst (brute-lru-elts a-lru))
  (cond [(assoc k lst)
         =>
         (lambda (pair)
           (set-brute-lru-elts! a-lru (remq pair lst)))]
        [else
         (void)]))


(: brute-keys (All (K V) ((brute-lru K V) -> (Listof K))))           
(define (brute-keys a-lru)
  (define lst (brute-lru-elts a-lru))
  (map (lambda: ([p : (Pair K V)])
         (car p))
       lst))

(: brute-items (All (K V) ((brute-lru K V) -> (Listof (Pair K V)))))
(define (brute-items a-lru)
  (brute-lru-elts a-lru))

(: brute-has-key? (All (K V) ((brute-lru K V) K -> Boolean)))
(define (brute-has-key? a-lru k)
  (and (assoc k (brute-lru-elts a-lru))
       #t))

(: brute-count (All (K V) ((brute-lru K V) -> Natural)))
(define (brute-count a-lru)
  (length (brute-lru-elts a-lru)))


(: fuzz-keys (Listof String))
(define fuzz-keys (build-list 10 (lambda (i) (format "key~a" i))))

(: fuzz-vals (Listof String))
(define fuzz-vals (build-list 10 (lambda (i) (format "val~a" i))))

(: fuzz (Natural -> Void))
;; Take our lru and a brute lru, do a random operation, and check
;; that they have the same content.  Iterate.
(define (fuzz cap)
  (: a-lru (Lru String String))
  (define a-lru (make-lru cap))
  
  (: b-lru (brute-lru String String))
  (define b-lru (brute-lru cap '()))
  
  (define (same?)
    (and (equal? (lru-keys a-lru)
                 (brute-keys b-lru))
         (equal? (lru-items a-lru)
                 (brute-items b-lru))
         (equal? (lru-count a-lru)
                 (brute-count b-lru))))
  
  (for ([i 100])
    (check-true (same?))
    (case (random-choice '(set get rem has-key))
      [(set)
       (define k (random-choice fuzz-keys))
       (define v (random-choice fuzz-vals))
       (lru-set! a-lru k v)
       (brute-set! b-lru k v)]
      [(get)
       (define k (random-choice fuzz-keys))
       (check-equal? (lru-ref a-lru k (lambda () #f))
                     (brute-ref b-lru k (lambda () #f)))]
      [(rem)
       (define k (random-choice fuzz-keys))
       (lru-remove! a-lru k)
       (brute-remove! b-lru k)]
      [(has-key)
       (define k (random-choice fuzz-keys))
       (check-equal? (lru-has-key? a-lru k)
                     (brute-has-key? b-lru k))]
      [else
       (error 'huh?)])))


(: random-choice (All (X) ((Listof X) -> X)))
;; Choose a random item.
(define (random-choice elts)
  (list-ref elts (random (length elts))))


(printf "Running fuzzer.  This will take a few seconds.\n")
(for ([repeat 1000])
  (for ([cap 5])
    (fuzz cap)))