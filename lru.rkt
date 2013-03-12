#lang typed/racket/base

;; Least recently used (LRU) cache
;;
;; Danny Yoo (dyoo@hashcollision.org)
;;
;; A fairly direct implementation of an LRU cache.  We keep a hashtable from
;; keys to elements.  The elements themselves are linked together as a
;; doubly-linked list, to enable the fast arbitrary insertion and deletion
;; required for an LRU.
;;
;; This library is not thread-safe; if you need it to be, wrap your own
;; synchronization over its operations.

(provide (rename-out [-make-lru make-lru]
                     [-make-lru-eq make-lru-eq]
                     [Lru? lru?]
                     [Lru-cap lru-cap])
         Lru
         lru-has-key?
         lru-count
         lru-keys
         lru-items
         lru-ref
         lru-set!
         lru-remove!)


;; Elements will be doubly-linked:
(struct: (K V) Element ([key : K]
                        [val : V] 
                        [prev : (U #f (Element K V))]
                        [next : (U #f (Element K V))])

  #:mutable 
  #:transparent)


;; The LRU will have a capacity, as well as the map from keys to elements.
;; It also holds references to the first and last element, to implement
;; the LRU policy.
(struct: (K V) Lru ([cap : Natural]
                    [ht : (HashTable K (Element K V))]
                    [first-elt : (U #f (Element K V))]
                    [last-elt : (U #f (Element K V))])
  #:mutable
  #:transparent)


(: -make-lru (All (K V) (Natural -> (Lru K V))))
;; Constructs a new LRU with an equal?-based map.
(define (-make-lru cap)
  (: ht (HashTable K (Element K V)))
  (define ht (make-hash))
  (Lru cap ht #f #f))


(: -make-lru-eq (All (K V) (Natural -> (Lru K V))))
;; Constructs a new LRU with an eq?-based map.
(define (-make-lru-eq cap)
  (: ht (HashTable K (Element K V)))
  (define ht (make-hasheq))
  (Lru cap ht #f #f))


(: lru-has-key? (All (K V) ((Lru K V) K -> Boolean)))
;; Returns true if the lru has an entry for the given key.
(define (lru-has-key? an-lru a-key)
  (hash-has-key? (Lru-ht an-lru) a-key))


(: lru-count (All (K V) ((Lru K V) -> Natural)))
;; Returns the number of elements in the LRU.
(define (lru-count a-lru)
  (hash-count (Lru-ht a-lru)))


(: lru-ref (All (K V F) ((Lru K V) K (-> F) -> (U V F))))
;; Looks up an element in the cache.  If we can find it, refresh it.
;; If we can't find it, return the result of the failure thunk
(define (lru-ref an-lru a-key a-fail-thunk)
  (define ht (Lru-ht an-lru))
  (cond [(hash-has-key? ht a-key)
         (define elt (hash-ref ht a-key))
         (move-to-front! an-lru elt)
         (Element-val elt)]
        [else
         (a-fail-thunk)]))



(: lru-set! (All (K V) ((Lru K V) K V -> Void)))
;; Insert an element into the LRU.  If it already exists, refreshes it.
;; Otherwise, create it.
(define (lru-set! an-lru a-key a-val)
  (define ht (Lru-ht an-lru))
  (cond [(hash-has-key? ht a-key)
         (define elt (hash-ref ht a-key))
         (set-Element-val! elt a-val)
         (move-to-front! an-lru elt)]
        [else
         ;; Linkage to the doubly-linked list and mapping.
         (define elt (insert-first! an-lru a-key a-val))
         (hash-set! ht a-key elt)
         
         ;; If over capacity, drop the last element.
         (define last-elt (Lru-last-elt an-lru))
         (when (and (Element? last-elt)
                    (> (hash-count ht) (Lru-cap an-lru)))
           (hash-remove! ht (Element-key last-elt))
           (unlink! an-lru last-elt))]))


(: lru-remove! (All (K V) ((Lru K V) K -> Void)))
;; Remove a binding from the LRU.
(define (lru-remove! an-lru a-key)
  (define ht (Lru-ht an-lru))
  (cond [(hash-has-key? ht a-key)
         (define elt (hash-ref ht a-key))
         (unlink! an-lru elt)
         (hash-remove! ht (Element-key elt))]
        [else
         (void)]))


(: lru-keys (All (K V) ((Lru K V) -> (Listof K))))
;; Collects a list of the keys in the lru, in order of most recently used.
(define (lru-keys a-lru)
  (let loop ([elt (Lru-first-elt a-lru)])
    (cond
      [(eq? elt #f)
       '()]
      [(Element? elt)
       (cons (Element-key elt)
             (loop (Element-next elt)))])))


(: lru-items (All (K V) ((Lru K V) -> (Listof (Pair K V)))))
;; Collects a list of the items in the lru, in order of most recently used.
(define (lru-items a-lru)
  (let loop ([elt (Lru-first-elt a-lru)])
    (cond
      [(eq? elt #f)
       '()]
      [(Element? elt)
       (cons (cons (Element-key elt)
                   (Element-val elt))
             (loop (Element-next elt)))])))
  


;; Internal: put the element an-elt at the front of the lru.
;; Assumes an-elt is already part of the linked-list structure.
(: move-to-front! (All (K V) ((Lru K V) (Element K V) -> Void)))
;; TODO: report bug in TR & DrRacket: renaming move-to-front! did not change
;; the type associated to the name.
(define (move-to-front! an-lru an-elt)
  (define prior-first (Lru-first-elt an-lru))
  (unless (eq? prior-first an-elt)
    (define prev (Element-prev an-elt))
    (define next (Element-next an-elt))
    
    ;; Splice the element out...
    (when (Element? prev)
      (set-Element-next! prev next))
    (when (Element? next)
      (set-Element-prev! next prev))
    (when (eq? an-elt (Lru-last-elt an-lru))
      (set-Lru-last-elt! an-lru prev))
    
    ;; and move the element to the front:
    (set-Element-prev! an-elt #f)
    (set-Element-next! an-elt prior-first)
    (when (Element? prior-first)
      (set-Element-prev! prior-first an-elt))
    (set-Lru-first-elt! an-lru an-elt)))



(: unlink! (All (K V) ((Lru K V) (Element K V) -> Void)))
;; Internal: unlink the element from the linked list.
(define (unlink! an-lru an-elt)
  (define prev (Element-prev an-elt))
  (define next (Element-next an-elt))

  (when (eq? an-elt (Lru-first-elt an-lru))
    (set-Lru-first-elt! an-lru next))
  (when (eq? an-elt (Lru-last-elt an-lru))
    (set-Lru-last-elt! an-lru prev))
  
  ;; Splice the element out...
  (when (Element? prev)
    (set-Element-next! prev next))
  (when (Element? next)
    (set-Element-prev! next prev)))


(: insert-first! (All (K V) ((Lru K V) K V -> (Element K V))))
;; Internal: insert an element value into the linked list.
(define (insert-first! an-lru a-key a-val)
  (define elt (Element a-key a-val #f (Lru-first-elt an-lru)))
  (define prior-first-elt (Lru-first-elt an-lru))
  (when (Element? prior-first-elt)
    (set-Element-prev! prior-first-elt elt))
  (set-Lru-first-elt! an-lru elt)
  (when (eq? #f (Lru-last-elt an-lru))
    (set-Lru-last-elt! an-lru elt))
  elt)
