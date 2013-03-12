#lang racket
;; This file does not yet compile.  Asking on the list why.

(require "lru.rkt")

(define l (make-lru 5))
(lru-set! l "greeting" "hello")

;; The following call here fails to typecheck?
(lru-ref l "greeting")