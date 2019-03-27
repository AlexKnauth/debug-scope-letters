#lang racket/base

(provide syntax->s-expr+scopes)

(require racket/match
         racket/vector
         "scopes-to-superscript-string.rkt")

;; A DebugInfo is a:
;;   (hash 'name  Symbol
;;         'context [Listof ScopeVec]
;;         'bindings [Listof DebugInfoBinding])

;; A DebugInfoBinding is one of:
;;  - (hash 'name Symbol
;;          'module [Maybe ModulePathIndex]
;;          'context [Listof ScopeVec])
;;  - (hash 'match? Boolean
;;          'module [Maybe ModulePathIndex]
;;          'context [Listof ScopeVec])

;; debug-info-scopes : DebugInfo -> [Listof ScopeVec]
(define (debug-info-scopes info) (hash-ref info 'context))

;; The scope->string field should be filled with a function
;; that is the result of calling make-scope->superscript-string-function
(struct symbol+scopes [symbol scopes]
  #:methods gen:custom-write
  [(define (write-proc self out mode)
     (match-define (symbol+scopes symbol scopes) self)
     (match mode
       [0 (fprintf out "~v~a" symbol (scopes->superscript-string scopes))]
       [1 (fprintf out "~s~a" symbol (scopes->superscript-string scopes))]
       [#t (fprintf out "~s~a" symbol (scopes->superscript-string scopes))]
       [#f (fprintf out "~a" symbol)]))])

(define (identifier->symbol+scopes id)
  (symbol+scopes (syntax-e id) (debug-info-scopes (syntax-debug-info id))))

(define (syntax->s-expr+scopes stx)
  (define e (stx-e stx))
  (cond
    [(identifier? stx) (identifier->symbol+scopes stx)]
    [(symbol? e) e]
    [(boolean? e) e]
    [(null? e) e]
    [(number? e) e]
    [(keyword? e) e]
    [(or (string? e) (bytes? e)) e]
    [(or (regexp? e) (byte-regexp? e)) e]
    [(void? e) e]
    [(list? e) (map syntax->s-expr+scopes e)]
    [(pair? e)
     (cons (syntax->s-expr+scopes (car e)) (syntax->s-expr+scopes (cdr e)))]
    [(mpair? e)
     (mcons (syntax->s-expr+scopes (mcar e)) (syntax->s-expr+scopes (mcdr e)))]
    [(list? e) (map syntax->s-expr+scopes e)]
    [(vector? e) (fmap-vector syntax->s-expr+scopes e)]
    [(box? e) (fmap-box syntax->s-expr+scopes e)]
    [(hash? e) (fmap-hash syntax->s-expr+scopes e)]
    [(prefab-struct-key e)
     => (λ (key)
          (apply make-prefab-struct key
                 (map syntax->s-expr+scopes
                      (cdr (vector->list (struct->vector e))))))]
    [else e]))

(define (stx-e stx)
  (if (syntax? stx) (syntax-e stx) stx))

(define (fmap-vector f v)
  (define r (vector-map syntax->s-expr+scopes v))
  (cond [(immutable? v) (vector->immutable-vector r)]
        [else           r]))

(define (fmap-box f b)
  (define v (syntax->s-expr+scopes (unbox b)))
  (cond [(immutable? b) (box-immutable v)]
        [else           (box v)]))

(define (fmap-hash f h)
  (cond
    [(immutable? h)
     (cond [(hash-equal? h) (for/hash ([(k v) (in-hash h)]) (values k (f v)))]
           [(hash-eqv? h) (for/hasheqv ([(k v) (in-hash h)]) (values k (f v)))]
           [(hash-eq? h) (for/hasheq ([(k v) (in-hash h)]) (values k (f v)))]
           [else (error 'fmap-hash "expected equal, eqv, or eq")])]
    [else
     (define r
       (cond [(hash-equal? h) (make-hash)]
             [(hash-eqv? h) (make-hasheqv)]
             [(hash-eq? h) (make-hasheq)]
             [else (error 'fmap-hash "expected equal, eqv, or eq")]))
     (for ([(k v) (in-hash h)]) (hash-set! r k (f v)))
     r]))

