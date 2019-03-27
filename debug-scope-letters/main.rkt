#lang racket/base

(provide write+scopes +scopes)

(require racket/pretty
         "s-expr-with-scopes.rkt")

;; Inspired by https://github.com/jsmaniac/debug-scopes

(define (write+scopes stx [out (current-output-port)])
  (pretty-write (syntax->s-expr+scopes stx) out))

(define (+scopes stx)
  (pretty-format (syntax->s-expr+scopes stx)))

