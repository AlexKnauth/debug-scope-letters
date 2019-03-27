#lang racket/base

(provide make-scope->superscript-string-function
         scopes->superscript-string
         current-scope->superscript-string)

(require racket/string
         "scope-to-string.rkt")
(module+ test
  (require rackunit))

(define superscript-alphabet
  "ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻ⁰¹²³⁴⁵⁶⁷⁸⁹")

;; make-scope->superscript-string-function : -> [ScopeVec -> String]
(define (make-scope->superscript-string-function)
  (make-scope->string-function superscript-alphabet))

;; current-scope->superscript-string : [Parameterof [ScopeVec -> String]]
(define current-scope->superscript-string
  (make-parameter (make-scope->superscript-string-function)))

;; scopes->superscript-string :
;;   [Listof ScopeVec] -> String
;; ∩ [Listof ScopeVec] #:scope->string [ScopeVec -> String] -> String
(define (scopes->superscript-string
         scopes
         #:scope->string [scope->string (current-scope->superscript-string)])
  (format "⁽~a⁾" (string-join (map scope->string scopes) "˒")))

(module+ test
  (define scope->string (make-scope->superscript-string-function))
  (define (show s) (scopes->superscript-string s #:scope->string scope->string))
  (check-equal? (show '(#(123))) "⁽ᵃ⁾")
  (check-equal? (show '(#(456))) "⁽ᵇ⁾")
  (check-equal? (show '(#(123) #(456))) "⁽ᵃ˒ᵇ⁾")
  (check-equal? (show '(#(456) #(123))) "⁽ᵇ˒ᵃ⁾")
  (check-equal? (show '(#(123) #(789))) "⁽ᵃ˒ᶜ⁾")
  (check-equal? (show '(#(1122) #(789))) "⁽ᵈ˒ᶜ⁾")
  (check-equal? (show '(#(789) #(123) #(1122) #(456))) "⁽ᶜ˒ᵃ˒ᵈ˒ᵇ⁾")
  )
