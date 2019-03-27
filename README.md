# debug-scope-letters

Showing scopes with letters instead of multi-digit numbers.

```racket
(require debug-scope-letters)
```
Provides `+scopes`, which returns a string, and
`write+scopes` writes the syntax object to the current
output port.

Examples:
```racket
> (write+scopes #'(foo a 1 (add1 12)))
(foo⁽ᵃ˒ᵇ⁾ a⁽ᵃ˒ᵇ⁾ 1 (add1⁽ᵃ˒ᵇ⁾ 12))
> (write+scopes #`(equal? (+ x 1) #,((make-syntax-introducer) #'(* y 3))))
(equal?⁽ᵃ˒ᵇ⁾ (+⁽ᵃ˒ᵇ⁾ x⁽ᵃ˒ᵇ⁾ 1) (*⁽ᵃ˒ᵇ˒ᶜ⁾ y⁽ᵃ˒ᵇ˒ᶜ⁾ 3))
```
