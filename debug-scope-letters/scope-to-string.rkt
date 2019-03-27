#lang racket/base

(provide make-scope->string-function)

(module+ test
  (require rackunit)
  (define-syntax-rule (check f g [a b] ...)
    (test-case (format "~a" 'f)
      (check-equal? (f a) (g b)) ...)))

;; ----------------------------------------------------------------------------

;; Generating Strings for Scopes

;; A ScopeVec is a (vector Integer Any ...)

;; scope-vec->number : ScopeVec -> Integer
;; Finds the number associated with the given scope
(define (scope-vec->number scope-vec)
  (vector-ref scope-vec 0))

;; make-scope->string-function : String -> [ScopeVec -> String]
;; Produces a function that generates new strings in
;; lexicographic order for each new scope number
(define (make-scope->string-function alphabet)
  ;; scope-num->natural : Integer -> Natural
  (define scope-num->natural (make-scope-num->natural-function 1))
  ;; scope->string : ScopeVec -> String
  (define (scope->string scope-vec)
    (nth-string alphabet (scope-num->natural (scope-vec->number scope-vec))))
  scope->string)

;; make-scope-num->natural-function :
;; String -> [Integer -> Natural]
;; Produces a function that generates new naturals in
;; order for each new scope number
(module+ test
  (define alphabet "abcdefghijklmnopqrstuvwxyz")
  (define scope-num->natural (make-scope-num->natural-function 1))
  (define scope-num->natural-2 (make-scope-num->natural-function 1))
  (define (scope-num->symbol x)
    (string->symbol (nth-string alphabet (scope-num->natural x))))
  (define (scope-num->symbol-2 x)
    (string->symbol (nth-string alphabet (scope-num->natural-2 x))))

  (check scope-num->symbol quote
         [3 a]
         [1 b] [4 c] [1 b] [5 d] [9 e]
         [2 f] [6 g] [5 d] [3 a] [5 d]
         [8 h] [9 e] [7 i] [9 e] [32 j] [38 k] [46 l]
         [26 m] [43 n] [38 k] [32 j] [79 o]
         [50 p] [28 q] [84 r] [19 s] [71 t]
         [69 u] [39 v] [93 w] [75 x] [10 y]
         [58 z] [20 aa] [97 ab] [49 ac] [44 ad])

  (check scope-num->symbol-2 quote
         [6 a]
         [2 b] [8 c] [3 d] [1 e] [8 c]
         [5 f] [3 d] [0 g] [7 h] [1 e]
         [7 h] [9 i] [5 f] [8 c] [6 a]
         [4 j] [7 h] [6 a] [9 i] [2 b]
         [52 k] [86 l] [76 m] [65 n] [59 o]
         [00 g] [57 p] [68 q] [39 r] [43 s])

  (check scope-num->symbol quote
         [59 ae] [23 af] [07 i] [81 ag] [64 ah]
         [06 g] [28 q] [62 ai] [08 h] [99 aj]
         [86 ak] [28 q] [03 a] [48 al] [25 am]
         [34 an] [21 ao] [17 ap] [06 g] [79 o]
         [82 aq] [14 ar] [80 as] [86 ak] [51 at]
         [32 j] [82 aq] [30 au] [66 av] [47 aw]
         [09 e] [38 k] [44 ad] [60 ax] [95 ay]
         [50 p] [58 z] [22 az] [31 ba] [72 bb]
         [53 bc] [59 ae] [40 bd] [81 ag] [28 q]))

(define (make-scope-num->natural-function start)
  ;; generate-next-natural : -> Natural
  (define generate-next-natural (make-natural-generator start))
  (memoize/eq (λ (scope-num) (generate-next-natural))))

;; ----------------------------------------------------------------------------

;; Generating Natural Numbers

;; make-natural-generator : Natural -> [-> Natural]
;; Produces a function that generates new naturals
(module+ test
  (define generate-natural (make-natural-generator 1))
  (check-equal? (build-list 104 (λ (i) (generate-natural)))
                (build-list 104 add1)))

(define (make-natural-generator start)
  ;; num : Natural
  ;; A local mutable variable that tracks the next
  ;; number to use
  (define num start)
  ;; generate-next-natural : -> Natural
  (define (generate-next-natural)
    (begin0
      num
      (set! num (add1 num))))
  generate-next-natural)

;; ----------------------------------------------------------------------------

;; Memoization

;; memoize/eq : [A -> B] -> [A -> B]
;; Produces a memoized version of f
(define (memoize/eq f)
  ;; hsh : (Hashof A B)
  ;; A local mutable hash table mapping previous
  ;; arguments to their corresponding results
  (define hsh (make-hasheq))
  ;; memoized : [A -> B]
  ;; The memoized verison of f
  (define (memoized a)
    (hash-ref! hsh a (λ () (f a))))
  memoized)

;; ----------------------------------------------------------------------------

;; Lexicographic Order

;; nth-string : String Natural -> String
;; Produces the nth alphabetical string in lexicographic
;; order
(module+ test
  (define (nth-string/alphabet n) (nth-string alphabet n))
  (define (nth-symbol/alphabet n) (string->symbol (nth-string/alphabet n)))

  (check-equal? (build-list 105 nth-symbol/alphabet)
                '(||
                  a b c d e f g h
                  i j k l m n o p
                  q r s t u v w x
                  y z
                  aa ab ac ad ae af ag ah
                  ai aj ak al am an ao ap
                  aq ar as at au av aw ax
                  ay az
                  ba bb bc bd be bf bg bh
                  bi bj bk bl bm bn bo bp
                  bq br bs bt bu bv bw bx
                  by bz
                  ca cb cc cd ce cf cg ch
                  ci cj ck cl cm cn co cp
                  cq cr cs ct cu cv cw cx
                  cy cz))
  
  (check nth-symbol/alphabet quote
         [ 0 ||]
         [ 1 a]
         [ 2 b]
         [26 z]
         [(+ (*  1 26)  1) aa]
         [(+ (*  1 26)  2) ab]
         [(+ (*  1 26) 26) az]
         [(+ (*  2 26)  1) ba]
         [(+ (*  2 26)  2) bb]
         [(+ (*  2 26) 26) bz]
         [(+ (* 26 26)  1) za]
         [(+ (* 26 26)  2) zb]
         [(+ (* 26 26) 26) zz]
         [(+ (* (+ (*  1 26)  1) 26)  1) aaa]
         [(+ (* (+ (*  1 26)  1) 26)  2) aab]
         [(+ (* (+ (*  1 26)  2) 26)  1) aba]
         [(+ (* (+ (*  2 26)  1) 26)  1) baa]
         [(+ (* (+ (*  1 26)  2) 26) 26) abz]
         [(+ (* (+ (*  1 26) 26) 26)  2) azb]
         [(+ (* (+ (* 26 26)  1) 26)  2) zab])

  (check nth-string/alphabet quote
         [ 0 ""]
         [ 1 "a"]
         [ 2 "b"]
         [26 "z"]
         [(+ (*  1 26)  1) "aa"]
         [(+ (*  1 26)  2) "ab"]
         [(+ (*  1 26) 26) "az"]
         [(+ (*  2 26)  1) "ba"]
         [(+ (*  2 26)  2) "bb"]
         [(+ (*  2 26) 26) "bz"]
         [(+ (* 26 26)  1) "za"]
         [(+ (* 26 26)  2) "zb"]
         [(+ (* 26 26) 26) "zz"]
         [(+ (* (+ (*  1 26)  1) 26)  1) "aaa"]
         [(+ (* (+ (*  1 26)  1) 26)  2) "aab"]
         [(+ (* (+ (*  1 26)  2) 26)  1) "aba"]
         [(+ (* (+ (*  2 26)  1) 26)  1) "baa"]
         [(+ (* (+ (*  1 26)  2) 26) 26) "abz"]
         [(+ (* (+ (*  1 26) 26) 26)  2) "azb"]
         [(+ (* (+ (* 26 26)  1) 26)  2) "zab"]))

(define (nth-string alphabet n)
  (define b (string-length alphabet))
  (define len (nth-string-length b n))
  (define s (make-string len))
  (let loop ([i (sub1 len)] [n n])
    (cond [(negative? i) (void)]
          [else
           (string-set! s i (string-ref alphabet (remainder (sub1 n) b)))
           (loop (sub1 i) (quotient (sub1 n) b))]))
  s)

;; nth-string-length : PosInt Natural -> Natural
(module+ test
  (check-equal? (nth-string-length 10 0) 0)
  (check-equal? (nth-string-length 10 1) 1)
  (check-equal? (nth-string-length 10 2) 1)
  (check-equal? (nth-string-length 10 5) 1)
  (check-equal? (nth-string-length 10 10) 1)
  (check-equal? (nth-string-length 10 11) 2)
  (check-equal? (nth-string-length 10 100) 2)
  (check-equal? (nth-string-length 10 109) 2)
  (check-equal? (nth-string-length 10 110) 2)
  (check-equal? (nth-string-length 10 111) 3)
  (check-equal? (nth-string-length 10 1024) 3)
  (check-equal? (nth-string-length 10 1110) 3)
  (check-equal? (nth-string-length 10 1111) 4)
  (check-equal? (nth-string-length 10 11110) 4)
  (check-equal? (nth-string-length 10 11111) 5)
  (check-equal? (nth-string-length 10 111110) 5)
  (check-equal? (nth-string-length 10 111111) 6)
  (check-equal? (nth-string-length 9 0) 0)
  (check-equal? (nth-string-length 9 1) 1)
  (check-equal? (nth-string-length 9 9) 1)
  (check-equal? (nth-string-length 9 10) 2)
  (check-equal? (nth-string-length 9 90) 2)
  (check-equal? (nth-string-length 9 91) 3)
  (check-equal? (nth-string-length 9 819) 3)
  (check-equal? (nth-string-length 9 820) 4)
  (check-equal? (nth-string-length 9 7380) 4)
  (check-equal? (nth-string-length 9 7381) 5)
  )

(define (nth-string-length b n) (nth-string-length+ b n 0))

;; INVARIANT: (nth-string-length+ b n acc) = (+ (nth-string-length b n) acc)
(define (nth-string-length+ b n acc)
  (cond [(zero? n) acc]
        [else      (nth-string-length+ b (quotient (sub1 n) b) (add1 acc))]))
