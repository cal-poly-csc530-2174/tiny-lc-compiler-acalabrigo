#lang typed/racket
(require typed/rackunit)

; Adam Calabrigo
; CPE 530 Assignment 1

; parser for this language, converts LC to JS
(define (translate [s : Sexp]) : Sexp
  (cond
    [(real? s) s]
    [(symbol? s) s]
    [(list? s)
     (define p (cast (ann s Any) (Listof Sexp)))
     (match s
       [(list '+ l r) (list (translate l) '+ (translate r))]
       [(list '* l r) (list (translate l) '* (translate r))]
       [(list 'ifleq0 c l r) (list (translate c) '<= 0 '? (translate l) ': (translate r))]
       [(list 'println s) (list 'function\(\)\{ 'console.log\( (translate s) '\) '\; 'return 0 '\; '\})]
       [(list 'λ (? list? x) expr) (list 'function\( (translate (first x)) '\)\{ 'return (translate expr) '\})]
       [(list fun arg) (list (translate fun) '\( (translate arg) '\))])]
    [else (error 'LC "invalid input")]))

; translate test cases
(check-equal? (translate '{+ 1 2}) '(1 + 2))
(check-equal? (translate '{+ x 2}) '(x + 2))
(check-equal? (translate '{* 1 2}) '(1 * 2))
(check-equal? (translate '{ifleq0 2 {+ 1 2} {* 3 4}})
              '(2 <= 0 ? (1 + 2) : (3 * 4)))
(check-equal? (translate '{println 5}) '(function\(\)\{ console.log\( 5 \) \; return 0 \; \}))
(check-equal? (translate '{{λ {x} {+ x 5}} 7}) '((function\( x \)\{ return (x + 5) \}) \( 7 \)))
(check-exn (regexp (regexp-quote "LC: invalid input")) (lambda () (translate "hello")))