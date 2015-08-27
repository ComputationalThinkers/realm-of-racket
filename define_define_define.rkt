#lang racket

; this define a variable (but caps "kinda" make it a constant)
(define SOME-CONSTANT "hello world")
(printf SOME-CONSTANT) ; then access here

; so is a variable
(require racket/date)
(define today (current-date))
(printf "\ntoday: ~a\n"  (date->string today))

; then a function
(define (function-name variable)
  (printf "this function print a variable: ~a\n" variable)
  )
; then you call it
(function-name "hello-world")

; you can have a local variable in a function (as usual)
(define (print-random-location)
  (define x (random 10))
  (define y (random 10))
  (struct posn (x y)) ; this is a struct (data type) defined inside a function
  (define a-random-position (posn x y))
  (printf "the generated random position: ~a,~a\n"
          (posn-x a-random-position)(posn-y a-random-position))
  )
(print-random-location)

; same as inside condition
(define (print-even-or-odd)
  (define a-number (random 10))
  (printf "for the number: ~a " a-number)

  (define (print-if-is-even-or-odd number)
    (cond
      [(= (remainder a-number 2) 0)
       (define result "is even\n") ; inside a cond here
       (printf result)]
      [else
       (define result "is odd\n") ; same here
       (printf result)]
      )
    ) ; define end

  (print-if-is-even-or-odd a-number) ; run the locally defined functiont
  )
(print-even-or-odd)