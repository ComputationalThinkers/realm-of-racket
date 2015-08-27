#lang racket

(define (factorial x)
  (cond
    [(= x 0) 1]
    [(= x 1) 1]
    [else (*
           x
           (factorial (- x 1)))])
  )

