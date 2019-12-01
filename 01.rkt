#lang racket/base

#|

https://adventofcode.com/2019/day/1

|#

;; fuel-needed : Int -> Int
(define (fuel-needed mass)
  (- (quotient mass 3) 2))

(module+ test
  (require rackunit)
  (check-equal? (fuel-needed 12) 2)
  (check-equal? (fuel-needed 14) 2)
  (check-equal? (fuel-needed 1969) 654)
  (check-equal? (fuel-needed 100756) 33583))

(module* part-one #f
  (call-with-input-file "inputs/01.txt"
    (lambda (input)
      (for/sum ([mass (in-port read input)])
        (fuel-needed mass)))))

;; total-fuel-needed : Int [Int] -> Int
(define (total-fuel-needed mass [fuel 0])
  (define new-fuel (fuel-needed mass))
  (cond
    [(<= new-fuel 0) fuel]
    [else
     (total-fuel-needed new-fuel (+ fuel new-fuel))]))

(module+ test
  (check-equal? (total-fuel-needed 14) 2)
  (check-equal? (total-fuel-needed 1969) 966)
  (check-equal? (total-fuel-needed 100756) 50346))

(module* part-two #f
  (call-with-input-file "inputs/01.txt"
    (lambda (input)
      (for/sum ([mass (in-port read input)])
        (total-fuel-needed mass)))))