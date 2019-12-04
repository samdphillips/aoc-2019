#lang racket/base

(require threading)

#|

|#

(module+ test
  (require rackunit))


;; has-duplicate? : Bytes -> Boolean
(define (has-duplicate? some-bytes)
  (define blen (bytes-length some-bytes))
  (define (ref i) (bytes-ref some-bytes i))
  (for/or ([i (in-range blen)]
           [j (in-range 1 blen)])
    (= (ref i) (ref j))))


(module+ test
  (check-true (has-duplicate? (bytes 1 1 2 3 4 5 6)))
  (check-true (has-duplicate? (bytes 1 2 3 4 6 6)))
  (check-true (has-duplicate? (bytes 1 2 2 4 6)))
  (check-false (has-duplicate? (bytes 1 2 3 4 5))))


;; increasing? : Bytes -> Boolean
(define (increasing? some-bytes)
  (define blen (bytes-length some-bytes))
  (define (ref i) (bytes-ref some-bytes i))
  (for/and ([i (in-range blen)]
            [j (in-range 1 blen)])
    (<= (ref i) (ref j))))


(module+ test
  (check-true (increasing? (bytes 1 2 3 4 5)))
  (check-true (increasing? (bytes 1 2 2 3 4 4 5)))
  (check-false (increasing? (bytes 5 4 3 2 1)))
  (check-false (increasing? (bytes 1 2 3 2 5))))


;; possible-password? : Bytes -> Boolean
(define (possible-password? some-bytes)
  (and (has-duplicate? some-bytes)
       (increasing? some-bytes)))


(module+ test
  (check-true (possible-password? (bytes 1 1 1 1 1 1)))
  (check-true (possible-password? (bytes 1 2 2 3 4 5)))
  (check-false (possible-password? (bytes 2 2 3 4 5 0)))
  (check-false (possible-password? (bytes 1 2 3 7 8 9))))


(define start 130254)
(define end 678275)

(define zero-char (char->integer #\0))

;; number->byte-digits : Int -> Bytes
(define number->byte-digits
  (lambda~>> number->string
             string->list
             (map (lambda~> char->integer
                            (- zero-char)))
             (apply bytes)))


(module* part-one #f
  (for/sum ([n (in-range start (add1 end))])
    (if (possible-password? (number->byte-digits n)) 1 0)))


;; proper-duplicate? : Bytes -> Boolean
(define (proper-duplicate? some-bytes)
  (define blen (bytes-length some-bytes))
  (define (ref i) (bytes-ref some-bytes i))
  ;; state0 - no proper duplicate yet
  (define (state0 i j)
    (cond
      [(= j blen) #f]
      [(= (ref i) (ref j)) (state1 j (add1 j))]
      [else
       (state0 (add1 i) (add1 j))]))
  ;; state1 - duplicate, but maybe too much?
  (define (state1 i j)
    (cond
      [(= j blen) #t]
      [(= (ref i) (ref j)) (state2 (add1 i) (add1 j))]
      [else #t]))
  ;; state2 - skip to the end of the current run duplicates
  (define (state2 i j)
    (cond
      [(= j blen) #f]
      [(= (ref i) (ref j)) (state2 (add1 i) (add1 j))]
      [else (state0 j (add1 j))]))
  (state0 0 1))


(module+ test
  (check-true (proper-duplicate?  (bytes 1 1 2 2 3 3)))
  (check-false (proper-duplicate? (bytes 1 2 3 4 4 4)))
  (check-true (proper-duplicate?  (bytes 1 1 1 1 2 2)))
  (check-true (proper-duplicate?  (bytes 2 2 1 1 1 1))))


;; possible-password/2? : Bytes -> Boolean
(define (possible-password/2? some-bytes)
  (and (increasing? some-bytes)
       (proper-duplicate? some-bytes)))


(module+ test
  (check-true (possible-password/2? (bytes 1 1 2 2 3 3)))
  (check-false (possible-password/2? (bytes 1 2 3 4 4 4)))
  (check-true (possible-password/2? (bytes 1 1 1 1 2 2))))


(module* part-two #f
  (for/sum ([n (in-range start (add1 end))])
    (if (possible-password/2? (number->byte-digits n)) 1 0)))