#lang racket/base

#|

https://adventofcode.com/2019/day/2

|#

(require racket/match
         racket/port
         racket/string
         racket/vector
         threading)

(module+ test
  (require racket/format
           racket/vector
           rackunit))

;; load-program : Input-Port -> (Vectorof Nonnegative-Integer)
(define (load-program an-input-port)
  (~>> (port->string an-input-port)
       (string-split _ #rx",")
       (map string->number)
       (apply vector)))

(module+ test
  (check-equal? (call-with-input-string "1,2,3,4,5,6" load-program)
                (vector 1 2 3 4 5 6)))

;; run-intcode! : (Vectorof Nonnegative-Integer)
(define (run-intcode! pgm)
  ;; memset! : Addr Nonnegative-Integer
  ;; looks up Addr in i and sets that Addr to v
  (define (memset! i v)
    (vector-set! pgm i v))

  ;; memref : Addr -> Nonnegative-Integer
  ;; looks up Addr in memory and returns value at that Addr
  (define memref
    (lambda~>> (vector-ref pgm)))

  ;; mem-deref : Addr -> Nonnegative-Integer
  ;; look up the Value in the cell pointed to by Addr
  (define mem-deref
    (lambda~> memref memref))

  ;; do-operation! : Addr (Int Int -> Int)
  (define (do-operation! ip op incr)
    (memset! (memref (+ ip 3))
             (op (mem-deref (+ ip 1))
                 (mem-deref (+ ip 2))))
    (run (+ incr ip)))

  ;; run : Addr
  ;; runs until ip points to a cell containing 99
  (define (run ip)
    (match (vector-ref pgm ip)
      [1  (do-operation! ip + 4)]
      [2  (do-operation! ip * 4)]
      [99 (void)]))
  (run 0))

(module+ test
  (define-syntax check-intcode
    (syntax-rules ()
      [(_ pgm idx val)
       (test-case
        (~a 'pgm)
        (define p (vector-copy pgm))
        (run-intcode! p)
        (check-equal? (vector-ref p idx) val))]))

  (check-intcode #(1 0 0 0 99) 0 2)
  (check-intcode #(2 3 0 3 99) 3 6)
  (check-intcode #(2 4 4 5 99 0) 5 9801)
  (check-intcode #(1 1 1 4 99 5 6 0 99) 0 30)
  (check-intcode #(1 1 1 4 99 5 6 0 99) 4 2))

(module* part-one #f
  (define pgm (call-with-input-file "inputs/02.txt" load-program))
  (vector-set! pgm 1 12)
  (vector-set! pgm 2 2)
  (run-intcode! pgm)
  (vector-ref pgm 0))


(define (find-inputs pgm)
  (define (run-once noun verb)
    (define trial-pgm (vector-copy pgm))
    (vector-set! trial-pgm 1 noun)
    (vector-set! trial-pgm 2 verb)
    (run-intcode! trial-pgm)
    (vector-ref trial-pgm 0))
  (let/ec return
    (for* ([n 100] [v 100])
      (when (= 19690720 (run-once n v))
        (return n v)))))

(module* part-two #f
  (define-values (noun verb)
    (find-inputs
     (call-with-input-file "inputs/02.txt" load-program)))
  (+ (* 100 noun) verb))
