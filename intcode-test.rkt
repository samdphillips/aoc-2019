#lang racket/base

(require (for-syntax racket/base)
         racket/format
         racket/port
         racket/vector
         rackunit
         syntax/parse/define

         "intcode.rkt")

(test-case
  "load memory"
  (let ([memory (call-with-input-string "1,2,3,4,5,6" load-memory-from-file)])
    (for ([v (in-vector (vector 1 2 3 4 5 6))]
          [i (in-naturals)])
      (check-equal? (memref memory i) v)))

  (let ([memory (call-with-input-string "1,2,3,4,5,6\r" load-memory-from-file)])
    (for ([v (in-vector (vector 1 2 3 4 5 6))]
          [i (in-naturals)])
      (check-equal? (memref memory i) v))))

(test-case
 "steppy"
 (define m
   (make-intcode-machine 0 (->memory (vector 1101 1 1 3 99))))
 (intcode-machine-step! m)
 (check-equal? (memref (intcode-machine-memory m) 3) 2)
 (intcode-machine-step! m)
 (check-true (intcode-machine-stopped? m)))

(define-syntax-parser check-intcode
  [(_ {~optional {~seq #:label label}}
      #:mem mem:expr
      {~optional {~seq #:inputs inputs}}
      assertions ...)
   #'(test-case
      (~a {~? label ""})
      (define pmem (memory-copy (->memory mem)))
      (define in-dev  (make-io-queue))
      {~? (io-queue-enqueue-all! in-dev inputs)}
      (define out-dev (make-io-queue))
      (parameterize ([intcode-machine-display-output? #f])
        (define test-machine
          (make-intcode-machine 0 pmem #:in-dev in-dev #:out-dev out-dev))
        (intcode-machine-run! test-machine))
      (check-intcode-assertion pmem out-dev assertions) ...)])

(define-syntax-parser check-intcode-assertion
  [(_ mem out-dev [#:mem= idx val])
   #'(check-equal? (memref mem idx) val)]
  [(_ mem out-dev [#:out (val)])
   #'(check-intcode-assertion mem out-dev [#:out val])]
  [(_ mem out-dev [#:out (v1 v2 vals ...)])
   #'(begin
       (check-intcode-assertion mem out-dev [#:out v1])
       (check-intcode-assertion mem out-dev [#:out (v2 vals ...)]))]
  [(_ mem out-dev [#:out val])
   #'(check-equal? (io-queue-dequeue! out-dev) val)])

(check-intcode #:mem #(1 0 0 0 99)
               [#:mem= 0 2])
(check-intcode #:mem #(2 3 0 3 99)
               [#:mem= 3 6])
(check-intcode #:mem #(2 4 4 5 99 0)
               [#:mem= 5 9801])
(check-intcode #:mem #(1 1 1 4 99 5 6 0 99)
               [#:mem= 0 30])
(check-intcode #:mem #(1 1 1 4 99 5 6 0 99)
               [#:mem= 4 2])

;; modes
(check-intcode #:mem "1002,5,3,5,99,33"
               [#:mem= 5 99])

;; day 5 part 1 test
(let ([mem (call-with-input-file "inputs/05.txt" load-memory-from-file)])
  (check-intcode #:label "day 5 part 1"
                 #:mem mem
                 #:inputs '(1)
                 [#:out (0 0 0 0 0 0 0 0 0 16348437)]))

;; $eq
(check-intcode #:mem "3,9,8,9,10,9,4,9,99,-1,8"
               #:inputs '(8)
               [#:out 1])
(check-intcode #:mem "3,9,8,9,10,9,4,9,99,-1,8"
               #:inputs '(7)
               [#:out 0])
(check-intcode #:mem "3,3,1108,-1,8,3,4,3,99"
               #:inputs '(8)
               [#:out 1])
(check-intcode #:mem "3,3,1108,-1,8,3,4,3,99"
               #:inputs '(7)
               [#:out 0])

;; $lt
(check-intcode #:mem "3,9,7,9,10,9,4,9,99,-1,8"
               #:inputs '(8)
               [#:out 0])
(check-intcode #:mem "3,9,7,9,10,9,4,9,99,-1,8"
               #:inputs '(7)
               [#:out 1])
(check-intcode #:mem "3,3,1107,-1,8,3,4,3,99"
               #:inputs '(8)
               [#:out 0])
(check-intcode #:mem "3,3,1107,-1,8,3,4,3,99"
               #:inputs '(7)
               [#:out 1])

;; $jmp*
(check-intcode #:mem "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
               #:inputs '(0)
               [#:out 0])
(check-intcode #:mem "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
               #:inputs '(42)
               [#:out 1])
(check-intcode #:mem "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
               #:inputs '(0)
               [#:out 0])
(check-intcode #:mem "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
               #:inputs '(42)
               [#:out 1])

(let ([mem (call-with-input-string
            (string-append
             "3,21,1008,21,8,20,1005,20,22,107,8,21,20,"
             "1006,20,31,1106,0,36,98,0,0,1002,21,125,20,"
             "4,20,1105,1,46,104,999,1105,1,46,1101,1000,"
             "1,20,4,20,1105,1,46,98,99")
            load-memory-from-file)])
  (check-intcode #:mem mem
                 #:inputs '(4)
                 [#:out 999])
  (check-intcode #:mem mem
                 #:inputs '(8)
                 [#:out 1000])
  (check-intcode #:mem mem
                 #:inputs '(42)
                 [#:out 1001]))

;; day 5 part 2 test
(let ([mem (call-with-input-file "inputs/05.txt" load-memory-from-file)])
  (check-intcode #:label "day 5 part 2"
                 #:mem mem
                 #:inputs '(5)
                 [#:out 6959377]))

(check-intcode #:label "day 9 part 1 example 1"
               #:mem
               (~a "109,1,204,-1,1001,100,1,100,1008,100,16,101,"
                   "1006,101,0,99")
               [#:out (109 1 204 -1 1001 100 1 100 1008 100 16 101
                       1006 101 0 99)])

(check-intcode #:label "day 9 part 1 example 2"
               #:mem
               "1102,34915192,34915192,7,4,7,99,0" 
               [#:out 1219070632396864])

(check-intcode #:label "day 9 part 1 example 3"
               #:mem
               "104,1125899906842624,99"
               [#:out 1125899906842624])

(define-simple-macro (test-amplifier label:str
                                     mem:expr
                                     (inputs:integer ...)
                                     result)
  (test-case
   label
   (define memory (->memory mem))
   (define amplifier-out-dev
     (parameterize ([intcode-machine-display-output? #f])
       (run-amplifiers! memory '(inputs ...))))
   (check-equal? (io-queue-dequeue! amplifier-out-dev)
                 result)))

(test-amplifier "basic adding input amplifier"
                (vector 3 11       ;; in $a
                        3 12       ;; in $b
                        1 11 12 13 ;; add $a $b $c
                        4 13       ;; out $c
                        99         ;; halt
                        0 0 0      ;; $a $b $c
                        )
                (1 1 1 1 1)
                5)

(test-amplifier "part 1 - example 1"
                "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
                (4 3 2 1 0)
                43210)

(test-amplifier "part 1 - example 2"
                (~a "3,23,3,24,1002,24,10,24,1002,23,-1,23,"
                    "101,5,23,23,1,24,23,23,4,23,99,0,0")
                (0 1 2 3 4)
                54321)

(test-amplifier "part 1 - example 3"
                (~a "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,"
                    "33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,"
                    "0,0,0")
                (1 0 4 3 2)
                65210)


(define-simple-macro (test-feedback-amplifier label:str
                                              mem:expr
                                              (inputs:nat ...)
                                              result)
  (test-case
   label
   (define memory (->memory mem))
   (define amplifier-out-dev
     (parameterize ([intcode-machine-display-output? #f])
       (run-feedback-amplifiers! memory '(inputs ...))))
   (check-equal? (io-queue-dequeue! amplifier-out-dev)
                 result)))

(test-feedback-amplifier "basic adding input feedback amplifier"
                         (vector 3 11       ;; in $a
                                 3 12       ;; in $b
                                 1 11 12 13 ;; add $a $b $c
                                 4 13       ;; out $c
                                 99         ;; halt
                                 0 0 0      ;; $a $b $c
                                 )
                         (1 1 1 1 1)
                         5)

(test-feedback-amplifier "part 2 - example 1"
                         (~a "3,26,1001,26,-4,26,3,27,1002,27,2,27,"
                             "1,27,26,27,4,27,1001,28,-1,28,1005,28,"
                             "6,99,0,0,5")
                         (9 8 7 6 5)
                         139629729)

(test-feedback-amplifier "part 2 - example 2"
                         (~a "3,52,1001,52,-5,52,3,53,1,52,56,54,"
                             "1007,54,5,55,1005,55,26,1001,54,-5,"
                             "54,1105,1,12,1,53,54,53,1008,54,0,"
                             "55,1001,55,1,55,2,53,55,53,4,53,"
                             "1001,56,-1,56,1005,56,6,99,"
                             "0,0,0,0,10")
                         (9 7 8 5 6)
                         18216)
