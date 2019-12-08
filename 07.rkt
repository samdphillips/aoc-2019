#lang racket/base

#|

|#

(require racket/format
         racket/match
         racket/port
         racket/stream
         racket/string
         racket/vector
         threading)

(module+ test
  (require (for-syntax racket/base)
           racket/format
           racket/vector
           rackunit
           syntax/parse/define))

;; Program = Memory = (Vectorof Integer)

;; load-memory : Input-Port -> Memory
(define (load-memory an-input-port)
  (~>> (port->string an-input-port)
       (string-split _ #rx",")
       (map (lambda~> string-trim))
       (map string->number)
       (apply vector)))

(module+ test
  (check-equal? (call-with-input-string "1,2,3,4,5,6" load-memory)
                (vector 1 2 3 4 5 6))
  (check-equal? (call-with-input-string "1,2,3,4,5,6\r" load-memory)
                (vector 1 2 3 4 5 6)))


;; memset! : Memory Addr Nonnegative-Integer -> Void
;; looks up Addr in i and sets that Addr to v
(define memset! vector-set!)

;; memref : Memory Addr -> Nonnegative-Integer
;; looks up Addr in memory and returns value at that Addr
(define memref vector-ref)

;; memderef : Memory Addr -> Nonnegative-Integer
;; look up the Value in the cell pointed to by Addr
(define (memderef mem ptr)
  (~>> (memref mem ptr)
       (memref mem)))

(struct io-queue (fore aft)
  #:transparent
  #:mutable)

(define (make-io-queue)
  (io-queue null null))

(define (io-queue-length q)
  (+ (length (io-queue-fore q))
     (length (io-queue-aft q))))

(define (io-queue-empty? q)
  (zero? (io-queue-length q)))

(define (io-queue-enqueue! q val)
  (match-define (io-queue _ aft) q)
  (set-io-queue-aft! q (cons val aft)))

(define (io-queue-enqueue-all! q vals)
  (for ([v (in-list vals)])
    (io-queue-enqueue! q v)))

(define (io-queue-dequeue! q)
  (if (io-queue-empty? q)
      #f
      (match q
        [(io-queue '() aft)
         (set-io-queue-aft!  q null)
         (set-io-queue-fore! q (reverse aft))
         (io-queue-dequeue! q)]
        [(io-queue (cons val fore) _)
         (set-io-queue-fore! q fore)
         val])))

;; read-in! : IO-Queue -> (or/c #f Integer)
;; remove next input from queue unless queue is empty then
;; return #f
(define (read-in! in-dev)
  (io-queue-dequeue! in-dev))

;; write-out! : IO-Queue -> Void
;; writes an item to the io-queue
(define (write-out! out-dev val)
  (io-queue-enqueue! out-dev val))

(define (mode-ref mode mem ptr)
  (match mode
    ['imm (memref mem ptr)]
    ['pos (memderef mem ptr)]))

(struct $add (a b)
  #:transparent
  #:property prop:procedure
  (lambda ($inst in-dev out-dev mem ip)
    (memset! mem
             (memref mem (+ 3 ip))
             (+ (mode-ref ($add-a $inst) mem (+ 1 ip))
                (mode-ref ($add-b $inst) mem (+ 2 ip))))
    (+ 4 ip)))

(struct $mul (a b)
  #:transparent
  #:property prop:procedure
  (lambda ($inst in-dev out-dev mem ip)
    (memset! mem
             (memref mem (+ 3 ip))
             (* (mode-ref ($mul-a $inst) mem (+ 1 ip))
                (mode-ref ($mul-b $inst) mem (+ 2 ip))))
    (+ 4 ip)))

(struct $in ()
  #:transparent
  #:property prop:procedure
  (lambda ($inst in-dev out-dev mem ip)
    (memset! mem (memref mem (+ 1 ip)) (read-in! in-dev))
    (+ 2 ip)))

(define display-output (make-parameter #t))

(struct $out (mode)
  #:transparent
  #:property prop:procedure
  (lambda ($inst in-dev out-dev mem ip)
    (define val (mode-ref ($out-mode $inst) mem (+ 1 ip)))
    (when (display-output)
      (display (~a  val " ")))
    (write-out! out-dev val)
    (+ 2 ip)))

(struct $jmpt (test addr)
  #:transparent
  #:property prop:procedure
  (lambda ($inst in-dev out-dev mem ip)
    (if (zero? (mode-ref ($jmpt-test $inst) mem (+ 1 ip)))
        (+ 3 ip)
        (mode-ref ($jmpt-addr $inst) mem (+ 2 ip)))))

(struct $jmpf (test addr)
  #:transparent
  #:property prop:procedure
  (lambda ($inst in-dev out-dev mem ip)
    (if (zero? (mode-ref ($jmpf-test $inst) mem (+ 1 ip)))
        (mode-ref ($jmpf-addr $inst) mem (+ 2 ip))
        (+ 3 ip))))

(struct $lt (a b)
  #:transparent
  #:property prop:procedure
  (lambda ($inst in-dev out-dev mem ip)
    (define v1 (mode-ref ($lt-a $inst) mem (+ 1 ip)))
    (define v2 (mode-ref ($lt-b $inst) mem (+ 2 ip)))
    (memset! mem
             (memref mem (+ 3 ip))
             (if (< v1 v2) 1 0))
    (+ 4 ip)))

(struct $eq (a b)
  #:transparent
  #:property prop:procedure
  (lambda ($inst in-dev out-dev mem ip)
    (define v1 (mode-ref ($eq-a $inst) mem (+ 1 ip)))
    (define v2 (mode-ref ($eq-b $inst) mem (+ 2 ip)))
    (memset! mem
             (memref mem (+ 3 ip))
             (if (= v1 v2) 1 0))
    (+ 4 ip)))


(define instruction-num-decode-args (vector #f 2 2 0 1 2 2 2 2))
(define instruction-makers
  (vector #f $add $mul $in $out $jmpt $jmpf $lt $eq))

;; decode-instruction : Nonnegative-Integer -> $instruction
(define (decode-instruction rator+modes)
  (define-values (mode-nums rator) (quotient/remainder rator+modes 100))
  (define num-args (vector-ref instruction-num-decode-args rator))
  (define modes
    (for/fold ([modes null]
               [mode-nums mode-nums]
               #:result (reverse modes))
              ([i (in-range num-args)])
      (let-values ([(mode-nums mode) (quotient/remainder mode-nums 10)])
        (values (cons (if (zero? mode) 'pos 'imm)
                      modes)
                mode-nums))))
  (apply (vector-ref instruction-makers rator) modes))

(struct machine
  (memory [ip #:mutable] [halted? #:mutable] in-dev out-dev))

(define (step-intcode! a-machine)
  (match-define (machine mem ip halted? in-dev out-dev) a-machine)

  (define (do-operation! opcode)
    (define $inst (decode-instruction opcode))
    (define new-ip
      ($inst in-dev out-dev mem ip))
    (set-machine-ip! a-machine new-ip))

  (unless halted?
    (match (memref mem ip)
      [99 (set-machine-halted?! a-machine #t)]
      [opcode (do-operation! opcode)])))

(module+ test
  (test-case
   "steppy"
   (define m
     (machine (vector 1101 1 1 3 99)
              0
              #f
              (make-io-queue)
              (make-io-queue)))
   (step-intcode! m)
   (check-equal? (memref (machine-memory m) 3) 2)
   (step-intcode! m)
   (check-true (machine-halted? m))))


#|
run-intcode! : Memory
               #:start [Addr]
               #:inputs [(Streamof Integer)]
               -> Void
|#
(define (run-intcode! mem
                      #:start  [ip 0]
                      #:input  [in-dev  (make-io-queue)]
                      #:output [out-dev (make-io-queue)])
  (define a-machine
    (machine mem ip #f in-dev out-dev))
  (define (run)
    (unless (machine-halted? a-machine)
      (step-intcode! a-machine)
      (run)))
  (run))

(module+ test
  (begin-for-syntax
    (define-syntax-class intcode-memory
      [pattern literal:str
               #:attr expr
               #'(call-with-input-string literal load-memory)]
      [pattern literal
               #:attr expr #'literal]))

  (define-syntax-parser check-intcode
    [(_ {~optional {~seq #:label label}}
        #:mem mem:intcode-memory
        {~optional {~seq #:inputs inputs}}
        assertions ...)
     #'(test-case
        (~a {~? label 'mem.literal})
        (define pmem (vector-copy mem.expr))
        (define in-dev  (make-io-queue))
        {~? (io-queue-enqueue-all! in-dev inputs)}
        (define out-dev (make-io-queue))
        (parameterize ([display-output #f])
          (run-intcode! pmem
                        #:input  in-dev
                        #:output out-dev))
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
  (let ([mem (call-with-input-file "inputs/05.txt" load-memory)])
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
              load-memory)])
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
  (let ([mem (call-with-input-file "inputs/05.txt" load-memory)])
    (check-intcode #:label "day 5 part 2"
                   #:mem mem
                   #:inputs '(5)
                   [#:out 6959377])))


(module* part-one #f)

(module* part-two #f)