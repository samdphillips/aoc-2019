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
       (map string->number)
       (apply vector)))

(module+ test
  (check-equal? (call-with-input-string "1,2,3,4,5,6" load-memory)
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


;; Input-Device = (Box (Streamof Integer))
;; read-in! : Input-Device -> Integer
;; side-effect: updates box
(define (read-in! in-dev)
  (define s (unbox in-dev))
  (define v (stream-first s))
  (set-box! in-dev (stream-rest s))
  v)

(define (mode-ref mode pgm ptr)
  (match mode
    ['imm (memref pgm ptr)]
    ['pos (memderef pgm ptr)]))

(struct $add (a b)
  #:property prop:procedure
  (lambda (inst in-dev mem ip)
    (memset! mem
             (memref mem (+ 3 ip))
             (+ (mode-ref ($add-a inst) mem (+ 1 ip))
                (mode-ref ($add-b inst) mem (+ 2 ip))))
    (+ 4 ip)))

(struct $mul (a b)
  #:property prop:procedure
  (lambda (inst in-dev mem ip)
    (memset! mem
             (memref mem (+ 3 ip))
             (* (mode-ref ($mul-a inst) mem (+ 1 ip))
                (mode-ref ($mul-b inst) mem (+ 2 ip))))
    (+ 4 ip)))

(struct $in ()
  #:property prop:procedure
  (lambda (inst in-dev mem ip)
    (memset! mem (memref mem (+ 1 ip)) (read-in! in-dev))
    (+ 2 ip)))

(struct $out (mode)
  #:property prop:procedure
  (lambda (inst in-dev mem ip)
    (display (~a (mode-ref ($out-mode inst) mem (+ 1 ip)) " "))
    (+ 2 ip)))

(define instruction-num-decode-args (vector #f 2 2 0 1))
(define instruction-makers
  (vector #f $add $mul $in $out))

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


#|
run-intcode! : Memory
               #:start [Addr]
               #:inputs [(Streamof Integer)]
               -> Void
|#
(define (run-intcode! mem #:start [ip 0] #:inputs [inputs null])
  (define in-dev (box inputs))

  ;; do-operation! : Nonnegative-Integer Addr -> Void
  (define (do-operation! opcode ip)
    (define $inst (decode-instruction opcode))
    (define new-ip
      ($inst in-dev mem ip))
    (run new-ip))

  ;; run : Addr
  ;; runs until ip points to a cell containing 99
  (define (run ip)
    (match (memref mem ip)
      [99 (void)]
      [opcode (do-operation! opcode ip)]))
  (run ip))

(module+ test
  (begin-for-syntax
    (define-syntax-class intcode-memory
      [pattern literal:str
               #:attr expr
               #'(call-with-input-string literal load-memory)]
      [pattern literal
               #:attr expr #'literal]))

  (define-syntax-parser check-intcode
    [(_ #:mem mem:intcode-memory assertions ...)
     #'(test-case
        (~a 'mem.literal)
        (define pmem (vector-copy mem.expr))
        (run-intcode! pmem)
        (check-intcode-assertion pmem assertions) ...)])

  (define-syntax-parser check-intcode-assertion
    [(_ mem [#:mem= idx val])
     #'(check-equal? (memref mem idx) val)])

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
                 [#:mem= 5 99]))

(module* part-one #f
  (define mem (call-with-input-file "inputs/05.txt" load-memory))
  (run-intcode! mem #:inputs (list 1)))


(module* part-two #f)
