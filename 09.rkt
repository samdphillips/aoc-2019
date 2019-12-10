#lang racket/base

#|

Behold!  A lot of imperative code and mutation!

|#

(require racket/format
         racket/match
         racket/port
         racket/string
         racket/vector
         mischief/for
         threading)

(module+ test
  (require (for-syntax racket/base)
           racket/format
           racket/vector
           rackunit
           syntax/parse/define))

#|
                   #    # ###### #    #  ####  #####  #   #
                   ##  ## #      ##  ## #    # #    #  # #
                   # ## # #####  # ## # #    # #    #   #
                   #    # #      #    # #    # #####    #
                   #    # #      #    # #    # #   #    #
                   #    # ###### #    #  ####  #    #   #
|#

;; Pagetable : (Hash Nonnegative-Integer Page)
;; Program = Memory = Pagetable
(struct pagetable (pages))

(define pagetable-addr-shift 3)
(define pagetable-addr-mask  #b111)
(define pagetable-page-size  (add1 pagetable-addr-mask))

(define (make-pagetable)
  (pagetable (make-hash)))

(define (memory-copy memory)
  (pagetable
    (for/hash! ([(pageid page) (in-hash (pagetable-pages memory))])
      (values pageid (vector-copy page)))))

(define (pagetable-alloc! memory pageid)
  (hash-set! (pagetable-pages memory)
             pageid
             (make-vector pagetable-page-size 0)))

(define (load-pagetable a-sequence)
  (define memory (make-pagetable))
  (for ([v    a-sequence]
        [addr (in-naturals)])
    (memset! memory 'pos #f addr v))
  memory)

(define (pagetable-physical-addr vaddr)
  (values (arithmetic-shift vaddr pagetable-addr-shift)
          (bitwise-and vaddr pagetable-addr-mask)))

(define (pagetable-has-page? memory pageid)
  (hash-has-key? (pagetable-pages memory) pageid))

(define (pagetable-page-ref memory pageid)
  (unless (pagetable-has-page? memory pageid)
    (pagetable-alloc! memory pageid))
  (hash-ref (pagetable-pages memory) pageid))

;; load-memory : Input-Port -> Memory
(define (load-memory an-input-port)
  (~>> (port->string an-input-port)
       (string-split _ #rx",")
       (map (lambda~> string-trim))
       (map string->number)
       (load-pagetable)))

(define (->memory a-value)
  (match a-value
    [(? pagetable?) a-value]
    [(? vector?)    (load-pagetable a-value)]
    [(? string?)
     (call-with-input-string a-value load-memory)]))


(module+ test
  (test-begin
    "load values"
    (let ([memory (call-with-input-string "1,2,3,4,5,6" load-memory)])
      (for ([v (in-vector (vector 1 2 3 4 5 6))]
            [i (in-naturals)])
        (check-equal? (memref memory i) v)))

    (let ([memory (call-with-input-string "1,2,3,4,5,6\r" load-memory)])
      (for ([v (in-vector (vector 1 2 3 4 5 6))]
            [i (in-naturals)])
        (check-equal? (memref memory i) v)))))


;; memset! : Memory Mode Addr Addr Nonnegative-Integer -> Void
;; looks up Addr (possibly modified by base addr depending on mode)
;; in i and sets that Addr to v
(define (memset! mem mode base inst-addr val)
  (define addr
    (match mode
      ['pos inst-addr]
      ['rel (+ inst-addr base)]))
  (define-values (pageid page-offset)
    (pagetable-physical-addr addr))
  (define page (pagetable-page-ref mem pageid))
  (vector-set! page page-offset val))

;; memref : Memory Addr -> Nonnegative-Integer
;; looks up Addr in memory and returns value at that Addr
(define (memref memory addr)
  (define-values (pageid page-offset)
    (pagetable-physical-addr addr))
  (define page (pagetable-page-ref memory pageid))
  (vector-ref page page-offset))

;; memderef : Memory Addr -> Nonnegative-Integer
;; look up the Value in the cell pointed to by Addr
(define (memderef mem ptr)
  (~>> (memref mem ptr)
       (memref mem)))

#|
               #  ####         ####  #    # ###### #    # ######
               # #    #       #    # #    # #      #    # #
               # #    # ##### #    # #    # #####  #    # #####
               # #    #       #  # # #    # #      #    # #
               # #    #       #   #  #    # #      #    # #
               #  ####         ### #  ####  ######  ####  ######
|#
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

#|


 # #    #  ####  ##### #####  #    #  ####  ##### #  ####  #    #  ####
 # ##   # #        #   #    # #    # #    #   #   # #    # ##   # #
 # # #  #  ####    #   #    # #    # #        #   # #    # # #  #  ####
 # #  # #      #   #   #####  #    # #        #   # #    # #  # #      #
 # #   ## #    #   #   #   #  #    # #    #   #   # #    # #   ## #    #
 # #    #  ####    #   #    #  ####   ####    #   #  ####  #    #  ####


|#

(define (mode-ref mode mem base ptr)
  (match mode
    ['imm (memref mem ptr)]
    ['pos (memderef mem ptr)]
    ['rel (memref mem (+ base (memref mem ptr)))]))

(struct $add (a b c)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (machine _ mem ip _ base _ _) a-machine)
    (memset! mem
             ($add-c $inst)
             base
             (memref mem (+ 3 ip))
             (+ (mode-ref ($add-a $inst) mem base (+ 1 ip))
                (mode-ref ($add-b $inst) mem base (+ 2 ip))))
    (set-machine-ip! a-machine (+ 4 ip))))

(struct $mul (a b c)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (machine _ mem ip _ base _ _) a-machine)
    (memset! mem
             ($mul-c $inst)
             base
             (memref mem (+ 3 ip))
             (* (mode-ref ($mul-a $inst) mem base (+ 1 ip))
                (mode-ref ($mul-b $inst) mem base (+ 2 ip))))
    (set-machine-ip! a-machine (+ 4 ip))))

(struct $in (mode)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (machine _ mem ip _ base in-dev _) a-machine)
    (define val-in (read-in! in-dev))
    (cond
      [val-in
       (memset! mem ($in-mode $inst) base (memref mem (+ 1 ip)) val-in)
       (set-machine-ip! a-machine (+ 2 ip))]
      [else
       (set-machine-state! a-machine 'wait)])))

(define display-output? (make-parameter #f))

(struct $out (mode)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (machine _ mem ip _ base _ out-dev) a-machine)
    (define val (mode-ref ($out-mode $inst) mem base (+ 1 ip)))
    (when (display-output?)
      (display (~a  val " ")))
    (write-out! out-dev val)
    (set-machine-ip! a-machine (+ 2 ip))))

(struct $jmpt (test addr)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (machine _ mem ip _ base _ _) a-machine)
    (set-machine-ip!
     a-machine
     (if (zero? (mode-ref ($jmpt-test $inst) mem base (+ 1 ip)))
         (+ 3 ip)
         (mode-ref ($jmpt-addr $inst) mem base (+ 2 ip))))))

(struct $jmpf (test addr)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (machine _ mem ip _ base _ _) a-machine)
    (set-machine-ip!
     a-machine
     (if (zero? (mode-ref ($jmpf-test $inst) mem base (+ 1 ip)))
         (mode-ref ($jmpf-addr $inst) mem base (+ 2 ip))
         (+ 3 ip)))))

(struct $lt (a b c)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (machine _ mem ip _ base _ _) a-machine)
    (define v1 (mode-ref ($lt-a $inst) mem base (+ 1 ip)))
    (define v2 (mode-ref ($lt-b $inst) mem base (+ 2 ip)))
    (memset! mem
             ($lt-c $inst)
             base
             (memref mem (+ 3 ip))
             (if (< v1 v2) 1 0))
    (set-machine-ip! a-machine (+ 4 ip))))

(struct $eq (a b c)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (machine _ mem ip _ base _ _) a-machine)
    (define v1 (mode-ref ($eq-a $inst) mem base (+ 1 ip)))
    (define v2 (mode-ref ($eq-b $inst) mem base (+ 2 ip)))
    (memset! mem
             ($eq-c $inst)
             base
             (memref mem (+ 3 ip))
             (if (= v1 v2) 1 0))
    (set-machine-ip! a-machine (+ 4 ip))))

(struct $relbase (mode)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (machine _ mem ip _ base _ _) a-machine)
    (define new-base
      (+ base (mode-ref ($relbase-mode $inst) mem base (+ 1 ip))))
    (set-machine-relative-base! a-machine new-base)
    (set-machine-ip! a-machine (+ 2 ip))))

(define instruction-num-decode-args (vector #f 3 3 1 1 2 2 3 3 1))
(define instruction-makers
  (vector #f $add $mul $in $out $jmpt $jmpf $lt $eq $relbase))

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
        (values (cons (match mode
                        [0 'pos]
                        [1 'imm]
                        [2 'rel])
                      modes)
                mode-nums))))
  (apply (vector-ref instruction-makers rator) modes))

#|
                  #    #   ##    ####  #    # # #    # ######
                  ##  ##  #  #  #    # #    # # ##   # #
                  # ## # #    # #      ###### # # #  # #####
                  #    # ###### #      #    # # #  # # #
                  #    # #    # #    # #    # # #   ## #
                  #    # #    #  ####  #    # # #    # ######
|#

(struct machine
  (id
   memory
   [ip            #:mutable]
   [state         #:mutable]
   [relative-base #:mutable]
   in-dev
   out-dev))

(define (machine-ready? a-machine)
  (eq? (machine-state a-machine) 'ready))

(define (machine-stopped? a-machine)
  (eq? (machine-state a-machine) 'stop))

(define (machine-waiting? a-machine)
  (eq? (machine-state a-machine) 'wait))

(define stepping-trace? (make-parameter #f))

(define (step-intcode! a-machine)
  (match-define (machine id mem ip _ _ _ _) a-machine)

  (define (do-operation! opcode)
    (define $inst (decode-instruction opcode))
    (when (stepping-trace?)
      (displayln (~a "[" id "] " $inst)))
    ($inst a-machine)
    (when (stepping-trace?)
      (for-each (lambda (label accessor)
                  (displayln (~a "[" id "]     "
                                 label ": "
                                 (accessor a-machine))))
                (list 'mem 'ip 'state 'relative-base 'in-dev 'out-dev)
                (list machine-memory
                      machine-ip
                      machine-state
                      machine-relative-base
                      machine-in-dev
                      machine-out-dev))))

  (when (machine-ready? a-machine)
    (match (memref mem ip)
      [99 (set-machine-state! a-machine 'stop)]
      [opcode (do-operation! opcode)])))

;; machine-poll! : Machine -> Void
;; when a machine is in wait state, check if there are any values in
;; the in-dev and change the state to ready
(define (machine-poll! a-machine)
  (unless (io-queue-empty? (machine-in-dev a-machine))
    (set-machine-state! a-machine 'ready)))

(module+ test
  (test-case
   "steppy"
   (define m
     (machine 0
              (load-pagetable
                (vector 1101 1 1 3 99))
              0
              'ready
              0
              (make-io-queue)
              (make-io-queue)))
   (step-intcode! m)
   (check-equal? (memref (machine-memory m) 3) 2)
   (step-intcode! m)
   (check-true (machine-stopped? m))))

;; machine-run! : Machine -> Void
(define (machine-run! a-machine)
  (when (machine-ready? a-machine)
    (step-intcode! a-machine)
    (machine-run! a-machine)))


#|
setup-run-intcode! : Memory
               #:start [Addr]
               #:inputs [(Streamof Integer)]
               -> Void

Make a machine from inputs and run it until it's not in the ready state.
|#
(define (setup-run-intcode! mem
                            #:start  [ip 0]
                            #:input  [in-dev  (make-io-queue)]
                            #:output [out-dev (make-io-queue)])
  (define a-machine
    (machine 0 mem ip 'ready 0 in-dev out-dev))
  (machine-run! a-machine))

(module+ test
  (define-syntax-parser check-intcode
    [(_ {~optional {~seq #:label label}}
        #:mem mem:expr
        {~optional {~seq #:inputs inputs}}
        assertions ...)
     #'(test-case
        (~a {~? label 'mem})
        (define pmem (memory-copy (->memory mem)))
        (define in-dev  (make-io-queue))
        {~? (io-queue-enqueue-all! in-dev inputs)}
        (define out-dev (make-io-queue))
        (parameterize ([display-output? #f])
          (setup-run-intcode! pmem
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
                 [#:out 1125899906842624]))


#|

            ##   #    # #####  #      # ###### # ###### #####   ####
           #  #  ##  ## #    # #      # #      # #      #    # #
          #    # # ## # #    # #      # #####  # #####  #    #  ####
          ###### #    # #####  #      # #      # #      #####       #
          #    # #    # #      #      # #      # #      #   #  #    #
          #    # #    # #      ###### # #      # ###### #    #  ####

|#
(define (run-amplifiers! mem inputs)
  (define ios (for/list ([_n 6]) (make-io-queue)))
  (define amp-in-dev (car ios))
  (define amp-out-dev (car (reverse ios)))
  (define machines
    (for/list ([n       (in-naturals)]
               [in-dev  (in-list ios)]
               [out-dev (in-list (cdr ios))])
      (machine n (memory-copy mem) 0 'ready 0 in-dev out-dev)))

  (for ([v      (in-list inputs)]
        [in-dev (in-list ios)])
    (io-queue-enqueue! in-dev v))
  (io-queue-enqueue! amp-in-dev 0)

  (define (queue-machine q m)
    (append q (list m)))

  (define (step ready waiting stopped)
    (match* (ready waiting)
      [('() '()) amp-out-dev]
      [('() pending)
       (define-values (ready waiting)
         (for/fold ([r null] [w null]) ([m (in-list pending)])
           (machine-poll! m)
           (match (machine-state m)
             ['ready (values (cons m r) w)]
             ['wait  (values r (cons m w))])))
       (when (null? ready)
         (error 'run-amplifiers! "no ready machines"))
       (step ready waiting stopped)]
      [((cons a-machine ready) waiting)
       (machine-run! a-machine)
       (match (machine-state a-machine)
         ['wait
          (step ready (queue-machine waiting a-machine) stopped)]
         ['stop
          (step ready waiting (cons a-machine stopped))])]))
  (step machines null null))

(module+ test
  (define-simple-macro (test-amplifier label:str
                                       mem:expr
                                       (inputs:nat ...)
                                       result)
    (test-case
     label
     (define memory (->memory mem))
     (define amplifier-out-dev
       (parameterize ([display-output? #f])
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
                  65210))


#|
  run-feedback-amplifiers!
  Same as before, except we connect the first io-queue to the out-dev
  of the last intcode machine.
|#
(define (run-feedback-amplifiers! mem inputs)
  (define ios (for/list ([_n 5]) (make-io-queue)))
  (define amp-in-dev (car ios))
  (define amp-out-dev amp-in-dev)

  ;; set up connections (this time with feedback)
  (define machines
    (for/list ([n       (in-naturals)]
               [in-dev  (in-list ios)]
               [out-dev (in-list (append (cdr ios) (list amp-in-dev)))])
      (machine n (memory-copy mem) 0 'ready 0 in-dev out-dev)))

  ;; setup initial inputs
  (for ([v      (in-list inputs)]
        [in-dev (in-list ios)])
    (io-queue-enqueue! in-dev v))
  (io-queue-enqueue! amp-in-dev 0)

  (define (queue-machine q m)
    (append q (list m)))

  (define (step ready waiting stopped)
    (match* (ready waiting)
      [('() '()) amp-out-dev]
      [('() pending)
       (define-values (ready waiting)
         (for/fold ([r null] [w null]) ([m (in-list pending)])
           (machine-poll! m)
           (match (machine-state m)
             ['ready (values (cons m r) w)]
             ['wait  (values r (cons m w))])))
       (when (null? ready)
         (error 'run-feedback-amplifiers! "no ready machines"))
       (step ready waiting stopped)]
      [((cons a-machine ready) waiting)
       (machine-run! a-machine)
       (match (machine-state a-machine)
         ['wait
          (step ready (queue-machine waiting a-machine) stopped)]
         ['stop
          (step ready waiting (cons a-machine stopped))])]))
  (step machines null null))

(module+ test
  (define-simple-macro (test-feedback-amplifier label:str
                                                mem:expr
                                                (inputs:nat ...)
                                                result)
    (test-case
     label
     (define memory (->memory mem))
     (define amplifier-out-dev
       (parameterize ([display-output? #f])
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
                           18216))

(module* part-one #f
  (define memory (call-with-input-file "inputs/09.txt" load-memory))
  (define in-dev (make-io-queue))
  (define out-dev (make-io-queue))
  (io-queue-enqueue! in-dev 1)
  (define boost-machine
    (machine 0 memory 0 'ready 0 in-dev out-dev))
  (parameterize ([display-output? #t])
    (machine-run! boost-machine))
  (newline))

(module* part-two #f
  (define memory (call-with-input-file "inputs/09.txt" load-memory))
  (define in-dev (make-io-queue))
  (define out-dev (make-io-queue))
  (io-queue-enqueue! in-dev 2)
  (define boost-machine
    (machine 0 memory 0 'ready 0 in-dev out-dev))
  (parameterize ([display-output? #t])
    (machine-run! boost-machine))
  (newline))