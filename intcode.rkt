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
         rebellion/type/singleton
         threading)

(provide ->memory
         load-memory-from-file
         memory-copy
         memref

         make-intcode-machine
         intcode-machine-memory
         intcode-machine-stopped?
         intcode-machine-step!
         intcode-machine-run!
         intcode-machine-display-output?

         make-io-queue
         io-queue-enqueue-all!
         io-queue-dequeue!

         multimachine-run!

         run-amplifiers!
         run-feedback-amplifiers!)

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

;; load-memory-from-file : Input-Port -> Memory
(define (load-memory-from-file an-input-port)
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
     (call-with-input-string a-value load-memory-from-file)]))

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
    (match-define (intcode-machine _ mem ip _ base _ _) a-machine)
    (memset! mem
             ($add-c $inst)
             base
             (memref mem (+ 3 ip))
             (+ (mode-ref ($add-a $inst) mem base (+ 1 ip))
                (mode-ref ($add-b $inst) mem base (+ 2 ip))))
    (set-intcode-machine-ip! a-machine (+ 4 ip))))

(struct $mul (a b c)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (intcode-machine _ mem ip _ base _ _) a-machine)
    (memset! mem
             ($mul-c $inst)
             base
             (memref mem (+ 3 ip))
             (* (mode-ref ($mul-a $inst) mem base (+ 1 ip))
                (mode-ref ($mul-b $inst) mem base (+ 2 ip))))
    (set-intcode-machine-ip! a-machine (+ 4 ip))))

(struct $in (mode)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (intcode-machine _ mem ip _ base in-dev _) a-machine)
    (define val-in (read-in! in-dev))
    (cond
      [val-in
       (memset! mem ($in-mode $inst) base (memref mem (+ 1 ip)) val-in)
       (set-intcode-machine-ip! a-machine (+ 2 ip))]
      [else
       (set-intcode-machine-state! a-machine wait)])))

(define intcode-machine-display-output? (make-parameter #f))

(struct $out (mode)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (intcode-machine _ mem ip _ base _ out-dev) a-machine)
    (define val (mode-ref ($out-mode $inst) mem base (+ 1 ip)))
    (when (intcode-machine-display-output?)
      (display (~a  val " ")))
    (write-out! out-dev val)
    (set-intcode-machine-ip! a-machine (+ 2 ip))))

(struct $jmpt (test addr)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (intcode-machine _ mem ip _ base _ _) a-machine)
    (set-intcode-machine-ip!
     a-machine
     (if (zero? (mode-ref ($jmpt-test $inst) mem base (+ 1 ip)))
         (+ 3 ip)
         (mode-ref ($jmpt-addr $inst) mem base (+ 2 ip))))))

(struct $jmpf (test addr)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (intcode-machine _ mem ip _ base _ _) a-machine)
    (set-intcode-machine-ip!
     a-machine
     (if (zero? (mode-ref ($jmpf-test $inst) mem base (+ 1 ip)))
         (mode-ref ($jmpf-addr $inst) mem base (+ 2 ip))
         (+ 3 ip)))))

(struct $lt (a b c)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (intcode-machine _ mem ip _ base _ _) a-machine)
    (define v1 (mode-ref ($lt-a $inst) mem base (+ 1 ip)))
    (define v2 (mode-ref ($lt-b $inst) mem base (+ 2 ip)))
    (memset! mem
             ($lt-c $inst)
             base
             (memref mem (+ 3 ip))
             (if (< v1 v2) 1 0))
    (set-intcode-machine-ip! a-machine (+ 4 ip))))

(struct $eq (a b c)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (intcode-machine _ mem ip _ base _ _) a-machine)
    (define v1 (mode-ref ($eq-a $inst) mem base (+ 1 ip)))
    (define v2 (mode-ref ($eq-b $inst) mem base (+ 2 ip)))
    (memset! mem
             ($eq-c $inst)
             base
             (memref mem (+ 3 ip))
             (if (= v1 v2) 1 0))
    (set-intcode-machine-ip! a-machine (+ 4 ip))))

(struct $relbase (mode)
  #:transparent
  #:property prop:procedure
  (lambda ($inst a-machine)
    (match-define (intcode-machine _ mem ip _ base _ _) a-machine)
    (define new-base
      (+ base (mode-ref ($relbase-mode $inst) mem base (+ 1 ip))))
    (set-intcode-machine-relative-base! a-machine new-base)
    (set-intcode-machine-ip! a-machine (+ 2 ip))))

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

(define-singleton-type ready)
(define-singleton-type stop)
(define-singleton-type wait)

(struct intcode-machine
  (id
   memory
   [ip            #:mutable]
   [state         #:mutable]
   [relative-base #:mutable]
   in-dev
   out-dev))

(define (make-intcode-machine id 
                              memory 
                              #:ip [ip 0]
                              #:state [state ready]
                              #:in-dev [in-dev (make-io-queue)]
                              #:out-dev [out-dev (make-io-queue)])
  (intcode-machine id memory ip state 0 in-dev out-dev))

(define (intcode-machine-ready? a-machine)
  (ready? (intcode-machine-state a-machine)))

(define (intcode-machine-stopped? a-machine)
  (stop? (intcode-machine-state a-machine)))

(define (intcode-machine-waiting? a-machine)
  (wait? (intcode-machine-state a-machine)))

(define intcode-machine-stepping-trace? (make-parameter #f))

(define (intcode-machine-step! a-machine)
  (match-define (intcode-machine id mem ip _ _ _ _) a-machine)

  (define (do-operation! opcode)
    (define $inst (decode-instruction opcode))
    (when (intcode-machine-stepping-trace?)
      (displayln (~a "[" id "] " $inst)))
    ($inst a-machine)
    (when (intcode-machine-stepping-trace?)
      (for-each (lambda (label accessor)
                  (displayln (~a "[" id "]     "
                                 label ": "
                                 (accessor a-machine))))
                (list 'mem 'ip 'state 'relative-base 'in-dev 'out-dev)
                (list intcode-machine-memory
                      intcode-machine-ip
                      intcode-machine-state
                      intcode-machine-relative-base
                      intcode-machine-in-dev
                      intcode-machine-out-dev))))

  (when (intcode-machine-ready? a-machine)
    (match (memref mem ip)
      [99 (set-intcode-machine-state! a-machine stop)]
      [opcode (do-operation! opcode)])))

;; intcode-machine-poll! : Intcode-Machine -> Void
;; when a machine is in wait state, check if there are any values in
;; the in-dev and change the state to ready
(define (intcode-machine-poll! a-machine)
  (unless (io-queue-empty? (intcode-machine-in-dev a-machine))
    (set-intcode-machine-state! a-machine ready)))

;; intcode-machine-run! : Intcode-Machine -> Void
(define (intcode-machine-run! a-machine)
  (when (intcode-machine-ready? a-machine)
    (intcode-machine-step! a-machine)
    (intcode-machine-run! a-machine)))

(define (multimachine-run! machine-list)
  (define (queue-machine q m)
    (append q (list m)))

  (define (step ready waiting stopped)
    (match* (ready waiting)
      [('() '()) (void)]
      [('() pending)
       (define-values (ready waiting)
         (for/fold ([r null] [w null]) ([m (in-list pending)])
           (intcode-machine-poll! m)
           (match (intcode-machine-state m)
             [(? ready?) (values (cons m r) w)]
             [(? wait?)  (values r (cons m w))])))
       (when (null? ready)
         (error 'multimachine-run! "no ready machines"))
       (step ready waiting stopped)]
      [((cons a-machine ready) waiting)
       (intcode-machine-run! a-machine)
       (match (intcode-machine-state a-machine)
         [(? wait?)
          (step ready (queue-machine waiting a-machine) stopped)]
         [(? stop?)
          (step ready waiting (cons a-machine stopped))])]))

  (step machine-list null null))

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
      (make-intcode-machine n (memory-copy mem) #:in-dev in-dev #:out-dev out-dev)))

  (for ([v      (in-list inputs)]
        [in-dev (in-list ios)])
    (io-queue-enqueue! in-dev v))
  (io-queue-enqueue! amp-in-dev 0)

  (multimachine-run! machines)
  amp-out-dev)

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
      (make-intcode-machine n (memory-copy mem) #:in-dev in-dev #:out-dev out-dev)))

  ;; setup initial inputs
  (for ([v      (in-list inputs)]
        [in-dev (in-list ios)])
    (io-queue-enqueue! in-dev v))
  (io-queue-enqueue! amp-in-dev 0)

  (multimachine-run! machines)
  amp-out-dev)