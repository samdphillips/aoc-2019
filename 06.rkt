#lang racket/base

(require racket/match
         racket/port
         racket/set
         racket/stream
         racket/string
         graph
         memoize
         threading)

(module+ test
  (require rackunit))

(struct orbit (center satelite) #:transparent)

(struct orbital-map (orbits)
  #:transparent
  #:methods gen:graph
  [(define (in-vertices an-orbital-map)
     (for/fold ([v (seteq)])
               ([an-orbit (in-orbits an-orbital-map)])
       (set-add (set-add v (orbit-center an-orbit))
                (orbit-satelite an-orbit))))

   (define (in-neighbors an-orbital-map body)
     (define (iter orbits)
       (match orbits
         ['() empty-stream]
         [(cons (orbit (== body) satelite) rest)
          (stream-cons satelite (iter rest))]
         [(cons (orbit center (== body)) rest)
          (stream-cons center (iter rest))]
         [(cons _ rest) (iter rest)]))
     (iter (in-orbits an-orbital-map)))
   ])

(define (in-orbits an-orbital-map)
  (orbital-map-orbits an-orbital-map))

(define (read-orbit in)
  (define line (read-line in))
  (cond
    [(eof-object? line) line]
    [else
     (~>> (string-split line ")")
          (map string->symbol)
          (apply orbit))]))

(define (read-orbital-map in)
  (orbital-map (port->list read-orbit in)))

(module+ test
  (check-equal?
   (call-with-input-string "A)B" read-orbit)
   (orbit 'A 'B))

  (check-equal?
   (call-with-input-string "A12)B34\nA12)C56" read-orbital-map)
   (orbital-map
    (list (orbit 'A12 'B34)
          (orbit 'A12 'C56)))))


(define (orbit-count-checksum list-of-orbits)
  (define links
    (for/hash ([an-orbit (in-orbits list-of-orbits)])
      (values (orbit-satelite an-orbit)
              (orbit-center an-orbit))))

  (define/memo (count-orbits body)
    (cond
      [(eq? 'COM body) 0]
      [else (add1 (count-orbits (hash-ref links body)))]))

  (for/sum ([body (in-hash-keys links)])
    (count-orbits body)))

(module+ test
  (define test-input-map #<<INPUT
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
INPUT
    )
  (check-equal?
   (orbit-count-checksum
    (call-with-input-string test-input-map read-orbital-map))
   42))

(module* part-one #f
  (orbit-count-checksum
   (call-with-input-file "inputs/06.txt" read-orbital-map)))

(define (transfers-between an-orbital-map src dest)
  (define-values (step-table path-table)
    (dijkstra an-orbital-map src))
  (- (hash-ref step-table dest) 2))

(module+ test
  (define test-input-transfers #<<INPUT
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN
INPUT
    )
  (let ([orbital-map (call-with-input-string test-input-transfers
                                             read-orbital-map)])
    (check-equal? (transfers-between orbital-map 'YOU 'SAN)
                  4)))


(module* part-two #f
  (transfers-between (call-with-input-file "inputs/06.txt"
                       read-orbital-map)
                     'YOU
                     'SAN))

