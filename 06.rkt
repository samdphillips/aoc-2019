#lang racket/base

(require racket/port
         racket/string
         memoize
         threading)

(module+ test
  (require rackunit))

(struct orbit (center satelite) #:transparent)

(define (read-orbit in)
  (define line (read-line in))
  (cond
    [(eof-object? line) line]
    [else
     (~>> (string-split line ")")
          (map string->symbol)
          (apply orbit))]))

(define (read-all-orbits in)
  (port->list read-orbit in))

(module+ test
  (check-equal?
   (call-with-input-string "A)B" read-orbit)
   (orbit 'A 'B))

  (check-equal?
   (call-with-input-string "A12)B34\nA12)C56" read-all-orbits)
   (list (orbit 'A12 'B34)
         (orbit 'A12 'C56))))


(define (orbit-count-checksum list-of-orbits)
  (define links
    (for/hash ([an-orbit (in-list list-of-orbits)])
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
    (call-with-input-string test-input-map read-all-orbits))
   42))

(module* part-one #f
  (orbit-count-checksum
   (call-with-input-file "inputs/06.txt" read-all-orbits)))


(module* part-two #f)
