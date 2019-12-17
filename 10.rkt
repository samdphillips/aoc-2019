#lang racket/base

(require racket/generator
         racket/match
         racket/math
         racket/sequence
         racket/set

         rebellion/base/comparator
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/list
         rebellion/streaming/reducer
         rebellion/streaming/transducer)

(module+ test
  (require racket/format
           racket/port
           syntax/parse/define
           rackunit))


(struct posn (x y) #:transparent)

#|

Previous solution calculated unique rays in a space.  What if
the solution just calculates the deltas from the origin, since
the comparison is only with a single origin each time.

It should be mostly the same.  Just removes a bit of math.

|#

(define (delta-normed p0 p1)
  (match-define (posn x0 y0) p0)
  (match-define (posn x1 y1) p1)
  (define dx (- x1 x0))
  (define dy (- y1 y0))
  (define m (gcd dx dy))
  (list (/ dx m) (/ dy m)))

(module+ test
  (check-equal? (delta-normed (posn 3 3) (posn 2 2))
                (delta-normed (posn 3 3) (posn 1 1)))
  (check-not-equal? (delta-normed (posn 3 3) (posn 2 2))
                    (delta-normed (posn 3 3) (posn 2 1)))

  (check-equal? (delta-normed (posn 0 0) (posn 3 2))
                (delta-normed (posn 0 0) (posn 6 4)))

  (check-equal? (delta-normed (posn 0 0) (posn 0 1))
                (delta-normed (posn 0 0) (posn 0 2)))

  (check-equal? (delta-normed (posn 0 0) (posn 1 0))
                (delta-normed (posn 0 0) (posn 2 0)))

  (check-not-equal? (delta-normed (posn 5 5) (posn 10 10))
                    (delta-normed (posn 5 5) (posn 1 1))))


(define (solve-part-one asteroids)
  (for/fold ([m #f]) ([origin (in-list asteroids)])
    (define visible
      (set-count
       (for/set ([dest   (in-list asteroids)]
                 #:unless (equal? origin dest))
         (delta-normed origin dest))))
    (if m (max m visible) visible)))

(module+ test
  #|
  01234
0 .#..#
1 .....
2 #####
3 ....#
4 ...##

|#

  (define asteroids
    (list (posn 0 2)
          (posn 1 0)
          (posn 1 2)
          (posn 2 2)
          (posn 3 2)
          (posn 3 4)
          (posn 4 0)
          (posn 4 2)
          (posn 4 3)
          (posn 4 4)))
  (check-equal? (solve-part-one asteroids) 8))

(define (read-asteroid-map in)
  (sequence->list
   (in-generator
    (for ([line (in-port read-line in)]
          [y    (in-naturals)])
      (for ([c (in-string line)]
            [x (in-naturals)])
        (when (char=? #\# c)
          (yield (posn x y))))))))


(module+ test
  (define asteroid-map #<<MAP
.#..#
.....
#####
....#
...##
MAP
    )

  (check-equal? (list->set
                 (call-with-input-string
                  asteroid-map read-asteroid-map))
                (list->set asteroids))

  (check-equal?
   (call-with-input-file "test-inputs/10_1_01.txt"
     (lambda (in)
       (solve-part-one (read-asteroid-map in))))
   33)

  (check-equal?
   (call-with-input-file "test-inputs/10_1_02.txt"
     (lambda (in)
       (solve-part-one (read-asteroid-map in))))
   35))


(module* part-one #f
  (call-with-input-file "inputs/10.txt"
    (lambda (in)
      (solve-part-one (read-asteroid-map in)))))

(define (find-asteroid-base asteroids)
  (for/fold ([m #f] [asteroid #f] #:result asteroid)
            ([origin (in-list asteroids)])
    (define visible
      (set-count
       (for/set ([dest   (in-list asteroids)]
                 #:unless (equal? origin dest))
         (delta-normed origin dest))))
    (cond
      [(not m)       (values visible origin)]
      [(< m visible) (values visible origin)]
      [else          (values m asteroid)])))

(struct relative-asteroid (angle distance position) #:transparent)

(define (calculate-angle dx dy)
  (define tx (* dy -1))
  (define ty (* dx -1))
  (define r (atan ty tx))
  (if (<= r 0)
      (abs r)
      (- (* 2 pi) r)))

(module+ test
  (define-simple-macro (test-angle x y v)
    (test-equal? (~a "(" x ", " y ")")
                 (calculate-angle x y) v))

  (test-angle 0 -1 0)
  (test-angle 1 0 (/ pi 2))
  (test-angle 0 1 pi)
  (test-angle -1 0 (+ pi (/ pi 2))))

(define (calculate-laser-sequence base asteroids)
  (match-define (posn x0 y0) base)
  (transduce asteroids
             (filtering
              (lambda (a-posn)
                (not (equal? a-posn base))))
             (mapping
              (lambda (a-posn)
                (match-define (posn x1 y1) a-posn)
                (define dx (- x1 x0))
                (define dy (- y1 y0))
                (define distance (sqrt (+ (* dx dx) (* dy dy))))
                (match-define (list ndx ndy) (delta-normed base a-posn))
                (define angle (calculate-angle ndx ndy))
                (relative-asteroid angle distance a-posn)))
             (bisecting relative-asteroid-angle values)
             (grouping (into-transduced
                        (sorting real<=>
                                 #:key relative-asteroid-distance)
                        #:into into-list))
             (append-mapping
              (lambda (an-entry)
                (define angle (entry-key an-entry))
                (match (entry-value an-entry)
                  [(and (list a) v) v]
                  [(list a ...)
                   (for/list ([an-asteroid (in-list a)]
                              [n (in-naturals)])
                     (struct-copy relative-asteroid
                                  an-asteroid
                                  [angle (+ (* n 2 pi) angle)]))])))

             (sorting real<=> #:key relative-asteroid-angle)
             #:into
             into-list))

(module+ test
  (test-case "small laser sequence"
    (define base (posn 2 2))
    (define asteroids
      (list (posn 1 2) (posn 3 2) (posn 2 1) (posn 2 3)
            (posn 2 0) (posn 4 2) (posn 2 3) (posn 1 2)))
    (define asteroid-sequence
      (map relative-asteroid-position
           (calculate-laser-sequence base asteroids)))
    (check-equal? asteroid-sequence
                  (list (posn 2 1)
                        (posn 3 2)
                        (posn 2 3)
                        (posn 1 2)
                        (posn 2 0)
                        (posn 4 2)
                        (posn 2 3)
                        (posn 1 2)))))


(define (solve-part-two asteroids n)
  (define base (find-asteroid-base asteroids))
  (reduce-all (reducer-map (into-nth (sub1 n))
                           #:range
                           (compose1 relative-asteroid-position
                                     present-value))
              (in-list (calculate-laser-sequence base asteroids))))

(module+ test
  (let ()
    (define asteroids
      (call-with-input-file "test-inputs/10_1_03.txt" read-asteroid-map))
    (define laser-sequence
      (calculate-laser-sequence (posn 11 13) asteroids))
    (define-simple-macro (test-sequence n p)
      (test-equal? (~a "step " n)
                   (reduce-all
                     (reducer-map (into-nth (sub1 n))
                                  #:range
                                  (compose1 relative-asteroid-position
                                            present-value))
                     laser-sequence)
                   p))

    (test-sequence   1 (posn 11 12))
    (test-sequence   2 (posn 12  1))
    (test-sequence   3 (posn 12  2))
    (test-sequence  10 (posn 12  8))
    (test-sequence  20 (posn 16  0))
    (test-sequence  50 (posn 16  9))
    (test-sequence 100 (posn 10 16))
    (test-sequence 199 (posn  9  6))
    (test-sequence 200 (posn  8  2))
    (test-sequence 201 (posn 10  9))
    (test-sequence 299 (posn 11  1))))

(module* part-two #f
  (require threading)
  (call-with-input-file "inputs/10.txt"
    (lambda~> read-asteroid-map
              (solve-part-two 200))))
