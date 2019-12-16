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
  (require racket/port
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
                (define angle
                  (let ([r (- (atan (- ndy) ndx) (/ pi 2))])
                    (if (< r 0) (+ r (* 2 pi)) r)))
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
             #;into-list
             (reducer-map (into-nth 201)
                          #:range
                          (compose1 relative-asteroid-position
                                    present-value))))
