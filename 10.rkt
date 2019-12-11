#lang racket/base

#|

dy/dx * x + b = y
dy * x + dx * b = dx * y

dx * b = dx * y - dy * x
-dx * b = dy * x - dx * y
0 = dy * x - dx * y + dx * b

dx * b = dx * y - dy * x
b = (dx * y - dy * x) / dx
b = y - dy / dx * x

|#

(require racket/generator
         racket/match
         racket/sequence
         racket/set)

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
                 (call-with-input-string asteroid-map read-asteroid-map))
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