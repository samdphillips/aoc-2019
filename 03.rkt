#lang racket/base

(require racket/match
         racket/set
         racket/string)

#|

|#

(module+ test
  (require rackunit))

(struct posn (x y) #:transparent)
(struct step (direction distance) #:transparent)

(define (posn+ a b)
  (match-define (posn x1 y1) a)
  (match-define (posn x2 y2) b)
  (posn (+ x1 x2) (+ y1 y2)))

;; plot1 : Posn Posn Integer -> (Listof Posn)
(define (plot1 cur d count)
  (define (loop cur count acc)
    (cond
      [(zero? count) acc]
      [else
       (define new (posn+ cur d))
       (loop new (sub1 count) (cons new acc))]))
  (loop cur count null))

;; plot-path : (Listof PathStep) -> (Setof Posn)
(define (plot path)
  (for/fold ([cur (posn 0 0)]
             [cells (set)]
             #:result cells)
            ([a-step (in-list path)])
    (define d
      (match a-step
        [(step 'U _) (posn 0 1)]
        [(step 'D _) (posn 0 -1)]
        [(step 'L _) (posn -1 0)]
        [(step 'R _) (posn 1 0)]))
    (define new-cells (plot1 cur d (step-distance a-step)))
    (values (car new-cells) (set-union (list->set new-cells) cells))))

(module+ test
  (check-equal? (plot (list (step 'R 3)))
                (set (posn 1 0)
                     (posn 2 0)
                     (posn 3 0)))
  (check-equal? (plot (list (step 'U 1) (step 'R 1)))
                (set (posn 0 1) (posn 1 1)))
  (check-equal? (plot (list (step 'D 1)))
                (set (posn 0 -1)))
  (check-equal? (plot (list (step 'L 1)))
                (set (posn -1 0))))

(define (parse-path s)
  (for/list ([step-str (string-split s ",")])
    (step (string->symbol (substring step-str 0 1))
          (string->number (substring step-str 1)))))

(module+ test
  (check-equal? (parse-path "R8,U5,L5,D3")
                (list (step 'R 8)
                      (step 'U 5)
                      (step 'L 5)
                      (step 'D 3)))

  (check-equal? (parse-path "U7,R6,D4,L4")
                (list (step 'U 7)
                      (step 'R 6)
                      (step 'D 4)
                      (step 'L 4))))

(define path-crossings set-intersect)

(module+ test
  (check-equal? (path-crossings
                 (plot (parse-path "R8,U5,L5,D3"))
                 (plot (parse-path "U7,R6,D4,L4")))
                (set (posn 6 5) (posn 3 3))))

(define (find-closest crossings)
  (for/fold ([close #f]) ([cell (in-set crossings)])
    (define dist (+ (abs (posn-x cell)) (abs (posn-y cell))))
    (cond
      [(not close) dist]
      [(< dist close) dist]
      [else close])))

(define (solve-part-one path1 path2)
  (find-closest
   (path-crossings
    (plot (parse-path path1))
    (plot (parse-path path2)))))

(module+ test
  (check-equal? (solve-part-one "R8,U5,L5,D3"
                                "U7,R6,D4,L4")
                6)

  (check-equal? (solve-part-one "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                                "U62,R66,U55,R34,D71,R55,D58,R83")
                159)


  )

(module* part-one #f
  (call-with-input-file "inputs/03.txt"
    (lambda (input)
      (solve-part-one (read-line input)
                      (read-line input)))))


(module* part-two #f)
