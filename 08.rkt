#lang racket/base

#|

|#

(require racket/vector)

(module+ test
  (require racket/port
           rackunit))

(define width  25)
(define height 6)
(define zero (bytes-ref #"0" 0))

(define (read-image in)
  (for/vector ([b (in-port read-byte in)]
               #:when (>= b zero))
    (- b zero)))

(module+ test
  (check-equal?
   (call-with-input-string "123456789012" read-image)
   #(1 2 3 4 5 6 7 8 9 0 1 2)))

(define (split-layers img w h)
  (define img-size (vector-length img))
  (define layer-size (* w h))
  (for/list ([offset (in-range 0 img-size layer-size)])
    (vector-copy img offset (+ offset layer-size))))

(module+ test
  (check-equal?
   (let ([img (call-with-input-string "123456789012" read-image)])
     (split-layers img 3 2))
   (list (vector 1 2 3 4 5 6)
         (vector 7 8 9 0 1 2))))

#;
(let ([img (call-with-input-file "inputs/08.txt" read-image)])
  (let-syntax ([update-count
                (syntax-rules ()
                  [(_ update-var compare-var val)
                   (+ update-var (if (= val compare-var) 1 0))])])
    (for/list ([layer (split-layers img width height)])
      (for/fold ([z 0] [o 0] [t 0])
                ([v (in-vector layer)])
        (values (update-count z v 0)
                (update-count o v 1)
                (update-count t v 2))))))


(module* part-one #f
  (require racket/match)

  (define img (call-with-input-file "inputs/08.txt" read-image))

  (define-syntax-rule (update-count u c v)
    (+ u (if (= v c) 1 0)))
  (define layers (split-layers img width height))
  (define layer-stats
    (for/list ([layer (in-list layers)])
      (for/fold ([z 0] [o 0] [t 0] #:result (list z o t))
                ([v (in-vector layer)])
        (values (update-count z v 0)
                (update-count o v 1)
                (update-count t v 2)))))

  (match-define (list _ o t)
    (car (sort layer-stats < #:key car)))
  (* o t))

(module* part-two #f
  (define img (call-with-input-file "inputs/08.txt" read-image))

  (define layers (split-layers img width height))

  (define composite-image
    (for/vector #:length (* width height) ([i (in-range
                                               (* width height))])
      (for/or ([layer layers])
        (define pixel (vector-ref layer i))
        (if (= pixel 2) #f pixel))))

  ;; display
  (for ([r (in-range height)])
    (for ([c (in-range width)])
      (define pixel
        (vector-ref composite-image (+ c (* r width))))
      (display
       (if (= 0 pixel) "#" ".")))
    (newline)))



