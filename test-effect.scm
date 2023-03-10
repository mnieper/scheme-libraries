(import (scheme base)
        (effect)
        (chibi test))

(test-begin "effect")

(define (fail . arg*)
  (error "no handler available"))

(define t1 (make-operation fail))
(define t2 (make-operation fail))

(test 1 (with (handler)
          1))

(test 2 (with (handler
               ((t1 k) 2))
          (t1)
          1))

(test 3 (with (handler
                ((t1 k) 3))
          (with (handler
                  ((t2 k) 4))
            (t1)
            5)))

(test 4 (with (handler
                ((t1 k) 3))
          (with (handler
                  ((t2 k) 4))
            (t2)
            5)))

(test 10 (with (handler
                 ((t1 k x)
                  (k x)))
           (with (handler
                   ((t2 k) 9))
             (t1 10))))

(test 11 (with (handler
                ((t1 k x) (+ x 1))
                ((t2 k x) (+ x 2)))
           (t2 9)))

(define get (make-operation fail))
(define put (make-operation fail))

(define state
  (handler
    ((get k)
     (lambda (s)
       ((k s) 12)))
    ((put k x)
     (lambda (_)
       ((k (if #f #f)) x)))
    ((else . arg*)
     (lambda (s)
       (apply values arg*)))))

(test 10
      ((with state
         10)
       0))

(test 11
      ((with state
         (get))
       11))

(test 12
      ((with state
         (put (+ 1 (get)))
         (get))
       11))

(define (collector x x!)
  (let f ((ls '()))
    (handler*
      ((x k)
       (with (f ls)
         (k ls)))
      ((x! k n)
       (with (f (cons n ls))
         (k (if #f #f)))))))

(define collect! (make-operation fail))
(define get-collection (make-operation fail))

(test '(2 1)
      (with (collector get-collection collect!)
        (collect! 1)
        (collect! 2)
        (get-collection)))

(define count 0)
(define-operation (add! n)
  (set! count (+ count n)))

(test '3
      (begin
        (add! 3)
        count))

(test '(4 3)
      (let ((c 0))
        (with
            (handler
              ((add! k n)
               (set! c (+ c n))
               (k)))
          (add! 4)
          (list c count))))

(test-end "effect")
