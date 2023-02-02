(import (scheme base)
        (effect)                  (scheme write)
        (chibi test))

(test-begin "effect")

(define t1 (make-operation))
(define t2 (make-operation))

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

(define get (make-operation))
(define put (make-operation))

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

(test-end "effect")
