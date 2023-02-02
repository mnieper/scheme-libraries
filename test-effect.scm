(import (scheme base)
        (effect)
        (chibi test))

(test-begin "effect")

(define t1 (make-operation))
(define t2 (make-operation))

(test 1 (with-handlers ()
          1))

(test 2 (with-handler
            ((t1 k) 2)
          (t1)
          1))

(test 3 (with-handler
            ((t1 k) 3)
          (with-handler
              ((t2 k) 4)
              (t1)
            5)))

(test 4 (with-handler
            ((t1 k) 3)
          (with-handler
              ((t2 k) 4)
            (t2)
            5)))

(test 10 (with-handler
             ((t1 k x) (k x))
           (with-handler
               ((t2 k) 9)
             (t1 10))))

(test 11 (with-handlers
             (((t1 k x) (+ x 1))
              ((t2 k x) (+ x 2)))
           (t2 9)))

(test-end "effect")
