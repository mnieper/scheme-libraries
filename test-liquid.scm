(import (scheme base)
        (liquid)
        (chibi test))

(test-begin "liquid")

(define-liquid liq1 1)

(test 1 liq1)

(define-liquid liq2 2)

(test 2 liq2)
(test 1 liq1)

(set! liq1 3)

(test 3 liq1)
(test 2 liq2)
(test 4 (liquid-let
            ((liq1 4)) liq1))
(test 3 liq1)
(test 9 (liquid-let ((liq1 4))
          (set! liq1 (+ liq1 5)) liq1))
(test 3 liq1)

(test '(7 5)
      (let-liquid ((liq 5))
        (list (liquid-let ((liq liq))
                (set! liq (+ liq 2)) liq)
              liq)))

(test '1
      (let-liquid ((liq 1))
        ((call/cc
           (lambda (c)
             (set! liq 2)
             (c (lambda () liq)))))))

(test '2
      (let ((liq 1))
        ((call/cc
           (lambda (c)
             (set! liq 2)
             (c (lambda () liq)))))))

(test-end "liquid")
