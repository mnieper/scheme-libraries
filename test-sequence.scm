(import (scheme base)
        (sequence)
        (liquid)
        (chibi test))

(test-begin "sequence")

(define seq (list->sequence '(1 2 3)))

(test 6 (sequence-fold + 0 seq))
(test 6 (let-liquid ((c 0))
          (sequence-for-each
           (lambda (x)
             (set! c (+ x c)))
           seq)
          c))

(test 3 (sequence-select
         (list->sequence '())
         (lambda arg* 4)
         (lambda () 3)))

(test 1 (sequence-select
         seq
         (lambda (tail head)
           head)
         (lambda ()
           0)))

(test 5 (sequence-select
         seq
         (lambda (tail head)
           (sequence-fold + 0 tail))
         (lambda ()
           0)))

(test '(1 2 3)
      (sequence->list seq))

(define coseq
  (coroutine->sequence
   (lambda (yield)
     (yield 1)
     (yield 2)
     (yield 3))))

(test '(1 2 3)
      (sequence->list seq))

(test '(1 2 3)
      (sequence->list seq))

(test-end "sequence")
