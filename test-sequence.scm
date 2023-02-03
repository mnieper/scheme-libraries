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
      (sequence->list coseq))

(test '(1 2 3)
      (sequence->list coseq))

(define (make-generator n)
  (define i 0)
  (lambda ()
    (if (= i n)
        (eof-object)
        (let ((res i))
          (set! i (+ i 1))
          res))))

(define gen (make-generator 3))
(define seq (generator->sequence gen))

(test '(0 1 2)
      (sequence->list seq))

(define (loop proc)
  (for-each proc '(1 2 3)))

(define loop-seq (loop->sequence loop))

(test #f (sequence-null? loop-seq))

(test '(1 2 3)
      (sequence->list loop-seq))

(test-error (sequence->list seq))

(test-end "sequence")
