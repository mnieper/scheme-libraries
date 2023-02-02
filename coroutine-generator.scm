(import (scheme base)
        (scheme write)
        (effect))

(define (make-coroutine-generator proc)
  (define yield (make-operation))
  (define (thunk)
    (with (handler
            ((yield k val)
             (set! thunk k)
             val)
            ((else . ignore*)
             (eof-object)))
      (proc yield)))
  (define (gen)
    (thunk))
  gen)

(define g
  (make-coroutine-generator
   (lambda (yield)
     (yield 1)
     (yield 2)
     (yield 3))))

(display (g))
(newline)
(display (g))
(newline)
(display (g))
(newline)
(display (eof-object? (g)))
(newline)
