(import (scheme base)
        (scheme write)
        (environment)
        (chibi test))

(test-begin "environment")

(define env (make-environment))

(define var1 (environment-make-variable env 1))

(test 1 (var1))

(define var2 (environment-make-variable env 2))

(test 2 (var2))

(var1 3)

(test 3 (var1))

(test 2 (var2))

(test 3 (with-environment env
          (lambda ()
            (var1))))

(test 4 (with-environment env
          (lambda ()
            (var1 4)
            (var1))))

(test 3 (var1))

(test '(7 5)
      (with-environment env
        (lambda ()
          (var1 5)
          (list
           (with-environment env
             (lambda ()
               (var1 (+ (var1) 2))
               (var1)))
           (var1)))))

(test-end "environment")
