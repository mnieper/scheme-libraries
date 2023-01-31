(import (scheme base)
        (scheme write)
        (protector)
        (chibi test))

(test-begin "protector")

(define prot (make-protector))

(define var1 (protector-make-parameter prot 1))

(test 1 (var1))

(define var2 (protector-make-parameter prot 2))

(test 2 (var2))

(var1 3)

(test 3 (var1))

(test 2 (var2))

(test 3 (with-protector prot
          (lambda ()
            (var1))))

(test 4 (with-protector prot
          (lambda ()
            (var1 4)
            (var1))))

(test 3 (var1))

(test '(7 5)
      (with-protector prot
        (lambda ()
          (var1 5)
          (list
           (with-protector prot
             (lambda ()
               (var1 (+ (var1) 2))
               (var1)))
           (var1)))))

(test-end "protector")
