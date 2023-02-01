(import (scheme base)
        (amb)
        (chibi test))

(test-begin "amb")

(test-error (with-amb amb
              (amb)))

(test 1 (with-amb amb
          (amb 1)))

(test 2
      (with-amb amb
        (let ((x (amb 1 2)))
          (when (eqv? x 1)
            (amb))
          x)))

(test-end "amb")
