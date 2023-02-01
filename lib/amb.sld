(define-library (amb)
  (export
    with-amb)
  (import (scheme base)
          (liquid))
  (begin

    (define-syntax with-amb
      (syntax-rules ()
        ((with-amb amb body1 ... body2)
         (let-liquid ((fail (lambda () (error "amb: failure"))))
           (define-syntax amb
             (syntax-rules ()
               ((amb) (fail))
               ((amb e1 e2 (... ...))
                ((call/cc
                  (lambda (k)
                    (set! fail (lambda ()
                                 (k (lambda () (amb e2 (... ...))))))
                    (let-values ((arg* e1))
                      (lambda ()
                        (apply values arg*)))))))))
           body1 ... body2)))))

  )

;; Local Variables:
;; mode: scheme
;; End:
