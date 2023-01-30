(define-library (environment)
  (export
    make-environment
    environment-make-variable
    with-environment)
  (import
    (scheme base)
    (scheme case-lambda)
    (srfi 226 prompt)
    (srfi 226 continuation)
    (srfi 226 shift-reset))
  (begin

    (define-record-type <environment>
      (%make-environment prompt-tag)
      environment?
      (prompt-tag environment-prompt-tag))

    (define (make-environment)
      (%make-environment (make-continuation-prompt-tag)))

    (define environment-make-variable
      (case-lambda
        ((environment default)
         (environment-make-variable environment default values))
        ((environment default filter)
         (define prompt-tag (environment-prompt-tag environment))
         (define variable
           (case-lambda
             (()
              (if (continuation-prompt-available? prompt-tag)
                  (call-with-composable-continuation
                   (lambda (k)
                     (abort-current-continuation prompt-tag
                       k variable))
                   prompt-tag)
                  default))
             ((val)
              (if (continuation-prompt-available? prompt-tag)
                  (call-with-composable-continuation
                   (lambda (k)
                     (abort-current-continuation prompt-tag
                       k variable (filter val)))
                   prompt-tag)
                  (set! default (filter val))))))
         variable)))

    (define (with-environment environment thunk)
      (define prompt-tag (environment-prompt-tag environment))
      (let f ((thunk thunk) (state '()))
        (call-with-continuation-prompt
         thunk
         prompt-tag
         (case-lambda
           ((k var)
            (let ((val (cond
                        ((assq var state) => cdr)
                        (else (var)))))
              (f (lambda () (k val)) state)))
           ((k var val)
            (f (lambda () (k (if #f #f)))
               (let g ((state state))
                 (cond
                  ((null? state)
                   (list (cons var val)))
                  ((eq? var (caar state))
                   (cons (cons var val) (cdr state)))
                  (else
                   (cons (car state) (g (cdr state))))))))))))



    ;; Local Variables:
    ;; mode: scheme
    ;; End:
    ))
