(define-library (protection)
  (export
    make-protection
    protection-make-variable
    with-protection)
  (import
    (scheme base)
    (scheme case-lambda)
    (srfi 226 prompt)
    (srfi 226 continuation))
  (begin

    (define-record-type <protection>
      (%make-protection prompt-tag)
      protection?
      (prompt-tag protection-prompt-tag))

    (define (make-protection)
      (%make-protection (make-continuation-prompt-tag)))

    (define protection-make-variable
      (case-lambda
        ((protection default)
         (protection-make-variable protection default values))
        ((protection default filter)
         (define prompt-tag (protection-prompt-tag protection))
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

    (define (with-protection protection thunk)
      (define prompt-tag (protection-prompt-tag protection))
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
