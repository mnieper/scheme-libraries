(define-library (protector)
  (export
    make-protector
    protector-make-parameter
    with-protector)
  (import
    (scheme base)
    (scheme case-lambda)
    (srfi 226 prompt)
    (srfi 226 continuation))
  (begin

    (define-record-type <protector>
      (%make-protector prompt-tag)
      protector?
      (prompt-tag protector-prompt-tag))

    (define (make-protector)
      (%make-protector (make-continuation-prompt-tag)))

    (define protector-make-parameter
      (case-lambda
        ((protector default)
         (protector-make-parameter protector default values))
        ((protector default filter)
         (define prompt-tag (protector-prompt-tag protector))
         (define parameter
           (case-lambda
             (()
              (if (continuation-prompt-available? prompt-tag)
                  (call-with-composable-continuation
                   (lambda (k)
                     (abort-current-continuation prompt-tag
                       k parameter))
                   prompt-tag)
                  default))
             ((val)
              (if (continuation-prompt-available? prompt-tag)
                  (call-with-composable-continuation
                   (lambda (k)
                     (abort-current-continuation prompt-tag
                       k parameter (filter val)))
                   prompt-tag)
                  (set! default (filter val))))))
         parameter)))

    (define (with-protector protector thunk)
      (define prompt-tag (protector-prompt-tag protector))
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

    ))

;; Local Variables:
;; mode: scheme
;; End:
