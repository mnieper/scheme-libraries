(define-library (effect)
  (export
    make-operation
    handler
    with)
  (import
    (scheme base)
    (srfi 226 prompt)
    (srfi 226 continuation))

  (begin
    (define prompt-tag (make-continuation-prompt-tag 'effect))

    (define-record-type <handler>
      (%make-handler procedure)
      handler?
      (procedure handler-procedure))

    (define (make-operation)
      (define (op . arg*)
        (call-with-composable-continuation
         (lambda (k)
           (abort-current-continuation prompt-tag
             op k arg*))
         prompt-tag))
      op)

    (define (return . arg*)
      (abort-current-continuation prompt-tag
        return (if #f #f) arg*))

    (define-syntax handler
      (syntax-rules (else)
        ((handler ((op k1 . arg1*) e1 ... e2) ... ((else . arg2*) e3 ... e4))
         (make-handler (list (cons op (lambda (k1 . arg1*) e1 ... e2)) ...
                             (cons return (lambda (k2 . arg2*) e3 ... e4)))))
        ((handler ((op k . arg1*) e1 ... e2) ...)
         (handler ((op k . arg1*) e1 ... e2) ...
                  ((else . arg2*)
                   (apply values arg2*))))))

    (define-syntax with
      (syntax-rules ()
        ((with handler body1 ... body2)
         (call-with-handler handler (lambda () body1 ... body2)))))

    (define (make-handler clause*)
      (%make-handler
       (lambda (loop op shallow-k arg*)
         (define (k . val*)
           (loop (lambda () (apply shallow-k val*))))
         (cond
          ((assq op clause*)
           => (lambda (clause)
                (apply (cdr clause) k arg*)))
          (else
           (call-with-values
               (lambda ()
                 (apply op arg*))
             k))))))

    (define (call-with-handler handler thunk)
      (define proc (handler-procedure handler))
      (let loop ((thunk thunk))
        (call-with-continuation-prompt
         (lambda ()
           (call-with-values thunk return))
         prompt-tag
         (lambda (op k arg*)
           (proc
            loop
            op
            k
            arg*))))))

  )

;; Local Variables:
;; mode: scheme
;; End:
