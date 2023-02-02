(define-library (effect)
  (export
    make-operation
    with-handler
    with-handlers)
  (import
    (scheme base)
    (srfi 226 prompt)
    (srfi 226 continuation))

  (begin
    (define prompt-tag (make-continuation-prompt-tag 'effect))

    (define-record-type <effect-type>
      (%make-effect-type)
      effect-type?
      (name effect-type-name effect-type-set-name!))

    (define (make-operation)
      (define op
        (lambda arg*
          (call-with-composable-continuation
           (lambda (k)
             (apply abort-current-continuation prompt-tag op k arg*))
           prompt-tag)))
      op)

    (define-syntax with-handlers
      (syntax-rules ()
        ((with-handlers (((type k . arg*) e1 ... e2) ...) body1 ... body2)
         (call-with-handlers (list (cons type (lambda (k . arg*) e1 ... e2)) ...) (lambda () body1 ... body2)))))

    (define-syntax with-handler
      (syntax-rules ()
        ((with-handler ((type k . arg*) e1 ... e2) body1 ... body2)
         (with-handlers (((type k . arg*) e1 ... e2)) body1 ... body2))))

    (define (call-with-handlers clause* thunk)
      (call-with-continuation-prompt
       thunk
       prompt-tag
       (lambda (type k1 . arg*)
         (cond
          ((assq type clause*)
           => (lambda (clause)
                (apply (cdr clause) k1 arg*)))
          (else
           (call-with-composable-continuation
            (lambda (k2)
              (apply abort-current-continuation prompt-tag type (compose k2 k1) arg*))
            prompt-tag))))))

    (define compose
      (lambda (k2 k1)
        (lambda arg*
          (let-values ((arg* (apply k1 arg*)))
            (apply k2 arg*))))))

  )

;; Local Variables:
;; mode: scheme
;; End:
