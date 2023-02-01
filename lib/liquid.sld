(define-library (liquid)
  (export
    define-liquid
    let-liquid
    liquid-let)
  (import
    (scheme base)
    (scheme case-lambda)
    (srfi 211 identifier-syntax)
    (srfi 226 prompt)
    (srfi 226 continuation))
  (begin

    (define-record-type <liquid>
      (make-liquid tag default)
      liquid?
      (tag liquid-tag)
      (default liquid-default liquid-set-default!))

    (define *sentinel* (list 'liquid))

    (define-syntax define-liquid
      (syntax-rules ()
        ((define-liquid id expr)
         (begin
           (define liquid (make-liquid (make-continuation-prompt-tag 'liquid) expr))
           (define-syntax id
             (identifier-syntax
              (i (liquid-ref liquid))
              ((set! i e)
               (let ((tmp e))
                 (if (eq? tmp *sentinel*)
                     liquid
                     (liquid-set! liquid e))))))))))

    (define-syntax liquid
      (syntax-rules ()
        ((liquid id) (set! id *sentinel*))))

    (define (liquid-ref liquid)
      (define tag (liquid-tag liquid))
      (if (continuation-prompt-available? tag)
          (call-with-composable-continuation
           (lambda (k)
             (abort-current-continuation tag k))
           tag)
          (liquid-default liquid)))

    (define (liquid-set! liquid val)
      (define tag (liquid-tag liquid))
      (if (continuation-prompt-available? tag)
          (call-with-composable-continuation
           (lambda (k)
             (abort-current-continuation tag k val))
           tag)
          (liquid-set-default! liquid val)))

    (define-syntax liquid-let
      (syntax-rules ()
        ((liquid-let ((id expr) ...) body1 ... body2)
         (liquid-let-aux1 () ((id expr) ...) body1 ... body2))))

    (define-syntax liquid-let-aux1
      (syntax-rules ()
        ((_ ((id1 expr1 tmp1) ...) ((id2 expr2) bdg ...) . body)
         (liquid-let-aux1 ((id1 expr1 tmp1) ... (id2 expr2 tmp2)) (bdg ...) . body))
        ((_ ((id expr tmp) ...) () . body)
         (let ((tmp expr) ...)
           (liquid-let-aux2 ((id tmp) ...) . body)))))

    (define-syntax liquid-let-aux2
      (syntax-rules ()
        ((_ ((id1 tmp1) (id2 tmp2) ...) . body)
         (begin
           (define liq (liquid id1))
           (define tag (liquid-tag liq))
           (let f ((thunk (lambda () (liquid-let-aux2 ((id2 tmp2) ...) . body)))
                   (val tmp1))
             (call-with-continuation-prompt
              thunk
              tag
              (case-lambda
                ((k)
                 (f (lambda () (k val)) val))
                ((k new-val)
                 (f (lambda () (k (if #f #f))) new-val)))))))
        ((_ () . body)
         (begin . body))))

    (define-syntax let-liquid
      (syntax-rules ()
        ((_ ((id init) ...) body1 ... body2)
         (let* ()
           (define-liquid id (if #f #f)) ...
           (liquid-let ((id init) ...) body1 ... body2)))))

    ))

;; Local Variables:
;; mode: scheme
;; End:
