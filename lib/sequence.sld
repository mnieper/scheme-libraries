(define-library (sequence)
  (export
    sequence?
    ;; TODO: sequence-null
    list->sequence
    sequence->list
    sequence-select
    sequence-for-each
    sequence-fold
    sequence-case)
  (import
    (scheme base)
    (srfi 226 prompt)
    (srfi 226 continuation)
    (liquid))

  (begin
    (define-record-type <sequence>
      (make-sequence enumerator)
      sequence?
      (enumerator sequence-enumerator))

    (define-syntax sequence-case
      (syntax-rules ()
        ((sequence-case expr
           ((head . tail) e1 ... e2)
           (() e3 ... e4))
         (sequence-select expr
                          (lambda (tail . head) e1 ... e2)
                          (lambda () e3 ... e4)))))

    (define list->sequence
      (lambda (ls)
        (make-sequence
         (lambda (proc seed)
           (let f ((ls ls)
                   (seed seed))
             (if (null? ls)
                 seed
                 (f (cdr ls) (proc seed (car ls)))))))))

    (define sequence-fold
      (lambda (proc seed seq)
        ((sequence-enumerator seq) proc seed)))

    (define sequence-fold*
      (lambda (proc seed seq)
        (cadr
         (sequence-fold
          (lambda (seed* . el*)
            (define-values seed*
              (apply (car seed*) (cadr seed*) el*))
            seed*)
          (list proc seed) seq))))

    (define sequence-for-each
      (lambda (proc seq)
        (sequence-fold
         (lambda (seed . el*)
           (apply proc el*)
           (if #f #f))
         (if #f #f) seq)))

    (define sequence-select
      (lambda (seq success fail)
        (define tag (make-continuation-prompt-tag 'select))
        ((call-with-continuation-prompt
          (lambda ()
            (sequence-fold*
             (lambda (acc . el*)
               (call-with-composable-continuation
                (lambda (k)
                  (abort-current-continuation tag
                    k el*))
                tag))
             fail
             seq))
          tag
          (lambda (k el*)
            (lambda ()
              (apply success
                     (make-sequence
                      (lambda (proc seed)
                        (define (f acc . el*)
                          (values f (apply proc acc el*)))
                        (k f seed)))
                     el*)))))))

    (define sequence->list
      (lambda (seq)
        (sequence-case seq
          (((head) . tail)
           (cons head (sequence->list tail)))
          (() '()))))

    ))

;; Local Variables:
;; mode: scheme
;; End:
