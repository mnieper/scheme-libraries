(define-library (sequence)
  (export
    sequence?
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
    (srfi 226 shift-reset)
    (liquid))

  (define-record-type <sequence>
    (make-sequence enumerator)
    sequence?
    (enumerator sequence-enumerator))

  (define-syntax sequence-case
    (syntax-rules ()
      ((sequence-case expr
         ((head . tail) e1 ... e2)
        (() e3 ... e4))
       (sequence-select (lambda (tail . head) e1 ... e2)
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
      ((sequence-enumerator proc seed))))

  (define sequence-for-each
    (lambda (proc seq)
      (sequence-fold
       (lambda (seed . el*)
         (apply proc el*)
         (if #f #f))
       (if #f #f) seq)))

  #;
  (define run-with-state
    (lambda (proc seed)
      (define tag (make-continuation-prompt-tag))
      (let f ((val seed)
              (k (lambda ()
                   (reset-at
                       tag
                     (call-with-values
                         (lambda ()
                           (proc (lambda ()
                                   (shift-at
                                       tag
                                       k (lambda (g p f)
                                           (g k))))
                                 (lambda (v)
                                   (shift-at
                                       tag k (lambda (g p f)
                                               (p k v))))))
                       (lambda args
                         (lambda (g p f)
                           (apply f args))))))))
        ((k)
         (lambda (k)
           (f val (lambda () (k val))))
         (lambda (k new-val)
           (f new-val k))
         values))))


  (define sequence-select
    (lambda (seq success fail)
      (define tag (make-continuation-prompt-tag 'select))
      (let f (
              (k (lambda ()
                   (reset-at tag
                     (lambda ()
                       (sequence-fold
                        (lambda (ACC . el*)

                          )
                        SEED seq)

                       )
                     ))))
        ((k)
         ...
         ...
         )


        ((k)
         (lambda (a b c)
           )))
      (call-with-continuation-prompt
       thunk
       tag
       (case-lambda
         (()
          ...))


       (reset-at tag
         (sequence-fold
          (lambda (acc . el*)
            (shift-at tag k
              (lambda ()
                (apply success
                       (make-sequence
                        (lambda (proc seed)
                          (k seed )
                          ))
                       el*))))
          fail seq)))))

  (define sequence->list
    (lambda (seq)
      (sequence-case seq
        (((head) . tail)
         (cons head (sequence->list tail)))
        (() '()))))

  )

;; Local Variables:
;; mode: scheme
;; End:
