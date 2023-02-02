(define-library (sequence)
  (export
    sequence?
    sequence-null
    list->sequence
    coroutine->sequence
    sequence->list
    sequence-select
    sequence-for-each
    sequence-fold
    sequence-case)
  (import
    (scheme base)
    (srfi 226 prompt)
    (srfi 226 continuation)
    (effect)
    (liquid))

  (begin

    ;; Exported record types

    (define-record-type <sequence>
      (make-sequence descriptor data)
      sequence?
      (descriptor sequence-type-descriptor)
      (data sequence-data))

    ;; Helper record types

    (define-record-type <sequence-type-descriptor>
      (make-sequence-type-descriptor select for-each fold ->list)
      sequence-type-descriptor?
      (select sequence-type-descriptor-select-procedure)
      (for-each sequence-type-descriptor-for-each-procedure)
      (fold sequence-type-descriptor-fold-procedure)
      (->list sequence-type-descriptor-->list-procedure))

    ;; Exported syntax

    (define-syntax sequence-case
      (syntax-rules ()
        ((sequence-case expr
           ((tail . head) e1 ... e2)
           (() e3 ... e4))
         (sequence-select expr
                          (lambda (tail . head) e1 ... e2)
                          (lambda () e3 ... e4)))))

    ;; List sequence type

    (define (list-select data success failure)
      (if (null? data)
          (failure)
          (success (list->sequence (cdr data)) (car data))))

    (define (list-for-each data proc)
      (for-each proc data))

    (define (list-fold data proc nil)
      (let loop ((nil nil) (data data))
        (if (null? data)
            nil
            (loop (proc nil
                        (car data))
                  (cdr data)))))

    (define (list->list data)
      data)

    (define list-type-descriptor
      (make-sequence-type-descriptor list-select
                                     list-for-each
                                     list-fold
                                     list->list))

    (define (list->sequence ls)
      (make-sequence list-type-descriptor ls))

    ;; Coroutine sequence type

    (define (coroutine-select select success failure)
      (select
       (lambda (tail . head*)
         (apply success
                (make-sequence coroutine-type-descriptor tail)
                head*))
       failure))

    (define (coroutine-for-each select proc)
      (let loop ((select select))
        (select
         (lambda (select . val*)
           (apply proc val*)
           (loop select))
         (lambda ()
           (if #f #f)))))

    (define (coroutine-fold select proc nil)
      (let loop ((select select) (nil nil))
        (select
         (lambda (select . val*)
           (loop select
                 (apply proc nil val*)))
         (lambda ()
           nil))))

    (define (coroutine->list select)
      (let loop ((select select))
        (select
         (lambda (select val)
           (cons val (loop select)))
         (lambda ()
           '()))))

    (define coroutine-type-descriptor
      (make-sequence-type-descriptor coroutine-select
                                     coroutine-for-each
                                     coroutine-fold
                                     coroutine->list))

    (define (coroutine->sequence co)
      (define produce (make-operation))
      (define stop (make-operation))
      (define (yield . val*)
        (apply produce val*))
      (define select
        (let loop ((thunk (lambda () (co yield))))
          (lambda (success fail)
            (with-handlers
                (((produce k . val*)
                  (apply success
                         (loop k)
                         val*))
                 ((stop k)
                  (fail)))
              (thunk)))))
      (make-sequence coroutine->sequence select))

    ;; Constructors

    (define sequence-null
      (make-sequence list-type-descriptor '()))

    ;; Predicates

    (define (sequence-null? seq)
      (sequence-case seq
        ((tail . head) #f)
        (() #t)))

    ;; Procedures

    (define (sequence-select seq success fail)
      (define std (sequence-type-descriptor seq))
      (define data (sequence-data seq))
      ((sequence-type-descriptor-select-procedure std) data success fail))

    (define (sequence-for-each proc seq)
      (define std (sequence-type-descriptor seq))
      (define data (sequence-data seq))
      ((sequence-type-descriptor-for-each-procedure std) data proc))

    (define (sequence-fold proc nil seq)
      (define std (sequence-type-descriptor seq))
      (define data (sequence-data seq))
      ((sequence-type-descriptor-fold-procedure std) data proc nil))

    (define (sequence->list seq)
      (define std (sequence-type-descriptor seq))
      (define data (sequence-data seq))
      ((sequence-type-descriptor-->list-procedure std) data))

    ;; Conversion procedures

    (define sequence->list
      (lambda (seq)
        (sequence-case seq
          ((tail head)
           (cons head (sequence->list tail)))
          (() '()))))))

;; Local Variables:
;; mode: scheme
;; End:
