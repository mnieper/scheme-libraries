# Scheme Libraries

This repository serves as a Scheme library incubator.

## Effects

The effect library implements algebraic effect handlers, a contemporary view on delimited continuations.

### Library name

`(effect)`

### Examples

```scheme
(define t1 (make-operation))
(define t2 (make-operation))

(test 1 (with (handler)
          1))

(test 2 (with (handler
               ((t1 k) 2))
          (t1)
          1))

(test 3 (with (handler
                ((t1 k) 3))
          (with (handler
                  ((t2 k) 4))
            (t1)
            5)))

(test 4 (with (handler
                ((t1 k) 3))
          (with (handler
                  ((t2 k) 4))
            (t2)
            5)))

(test 10 (with (handler
                 ((t1 k x)
                  (k x)))
           (with (handler
                   ((t2 k) 9))
             (t1 10))))

(test 11 (with (handler
                ((t1 k x) (+ x 1))
                ((t2 k x) (+ x 2)))
           (t2 9)))

(define get (make-operation))
(define put (make-operation))

(define state
  (handler
    ((get k)
     (lambda (s)
       ((k s) 12)))
    ((put k x)
     (lambda (_)
       ((k (if #f #f)) x)))
    ((else . arg*)
     (lambda (s)
       (apply values arg*)))))

(test 10
      ((with state
         10)
       0))

(test 11
      ((with state
         (get))
       11))

(test 12
      ((with state
         (put (+ 1 (get)))
         (get))
       11))

(define (collector x x!)
  (let f ((ls '()))
    (handler*
      ((x k)
       (with (f ls)
         (k ls)))
      ((x! k n)
       (with (f (cons n ls))
         (k (if #f #f)))))))

(define collect! (make-operation))
(define get-collection (make-operation))

(test '(2 1)
      (with (collector get-collection collect!)
        (collect! 1)
        (collect! 2)
        (get-collection)))
```

### Syntax

`(with <handler> <body>)`

The `<handler>` expression is evaluated to obtain an effect handler, the `<body>` is evaluated and the values of its last expression are returned to the handler.  The handler is installed in the dynamic extent of the evaluation of the `<body>`.

`(handler ((<op> <k> . <formals>) <body>) ...)
`
`(handler ((<op> <k> . <formals>) <body>) ... ((else . <formals>) <body>)`

The `<ops>` must be expressions and the `<ks>` must be identifiers.  The first form is equivalent to `(handler ((<op> <k> . <formals>) <body>) ... ((else . VAL*) (apply values VAL*)))`.

The `<ops>` are evaluated to obtain operations.  A handler that handles these operations is then returned.  The returned handler handles an operation with arguments list `VAL*` by creating a procedure `K` that, when called on arguments, calls the delimited continuation corresponding to the operation on the arguments.  During the dynamic extent of the call to the delimited continuation, the handler is installed.  The `<body>` corresponding to the operation is then evaluated with `<k>` bound to `K` and the `<formals>` bound to `VAL*` and the values of its last expression are returned.

When values are returned to a handler, these are bound to the `<formals>` in the else clause, the corresponding `<body>` is evaluated and the values of its last expression are returned. 

`(handler* ((<op> <k> . <formals>) <body>) ...)`

`(handler* ((<op> <k> . <formals>) <body>) ... ((else . <formals>) <body>)`

Like the respective `handler` forms except that the handler is not installed during the dynamic extent of the call to the delimited continuation corresponding to an operation being handled.  (Such handlers are also called *shallow effect handlers*.)

### Procedures

`(make-operation)`

Returns a procedure, which is an operation.  When called on arguments, the current continuation is aborted to the nearest installed handler handling such an operation.  The discarded continuation frames form the delimited continuation corresponding to the operation.

## Liquids

A `liquid` is an identifier naming a location.  Like a variable, a liquid can be mutated.  Contrary to variable assignments, assignments to liquids are pure in that the assignments are not exposed through `call/cc` as the following example shows.

### Example

Consider the following definition of an accumulation procedure that takes a mapper and a list and should return the sum of the values of applying the mapper to each list element:

```scheme
(define (accumulate mapper ls)
  (let ((count 0))
    (for-each 
     (lambda (el)
       (set! count (+ count (mapper el))))
     ls)
    count))
```

(This example is not to show good programming style; for Scheme lists one would use `fold` instead of `for-each`, so think of a sequence type for which a `for-each` but not a `fold` is defined.)

The procedure `accumulate` should be pure procedure but it isn't.  Through capturing a continuation during the dynamic extent of a call to `mapper`, the mutation of `count`, which should be an implementation detail, can be exposed.

A correct implementation for `accumulate` uses liquids instead of variables:

```scheme
(define (accumulate mapper ls)
  (let-liquid ((count 0))
    (for-each 
     (lambda (el)
       (set! count (+ count (mapper el))))
     ls)
    count))
```

Another use case is an implementation of McCarthy's `amb` operator that is correct even in the presence of `call/cc`.  As long as only liquids are mutated, even mutation is transparent to `amb`.  An implementation of the operator can be found in the library `(amb)`.

### Library name

`(liquid)`

### Binding constructs

`(let-liquid ((LIQUID INIT) ...) BODY)`

The `INIT`s are evaluated in the current environment (in some unspecified order), the `LIQUID`s are bound to fresh locations holding the results, the `BODY` is evaluated in the extended environment, and the values of the last expression of `BODY` are returned.  Each binding of a `LIQUID` has `BODY` as its region. 

Within the dynamic extent of the evaluation of `BODY`, capturing a continuation also captures the current values of the `LIQUID`s and restores them when the continuation is reinstated.

### Syntax

`LIQUID`

An expression consisting of a liquid is a liquid reference.  The value of the liquid reference is the value stored in the location to which the liquid is bound.

It is an error if the current continuation up to the continuation of the most recent `liquid-let` or `let-liquid` involving `LIQUID` includes a continuation barrier.

`(set! LIQUID EXPRESSION)`

The EXPRESSION is evaluated, and the resulting value is stored in the location to which LIQUID is bound.

It is an error if the current continuation up to the continuation of the most recent `liquid-let` or `let-liquid` involving `LIQUID` includes a continuation barrier.

`(liquid-let ((LIQUID EXPRESSION) ...) BODY)`

The `INIT`s are evaluated (in some unspecified order), the resulting values are then stored in the locations to which the `LIQUID`s are bound, the `BODY` is evaluated, the previous values of the `LIQUID`s are restored, and the values of the last expression `BODY` are returned.

Within the dynamic extent of the evaluation of `BODY`, capturing a continuation also captures the current values of the `LIQUID`s and restores them when the continuation is reinstated.

### Definitions

`(define-liquid LIQUID EXPRESSION)`

`LIQUID` is bound to a location whose initial value is given by `EXPRESSION`.
