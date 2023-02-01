# Scheme Libraries

This repository serves as a Scheme library incubator.

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
