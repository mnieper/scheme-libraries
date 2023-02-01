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

### Library name

`(liquid)`

### Binding constructs

`(let-liquid ((LIQUID INIT) ...) BODY)`

The `INIT`s are evaluated in the current environment (in some unspecified order), the `LIQUID`s are bound to fresh locations holding the results, the `BODY` is evaluated in the extended environment, and the values of the last expression of `BODY` are returned.  Each binding of a `LIQUID` has `BODY` as its region. 

Within the dynamic extent of the evaluation of `BODY`, capturing a continuation also captures the current values of the `LIQUID`s and restores them when the continuation is reinstated.

### Syntax

`LIQUID`

An expression consisting of a liquid is a liquid reference.  The value of the liquid reference is the value stored in the location to which the liquid is bound.

`(set! LIQUID EXPRESSION)`

The EXPRESSION is evaluated, and the resulting value is stored in the location to which LIQUID is bound.

`(liquid-let ((LIQUID EXPRESSION) ...) BODY)`

The `INIT`s are evaluated (in some unspecified order), the resulting values are then stored in the locations to which the `LIQUID`s are bound, the `BODY` is evaluated, the previous values of the `LIQUID`s are restored, and the values of the last expression `BODY` are returned.

Within the dynamic extent of the evaluation of `BODY`, capturing a continuation also captures the current values of the `LIQUID`s and restores them when the continuation is reinstated.

### Definitions

`(define-liquid LIQUID EXPRESSION)`

`LIQUID` is bound to a location whose initial value is given by `EXPRESSION`.

## Protectors

Protectors and protected parameters can be used to write pure effectful code.  They replace the environment monad and the procedural sublanguage introduced in SRFI 165.

### Library name

`(protector)`

### Procedures

`(make-protector)`

Returns a new protector.

`(protector-make-parameter PROTECTOR DEFAULT [FILTER])`

Returns a new protected parameter, governed by `PROTECTOR` with initial value `DEFAULT` and `FILTER`.

`(with-protector PROTECTOR THUNK)`

Evaluates `THUNK` and returns the resulting values.  Within the dynamic extent of the call to `THUNK`, capturing a continuation also captures the values of all protected parameters governed by `PROTECTOR` and restores them when the continuation is reinstated.  When the call to `THUNK` returns, all protected parameters governed by `PROTECTOR` are restored to their values they had when `THUNK` was called.

### Protected parameters

`(PROTECTED-PARAMETER)`

Returns the current value of the `PROTECTED-PARAMETER`.

`(PROTECTED-PARAMETER VALUE)`

Applies the protected parameter's filter to `VALUE` and makes the resulting value the current value of `PROTECTED-PARAMETER`.
