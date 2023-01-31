# Scheme Libraries

## Protector

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
