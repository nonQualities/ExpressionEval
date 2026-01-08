# Evaluation Module Documentation (`Eval.hs`)

## 1. Overview

This module implements a **big-step (natural semantics) evaluator** for a small expression language defined in `AST.hs`.

The evaluator maps expressions (`Exp`) directly to runtime values (`Val`) using a custom evaluation monad `EvalM`. The design explicitly models:

- Lexical scoping via an immutable environment
- State threading for future extensions (printing, stores)
- Explicit error handling with recovery

Conceptually, the evaluator corresponds to the monad stack:

```
Reader Env (State State (Either Error a))
```

This stack is implemented manually to keep evaluation order, scoping rules, and failure propagation explicit.

---

## 2. Runtime Values (`Val`)

```haskell
data Val
    = ValInt Int
    | ValBool Bool
    | ValFun Env Vname Exp
```

### Semantics

| Constructor | Meaning |
|------------|--------|
| `ValInt` | Integer values |
| `ValBool` | Boolean values |
| `ValFun` | Function closures |

A `ValFun` is a **closure**, capturing:
1. The environment at definition time
2. The formal parameter name
3. The function body expression

This ensures lexical (static) scoping.

---

## 3. Environment (`Env`)

```haskell
type Env = [(Vname, Val)]
```

- Implemented as an association list
- New bindings shadow older ones
- Variable lookup is deterministic and linear

This choice favors simplicity and clarity over performance.

---

## 4. Evaluation State (`State`)

```haskell
type State = ([String], KvStore)
```

### Components

1. **Printed output**
   - Accumulated in reverse order
   - Reversed when evaluation completes

2. **Key-value store**
   - Placeholder for future extensions
   - Threaded through evaluation to preserve structure

---

## 5. Error Model

```haskell
type Error = String
```

- Errors are explicit values
- No partial functions or implicit crashes
- Failure immediately aborts evaluation unless caught

---

## 6. Evaluation Monad (`EvalM`)

more on [[Monad]]

```haskell
newtype EvalM a =
    EvalM (Env -> State -> (Either Error a, State))
```

### Responsibilities

`EvalM` simultaneously:
- Reads from an environment
- Reads and updates state
- May fail with an error

### Design Rationale

Instead of stacking monads, the evaluator exposes the mechanics explicitly, making it suitable for:

- Formal reasoning
- Teaching
- Controlled extension

---

## 7. Functor, Applicative, and Monad

### Functor

- Applies a pure function to a successful result
- Errors pass through unchanged

### Applicative

- Enables sequencing of independent computations
- Implemented via `ap`

### Monad

- Enables dependent sequencing
- Short-circuits on the first error

---

## 8. Environment Operations

### `askEnv`

Retrieves the current environment.

Used for:
- Variable lookup
- Closure creation

### `localEnv`

```haskell
localEnv :: (Env -> Env) -> EvalM a -> EvalM a
```

Executes a computation under a modified environment.

Used for:
- Let bindings
- Function application
- Loop variables

---

## 9. Error Handling

### `failure`

Immediately aborts evaluation with an error message.

### `catch`

```haskell
catch :: EvalM a -> EvalM a -> EvalM a
```

Semantics:
- Evaluate the first computation
- If it fails, evaluate the second
- Otherwise, keep the first result

---

## 10. Running the Evaluator

```haskell
runEval :: EvalM a -> ([String], Either Error a)
```

- Starts from an empty environment and state
- Returns printed output and result
- Output order is normalized before returning

---

## 11. Integer Operation Helpers

### `evalIntBinOp`

Used for integer operations that may fail.

Responsibilities:
1. Evaluate both operands
2. Ensure both are integers
3. Apply a possibly failing operation

### `evalIntBinOp'`

Used for total integer operations such as addition and multiplication.

---

## 12. Evaluation Strategy

```haskell
eval :: Exp -> EvalM Val
```

This is a **big-step evaluator**:
- Expressions evaluate directly to values
- No intermediate machine states are exposed
- Errors abort evaluation immediately

---

## 13. Expression Semantics

### Constants

Evaluate directly to runtime values.

### Variables

- Resolved via environment lookup
- Unbound variables raise errors

### Arithmetic

- Dynamically type-checked
- Invalid operands cause failure

---

## 14. Conditionals

```haskell
If cond e1 e2
```

- Condition must evaluate to `ValBool`
- Only the selected branch is evaluated
- Non-boolean conditions cause failure

---

## 15. Let Bindings

```haskell
Let v e1 e2
```

Evaluation steps:
1. Evaluate `e1`
2. Extend the environment with `v`
3. Evaluate `e2` under the extended environment

---

## 16. For Loop Semantics

```haskell
ForLoop (p, initial) (iv, bound) body
```

This construct behaves like a **fold**, not a traditional imperative loop.

- `p` is an accumulator
- `initial` is the initial accumulator value
- `iv` is the loop index
- `bound` controls iteration count

The loop runs from `i = 0` to `i < bound`.

---

## 17. Lambda Abstraction

```haskell
Lambda v body
```

- Captures the current environment
- Produces a closure
- Does not evaluate the body immediately

---

## 18. Function Application

```haskell
Apply e1 e2
```

Semantics:
1. Evaluate the function expression
2. Evaluate the argument
3. Extend the function’s closure environment
4. Evaluate the body in that environment

This enforces lexical scoping.

---

## 19. Try-Catch

```haskell
TryCatch e1 e2
```

- Evaluate `e1`
- If it fails, evaluate `e2`
- Otherwise, keep the result of `e1`

---

## 20. Conceptual Summary

This evaluator is:

- Deterministic
- Lexically scoped
- Big-step
- Error-explicit
- Designed for extensibility

Its explicit structure supports formal reasoning and systematic extension.

---

## 21. Natural Extensions

This architecture can be extended to support:

- Small-step semantics
- CEK / CESK machines
- Mutable references
- Printing and tracing
- Static type checking
- Desugaring into a core language

---

End of documentation.
