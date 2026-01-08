# The Mathematics of Functors, Applicatives, and Monads

---

## 1. Why These Structures Exist

Functor, Applicative, and Monad are not programming inventions.  
They are **categorical structures** imported into programming to control *composition under effects*.

The core problem they solve is:

> How do we compose computations that are not plain functions  
> but carry extra structure (failure, state, context, nondeterminism)?

Category theory provides precise answers.

---

## 2. Categories: The Minimal Setting

A **category** 𝓒 consists of:

1. A collection of **objects**
2. A collection of **morphisms** (arrows) between objects
3. A **composition** operation `∘`
4. An **identity morphism** for each object

Satisfying:

- **Associativity**:  
  `(f ∘ g) ∘ h = f ∘ (g ∘ h)`
- **Identity laws**:  
  `id ∘ f = f = f ∘ id`

---

### Example: The Category **Hask**

- Objects: Types (`Int`, `Bool`, `a`, `b`)
- Morphisms: Total functions (`a -> b`)
- Identity: `id x = x`
- Composition: `(.)`

Haskell programs live inside this category.

---

## 3. Functors: Structure-Preserving Mappings

### 3.1 Mathematical Definition

A **functor** `F : 𝓒 → 𝓓` consists of:

1. A mapping of objects  
   ```
   A ↦ F(A)
   ```

2. A mapping of morphisms  
   ```
   (f : A → B) ↦ F(f) : F(A) → F(B)
   ```

Such that:

- **Identity preservation**  
  ```
  F(id_A) = id_{F(A)}
  ```

- **Composition preservation**  
  ```
  F(f ∘ g) = F(f) ∘ F(g)
  ```

---

### 3.2 Programming Interpretation

In Haskell:

```haskell
fmap :: (a -> b) -> f a -> f b
```

- `f` is a type constructor (`Maybe`, `List`, `EvalM`)
- `fmap` lifts a function into a context

The functor **does not inspect or modify the context**.
It only applies the function *inside* it.

---

### 3.3 Functor Laws (Mandatory)

1. **Identity**
   ```
   fmap id = id
   ```

2. **Composition**
   ```
   fmap (f ∘ g) = fmap f ∘ fmap g
   ```

Violation of these laws breaks compositional reasoning.

---

### 3.4 Conceptual Meaning

A functor represents:

> A *computational context* that preserves the shape of computation.

Examples:
- `Maybe`: computation may fail
- `[]`: computation may return many results
- `Reader r`: computation depends on an environment

---

## 4. Applicative Functors: Structured Function Application

Functors allow mapping functions.  
Applicatives allow **applying functions inside a context**.

---

### 4.1 Mathematical Characterization

An **applicative functor** is a functor equipped with:

- A unit (pure value injection)
- A binary operation for application

In categorical terms, applicatives correspond to **lax monoidal functors**.

---

### 4.2 Core Operations

```haskell
pure  :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
```

Interpretation:
- `pure` embeds a value into the context
- `<*>` applies a contextual function to a contextual value

---

### 4.3 Applicative Laws

1. **Identity**
   ```
   pure id <*> v = v
   ```

2. **Composition**
   ```
   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
   ```

3. **Homomorphism**
   ```
   pure f <*> pure x = pure (f x)
   ```

4. **Interchange**
   ```
   u <*> pure y = pure ($ y) <*> u
   ```

These ensure deterministic evaluation structure.

---

### 4.4 Why Applicatives Matter

Applicatives capture:

> Computations with **fixed structure** but **effectful values**

Key property:
- The computation graph is known *before execution*

This allows:
- Parallel evaluation
- Static analysis
- Validation accumulation

Monads cannot guarantee this.

---

## 5. Monads: Sequencing with Dependency

Monads allow later computations to **depend on earlier results**.

---

### 5.1 Mathematical Definition

A **monad** on a category 𝓒 is a triple:

```
(T, η, μ)
```

Where:
- `T : 𝓒 → 𝓒` is an endofunctor
- `η : Id → T` (unit)
- `μ : T² → T` (multiplication)

Satisfying coherence laws.

---

### 5.2 Programming Formulation

```haskell
return :: a -> m a
(>>=)  :: m a -> (a -> m b) -> m b
```

Equivalently:

```haskell
join :: m (m a) -> m a
```

---

### 5.3 Monad Laws

1. **Left identity**
   ```
   return a >>= f = f a
   ```

2. **Right identity**
   ```
   m >>= return = m
   ```

3. **Associativity**
   ```
   (m >>= f) >>= g = m >>= (\x -> f x >>= g)
   ```

These laws guarantee predictable sequencing.

---

### 5.4 Conceptual Meaning

A monad represents:

> Computations with **effects and dependency**

Examples:
- `Maybe`: failure propagation
- `State`: state threading
- `IO`: interaction with the external world
- `EvalM`: environment + state + failure

Monads define **control flow**, not just mapping.

---

## 6. Relationship Between the Three

There is a strict hierarchy:

```
Monad ⊂ Applicative ⊂ Functor
```

Meaning:
- Every Monad is an Applicative
- Every Applicative is a Functor
- The converse is false

Each step adds expressive power but removes static guarantees.

---

## 7. Categorical Summary Table

| Structure | Adds | Loses |
|---------|------|-------|
| Functor | Mapping | Nothing |
| Applicative | Structured application | Dynamic dependency |
| Monad | Dependent sequencing | Static structure |

---

## 8. Why Not Always Use Monads?

Because monads are **too powerful**.

They:
- Hide evaluation order
- Prevent static analysis
- Obscure parallelism

Applicatives should be preferred when dependency is unnecessary.

---

## 9. Mathematical Takeaway

- Functors preserve structure
- Applicatives combine independent effects
- Monads sequence dependent effects

They are not metaphors.  
They are **algebraic laws imposed on computation**.

Ignoring the laws breaks reasoning, refactoring, and correctness.

---

## 10. Final Perspective

Category theory does not explain how to write programs.  
It explains **why certain programs compose and others do not**.

Functors, Applicatives, and Monads are the minimal algebraic tools
needed to make effects composable without chaos.

---

End of document.
