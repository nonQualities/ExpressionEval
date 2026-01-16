
# ExpressionEval

> **A Haskell-based Mini Interpreter for Arithmetic & Logic**

## Overview

**ExpressionEval** is a lightweight, monadic interpreter written in Haskell. Designed to evaluate basic arithmetic operations, it serves as a foundational implementation of a parser-interpreter pipeline.

While currently focused on core arithmetic, the architecture is extensible, leveraging Haskell's type system and monadic structures to handle evaluation context and error propagation.

## Features

* **Arithmetic Evaluation**: specific support for standard integer/floating-point operations.
* **Monadic Context**: Uses a custom Monad stack (see `Monad.md`) for state management and error handling during evaluation.
* **Modular Design**: Separates parsing, evaluation logic, and application entry points.
* **Documentation**: Includes internal architectural notes (`EvalDoc.md`) for contributors.

## Project Structure

```bash
ExpressionEval/
├── app/             # Application entry point (Main.hs)
├── src/             # Core library logic (implied)
├── test/            # Unit tests
├── dist-newstyle/   # Cabal build artifacts
├── EvalDoc.md       # Documentation on the evaluation logic
├── Monad.md         # Documentation on the Monad stack used
├── Makefile         # Make build commands
├── package.yaml     # Hpack package description
└── ExpressionEval.cabal # Cabal file (generated)

```

## Getting Started

### Prerequisites

* [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler)
* [Cabal](https://www.haskell.org/cabal/) or [Stack](https://docs.haskellstack.org/en/stable/README/)
* `make` (optional, for using the Makefile)

### Building the Project

You can build the project using **Cabal**:
```bash
make gen

```


## Documentation

For a deeper dive into the theory and implementation details:
* **Evaluation Logic**: See [EvalDoc.md](https://github.com/nonQualities/ExpressionEval/blob/main/EvalDoc.md) for how expressions are reduced.
* **Monadic Structure**: See [Monad.md](https://github.com/nonQualities/ExpressionEval/blob/main/Monad.md) 

## Contributing

1. Fork the repository.
2. Create a feature branch 
3. Commit your changes 
4. Push to the branch 
5. Open a Pull Request.



---

*Generated using Github Copilot*