# curryhoward
A small library for automatic implementation of fully parametric functions via the Curry-Howard isomorphism.

The Curry-Howard isomorphism maps functions with fully parametric types to theorems in propositional logic with universally quantified propositions.
For example,

```scala
def f[X, Y]: X => Y => X = (x: X) => (y: Y) => X

```

corresponds to the propositional theorem `forall X, Y: X => (Y => X)` in the intuitionistic propositional logic (IPL).

This project is a Scala utility that generates code for such functions using a decision procedure algorithm for IPL.

# Usage

```scala
import io.chymyst.ch._

object MyApp extends App {

  def f[X, Y]: X => Y => X = implement

  // The code `(x: X) => (y: Y) => x` is generated for the function `f`.
}

```

# Status

Very early development. First examples are working as proof-of-concept.

