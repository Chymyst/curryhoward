[![Build Status](https://travis-ci.org/Chymyst/curryhoward.svg?branch=master)](https://travis-ci.org/Chymyst/curryhoward)
[![Coverage Status](https://codecov.io/gh/Chymyst/curryhoward/coverage.svg?branch=master)](https://codecov.io/gh/Chymyst/curryhoward?branch=master)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Github Tag](https://img.shields.io/github/tag/Chymyst/curryhoward.svg?label=release&colorB=blue)](https://github.com/Chymyst/curryhoward/tags)
[![Maven Central](https://img.shields.io/maven-central/v/io.chymyst/curryhoward_2.12.svg)](http://search.maven.org/#search%7Cga%7C1%7Cio.chymyst)

# curryhoward
A library for automatic implementation of fully type-parametric functions via the Curry-Howard isomorphism.

The Curry-Howard isomorphism maps functions with fully parametric types to theorems in the intuitionistic propositional logic (IPL) with universally quantified propositions.

For example, the type of the function

```scala
def f[X, Y]: X => Y => X = (x: X) => ((y: Y) => x)

```

is mapped to the propositional theorem `forall X, Y: X => (Y => X)` in the IPL.

This project provides a Scala utility that generates code for such functions using a decision algorithm for IPL.

The current implementation uses the sequent calculus called LJT as presented in:

[D. Galmiche , D. Larchey-Wendling. _Formulae-as-Resources Management for an Intuitionistic Theorem Prover_ (1998)](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.35.2618). 	In 5th Workshop on Logic, Language, Information and Computation, WoLLIC'98, Sao Paulo.

The original presentation of the LJT rules are found in:

R. Dyckhoff, _Contraction-Free Sequent Calculi for Intuitionistic Logic_, The Journal of Symbolic Logic, Vol. 57, No. 3, (Sep., 1992), pp. 795-807.


# Usage

```scala
import io.chymyst.ch._

object MyApp extends App {

  def f[X, Y]: X => Y => X = implement

  // The code `(x: X) => (y: Y) => x` is generated for the function `f`.
  
  f(123)("abc") // returns 123
}

```

# Unit tests

`sbt test`

# Status

Early development.

The implicational fragment of the IPL is working.

```scala
// "Weak" Peirce's law:
def f[A, B]: ((((A ⇒ B) ⇒ A) ⇒ A) ⇒ B) ⇒ B = implement

```

