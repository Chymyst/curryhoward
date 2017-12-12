[![Build Status](https://travis-ci.org/Chymyst/curryhoward.svg?branch=master)](https://travis-ci.org/Chymyst/curryhoward)
[![Coverage Status](https://codecov.io/gh/Chymyst/curryhoward/coverage.svg?branch=master)](https://codecov.io/gh/Chymyst/curryhoward?branch=master)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Github Tag](https://img.shields.io/github/tag/Chymyst/curryhoward.svg?label=release&colorB=blue)](https://github.com/Chymyst/curryhoward/tags)
[![Maven Central](https://img.shields.io/maven-central/v/io.chymyst/curryhoward_2.12.svg)](http://search.maven.org/#search%7Cga%7C1%7Cio.chymyst)

# curryhoward
A library for automatic implementation of fully type-parametric functions via the Curry-Howard isomorphism.

The Curry-Howard isomorphism maps functions with fully parametric types to theorems in propositional logic with universally quantified propositions.
For example,

```scala
def f[X, Y]: X => Y => X = (x: X) => (y: Y) => X

```

is mapped to the propositional theorem `forall X, Y: X => (Y => X)` in the intuitionistic propositional logic (IPL).

This project is a Scala utility that generates code for such functions using a decision procedure algorithm for IPL.

# Usage

```scala
import io.chymyst.ch._

object MyApp extends App {

  def f[X, Y]: X => Y => X = implement

  // The code `(x: X) => (y: Y) => x` is generated for the function `f`.
}

```

# Unit tests

`sbt test`

# Status

Very early development. First examples are working as proof-of-concept.

