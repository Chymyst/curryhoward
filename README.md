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

[R. Dyckhoff, _Contraction-Free Sequent Calculi for Intuitionistic Logic_, The Journal of Symbolic Logic, Vol. 57, No. 3, (Sep., 1992), pp. 795-807](https://rd.host.cs.st-andrews.ac.uk/publications/jsl57.pdf).


# Usage

```scala
import io.chymyst.ch._

object MyApp extends App {

  def f[X, Y]: X => Y => X = implement

  // The fully polymorphic code `(x: X) ⇒ (y: Y) ⇒ x` is generated for the function `f`.
  
  f(123)("abc") // returns 123
}

```

See also the [tutorial](docs/Tutorial.md).

# Unit tests

`sbt test`

# Status

- The theorem prover for the full IPL is working
- When a type cannot be inhabited, signal a compile-time error
- Support for `Unit` type, constant types, type parametrs, function types, tuples, sealed traits / case classes / case objects
- Both conventional Scala syntax `def f[T](x: T): T` and curried syntax `def f[T]: T ⇒ T` can be used
- When a type can be implemented in more than one way, heuristics ("least information loss") are used to prefer implementations that are more likely to satisfy algebraic laws 
- Signal error when a type can be implemented in more than one way despite using heuristics
- Tests and a tutorial

# Bugs and to-do

- Recursive case classes (including `List`!) cause stack overflow
- Type aliases `type MyType[T] = (Int, T)` generate incorrect code
- No support for the conventional Scala-style function types with multiple arguments, e.g. `(T, U) ⇒ T`; tuples need to be used instead, e.g. `((T, U)) ⇒ T`
- `toType()` cannot work with constant arguments, needs variables
- The type parameters must be named the same in the sealed trait and in each case class, otherwise things break

# Examples of functionality

The following code examples show how various functions are implemented automatically, given their type.

```scala
// "Weak" Peirce's law:
def f[A, B]: ((((A ⇒ B) ⇒ A) ⇒ A) ⇒ B) ⇒ B = implement

// Weak law of _tertium non datur_
def f[A, B]: (Either[A, A ⇒ B] ⇒ B) ⇒ B = implement

```

Automatic implementations of `pure`, `map`, and `flatMap` for the `Reader` monad:

```scala
def pure[E, A]: A ⇒ (E ⇒ A) = implement
def map[E, A, B]: (E ⇒ A) ⇒ (A ⇒ B) ⇒ (E ⇒ B) = implement
def flatMap[E, A, B]: (E ⇒ A) ⇒ (A ⇒ E ⇒ B) ⇒ (E ⇒ B) = implement

```

Constant types are treated as type parameters.

```scala

def f[A, B]: A ⇒ Int ⇒ (A, Int) = implement

f("abc")(123) // returns the tuple ("abc", 123)

```

## Alternative syntax

There are three ways in which code can be generated based on type:

1. the type is specified on the left-hand side: `def f: ... = implement`
2. the type is specified as an explicit type parameter on the right-hand side, and it is not necessary to define a new value: `ofType[...](...)`
3. the type is specified as an explicit type parameter on the right-hand side, and all possible implementations are returned: `allOfType[...](...)`

```scala
// Conventional Scala syntax for functions.
def f1[T, U](x: T, y: T ⇒ U) : (T, U) = implement

// Fully or partially curried functions.
def f2[T, U](x: T): (T ⇒ U) ⇒ (T, U) = implement

def f3[T, U]: T ⇒ (T ⇒ U) ⇒ (T, U) = implement

// Specifying

// Using locally generated expressions. 
case class UserId(name: String, id: Long)

val a: Int = 123
val b: String = "abc"
val c: Int ⇒ Long = _.toLong

ofType[UserId](a, b, c).id // 123L

```

## Heuristics for choosing different implementations

If the theorem prover finds several alternative implementations of a function, it attempts to find the implementation with the smallest "information loss".

The "information loss" of a function is defined as an integer number computed as the sum of:

- the number of (curried) arguments that are ignored by the function,
- the number of tuple parts that are computed but subsequently not used by the function,
- the number of `case` clauses that do not use their arguments.

Choosing the smallest "information loss" is a heuristic that enables automatic implementations of `pure`, `map`, and `flatMap` for the `State` monad:

```scala
def pure[S, A]: A ⇒ (S ⇒ (A, S)) = implement
def map[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ (S ⇒ (B, S)) = implement
def flatMap[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ S ⇒ (B, S)) ⇒ (S ⇒ (B, S)) = implement

```

Note that there are several inequivalent implementations for the State monad's `map` and `flatMap`,
but only one of them loses no information (and thus has a chance of satisfying the correct laws).

The theorem prover will generate a (compile-time) error when there are two or more implementations with the smallest level of information loss.

If there are several possible implementations but only one implementation with the smallest level of information loss,
the theorem prover will choose that implementation but print a warning message such as

```
Warning:scalac: type (S → (A, S)) → (A → B) → S → (B, S) has 2 implementations (laws need checking?)

```

This message means that the resulting implementation is _probably_ the right one, but there was a choice to be made.
If there exist some equational laws that apply to this function, the laws need to be checked.

## Case classes

Sealed traits and case classes are supported, including `Option` and `Either`:

```scala
def eitherCommut[A, B]: Either[A, B] ⇒ Either[B, A] = implement

def eitherAssoc[A, B, C]: Either[A, Either[B, C]] ⇒ Either[Either[A, B], C] = implement

```

Case objects are treated as named `Unit` type.

The "smallest information loss" heuristic allows us to select the "better" implementation in the following example:

```scala
def optionId[X]: Option[X] ⇒ Option[X] = implement

optionId(Some(123)) == 123
optionId(None) == None

```

There are two possible implementations of the type `Option[X] ⇒ Option[X]`: the "trivial" implementation (always return `None`), and the "interesting" implementation (return the same value as given).
The "trivial" implementation is rejected by the algorithm because it ignores the information given in the original data.

Generally, the algorithm prefers implementations that use more parts of the disjunction.
