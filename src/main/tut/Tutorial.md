<link href="{{ site.github.url }}/tables.css" rel="stylesheet" />

## Setup

First, declare this library dependency in your `build.sbt`:

```scala
libraryDependencies += "io.chymyst" %% "curryhoward" % "latest.integration"

```

The `curryhoward` functionality becomes available once you add this statement:

```tut:silent
import io.chymyst.ch._
```

This imports all the necessary symbols such as `implement`, `ofType`, `allOfType` and so on.

```tut:silent
System.setProperty("curryhoward.log", "terms")
```

This will enable logging of the generated terms.
Compared with other logging options (`"macros"` and `"prover"`), this will give a small amount of debugging output that will be useful in the tutorial.

# First examples

The `curryhoward` library is a compile-time code generator that implements pure functions given their types.

This works best for functions that perform generic data manipulation that is not specific to any given data type.

Consider this code:

```scala
case class User(name: String, id: Long)

def makeUser(userName: String, userIdGenerator: String ⇒ Long): User = {
  User(userName, userIdGenerator(userName))
}

```

The function `makeUser` does not actually perform any computation specific to types `String` or `Long`.
So, this code can be generalized to arbitrary types:

```scala
case class User[N, I](name: N, id: I)

def makeUser[N, I](userName: N, userIdGenerator: N ⇒ I): User[N, I] = {
  User(userName, userIdGenerator(userName))
}

```

When types are replaced by type parameters, it is clear that 
the only way to get a user ID of type `I` is to apply `userIdGenerator` to `userName`.

We see that the type of the function `makeUser[N, I]` constrains its algorithm to such an extent that there is only one way to write the code.

The `curryhoward` library can generate the code of functions of this sort using the macro `implement`:

```tut
case class User[N, I](name: N, id: I)
def makeUser[N, I](userName: N, userIdGenerator: N ⇒ I): User[N, I] = implement
makeUser(123, (n: Int) ⇒ "id:" + (n * 100).toString)
```

The library always prints the lambda-calculus term corresponding to the generated code.
In this example, the term is `User(userName, userIdGenerator userName)`.

The chosen notation for lambda-calculus terms supports tuples and named case classes.
Below we will see more examples of the generated terms.

## Curried functions

The `curryhoward` library, of course, works with _curried_ functions as well:

```tut
def const[A, B]: A ⇒ B ⇒ A = implement
val f: String ⇒ Int = const(10)

f("abc")
```

The returned lambda-calculus term is `(a ⇒ b ⇒ a)`.

Here is a more complicated example that automatically implements the `fmap` function for the Reader monad:

```tut
def fmap[E, A, B]: (A ⇒ B) ⇒ (E ⇒ A) ⇒ (E ⇒ B) = implement
val f: Int ⇒ Int = _ + 10
def eaeb[E]: (E ⇒ Int) ⇒ (E ⇒ Int) = fmap(f)
val ea: Double ⇒ Int = x ⇒ (x + 0.5).toInt

eaeb(ea)(1.9)
```

In this example, the returned lambda-calculus term is `(a ⇒ b ⇒ c ⇒ a (b c))`.

One can freely mix the curried and the conventional Scala function syntax.
Here is the applicative `map2` function for the Reader monad:

```tut
def map2[E, A, B, C](readerA: E ⇒ A, readerB: E ⇒ B, f: A ⇒ B ⇒ C): E ⇒ C = implement
```

## Expressions

The macro `implement` is designed to be used when defining new methods, as shown above.

A different use case is to generate an expression from already available values.

In the example shown above, we had to define a new method `makeUser` that creates a value of type `User` from other available values.
If we only need to create a value of type `User` once in our code, we would like to avoid having to define a new method, only to be used once.

This functionality is provided through the macro `ofType`.

Instead of the code

```scala
def makeUser[N, I](userName: N, userIdGenerator: N ⇒ I): User[N, I] = implement

makeUser(123, (n: Int) ⇒ "id:" + (n * 100).toString)

```

we now write

```tut
ofType[User[Int, String]](123, (n: Int) ⇒ "id:" + (n * 100).toString)
```

The macro `ofType[T](x, y, ..., z)` generates an expression of type `T` built up from the given values `x`, `y`, ..., `z`.
The values `x`, `y`, ..., `z` can have any type (but their type must be known or specified at that point).

Unlike `implement`, the macro `ofType()` is designed to be used within expressions, and so we are required to write an explicit type parameter that designates the desired result type.
The macro `ofType()` will not work without specifying a type expression as its type parameter:

```tut:fail
val x: Int = ofType(123)
```

# Choosing between different possible implementations

Some types have more than one implementation.

There are two ways in which this can happen:

1. The type involves a function with several arguments of the same type, for example `X ⇒ X ⇒ ...`.
If this type can be implemented, there will be at least two implementations that differ only by the choice of the argument of type `X`.
This ambiguity cannot be resolved in any reasonable way, except by making types different.
2. One implementation ignores some function argument(s), while another does not.
For example, the "Church numeral" type `(X ⇒ X) ⇒ X ⇒ X` can be implemented in infinitely many ways: `_ ⇒ x ⇒ x`, `f ⇒ x ⇒ f x`, `f ⇒ x ⇒ f (f x)`, etc. Of these ways, the most likely candidate is `f ⇒ x ⇒ f x` because it is the identity on the type `X ⇒ X`, which was most likely what is intended here. The implementation `_ ⇒ x ⇒ x` should be probably rejected because it ignores some part of the given input.

The `curryhoward` library implements a heuristic for choosing the "most sensible" implementation when there are several.
As an example, consider the `map` function for the State monad:

```tut
def map[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ (S ⇒ (B, S)) = implement
```

The warning shows that there exist two inequivalent implementations of the `map` function.
The first implementation was automatically chosen and returned as code.

The difference between the two implementations is that the initial state `s: S` can be either transformed using the given function `S ⇒ (A, S)`, or it can be left unchanged and returned in the pair `(B, S)`.
The first implementation is the correct functor instance for the State monad.
The second implementation "loses information" because the transformed value of type `S` has been computed and ignored.

It appears that information-losing functions are less likely to be useful in practice.
The implementation that is the least information-losing will be more likely, for instance, to satisfy applicable algebraic laws.

In the hopes of producing a sensible and useful answer, the algorithm in `curryhoward` will choose the implementation that loses the least amount of information.
If there are several such implementations, no sensible choice is possible, and the macro will generate a compile-time error:


```tut:fail
def ff[A, B]: A ⇒ A ⇒ (A ⇒ B) ⇒ B = implement
```


# Debugging and logging

The logging options are controlled via the JVM property `"curryhoward.log"`.
The value of this property is a comma-separated list of keywords.
The full logging is switched on by putting `-Dcurryhoward.log=macros,terms,prover` on the Java or SBT command line.

The `macros` logging option will print the code that the macro functions generate.
The `prover` logging option will print the steps in the proof search, including the new sequents generated by each applied rule.
The `terms` logging option will print the terms generated, in the short notation. 
