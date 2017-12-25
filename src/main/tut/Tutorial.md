<link href="{{ site.github.url }}/tables.css" rel="stylesheet" />

## Setup

First, declare this library dependency in your `build.sbt`:

```scala
libraryDependencies += "io.chymyst" %% "curryhoward" % "latest.integration"

```

The `curryhoward` functionality becomes available once you add this statement:

```tut
import io.chymyst.ch._
```

This imports all the necessary symbols such as `implement`, `ofType`, `allOfType` and so on.

# First examples

The `curryhoward` library is a compile-time code generator that implements pure functions given their types.

This works best for functions that perform generic data manipulation that is not specific to any given data type.

An example of such a function is this:

```scala
case class User(name: String, id: Long)

def makeUser(userName: String, userIdGenerator: String ⇒ Long): User = {
  User(userName, userIdGenerator(userName))
}

```

The logic of this computation can be fully generalized to arbitrary types:

```scala
case class User[N, I](name: N, id: I)

def makeUser[N, I](userName: N, userIdGenerator: N ⇒ I): User[N, I] = {
  User(userName, userIdGenerator(userName))
}

```

When types are replaced by type parameters, it is clear that 
the only way to get a user ID of type `I` is to apply `userIdGenerator` to `userName`.

We see that the type of the function `makeUser[N, I]` constrains its algorithm to such an extent that there is only one way to write the code.

The `curryhoward` library can generate the code of functions of this sort:

```tut
case class User[N, I](name: N, id: I)
def makeUser[N, I](userName: N, userIdGenerator: N ⇒ I): User[N, I] = implement
makeUser(123, (n: Int) => "id:" + (n * 100).toString)
```

The library always prints the lambda-calculus term corresponding to the generated code.
In this example, the term is `User(userName, userIdGenerator userName)`.

The chosen notation for lambda-calculus terms supports tuples and named case classes.
Below we will see more examples of the generated terms.

# Curried functions

The `curryhoward` library, of course, works with _curried_ functions as well:

```tut
def const[A, B]: A ⇒ B ⇒ A = implement
val f: String => Int = const(10)

f("abc")
```

The returned lambda-calculus term is `(a ⇒ b ⇒ a)`.

Here is a more complicated example that automatically implements the `fmap` function for the Reader monad:

```tut
def fmap[E, A, B]: (A ⇒ B) ⇒ (E ⇒ A) ⇒ (E ⇒ B) = implement
val f: Int => Int = _ + 10
def eaeb[E]: (E ⇒ Int) ⇒ (E ⇒ Int) = fmap(f)
val ea: Double => Int = x => (x + 0.5).toInt

eaeb(ea)(1.9)
```

In this example, the returned lambda-calculus term is `(a ⇒ b ⇒ c ⇒ a (b c))`.

One can freely mix the curried and the conventional Scala function syntax.
Here is the applicative `map2` function for the Reader monad:

```tut
def map2[E, A, B, C](readerA: E ⇒ A, readerB: E ⇒ B, f: A ⇒ B ⇒ C): E ⇒ C = implement
```
