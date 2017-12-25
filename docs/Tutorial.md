<link href="{{ site.github.url }}/tables.css" rel="stylesheet" />

## Setup

First, declare this library dependency in your `build.sbt`:

```scala
libraryDependencies += "io.chymyst" %% "curryhoward" % "latest.integration"

```

The `curryhoward` functionality becomes available once you add this statement:

```scala
scala> import io.chymyst.ch._
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

```scala
scala> case class User[N, I](name: N, id: I)
defined class User

scala> def makeUser[N, I]: N ⇒ (N ⇒ I) ⇒ User[N, I] = implement
<console>:17: Returning term: (b ⇒ a ⇒ User(b, a b))
       def makeUser[N, I]: N ⇒ (N ⇒ I) ⇒ User[N, I] = implement
                                                      ^
makeUser: [N, I]=> N => ((N => I) => User[N,I])

scala> makeUser[Int, String](123)(n => "id:" + (n * 100).toString)
res0: User[Int,String] = User(123,id:12300)
```

The library prints the lambda-calculus term notation for the generated code.
In this example, the term is `b ⇒ a ⇒ User(b, a b)`.
