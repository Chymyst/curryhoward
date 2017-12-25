<link href="{{ site.github.url }}/tables.css" rel="stylesheet" />

# Quick start

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

## Concurrent programming: processes and data

In the chemical machine, an asynchronous concurrent process (called a **reaction**) is implemented as a computation that works with a special kind of data called **molecules**.
A reaction can consume one or more input molecules and may emit (zero or more) new molecules.

Molecules are created out of ordinary data values by calling special **molecule emitters**.

All molecule emitters must be declared before using them.
A new molecule emitter is created using the special syntax `m[T]`, where `T` is the type of the value:

```tut
val c = m[Int] // emitter for molecule `c` with payload value of type `Int`

val in = m[Int] // emitter for molecule `in` with `Int` payload value

val result = m[Int] // emitter for molecule `result` with `String` payload value

```

Molecules can be emitted using this syntax:

```scala
val c = m[Int] // emitter for molecule `c` with payload value of type `Int`
c(123) // emit a new molecule `c()` carrying the payload value `123` of type `Int`

```
