<link href="{{ site.github.url }}/tables.css" rel="stylesheet" />

## Setup

First, declare this library dependency in your `build.sbt`:

```scala
libraryDependencies += "io.chymyst" %% "curryhoward" % "latest.integration"

```

The `curryhoward` functionality becomes available once you add this statement:

```scala
import io.chymyst.ch._
```

This imports all the necessary symbols such as `implement`, `ofType`, `allOfType` and so on.

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

```scala
scala> case class User[N, I](name: N, id: I)
defined class User

scala> def makeUser[N, I](userName: N, userIdGenerator: N ⇒ I): User[N, I] = implement
DEBUG: applied rule ->R to sequent [ |- N ⇒ (N ⇒ I) ⇒ User[N,I]], new sequents [N |- (N ⇒ I) ⇒ User[N,I]]
DEBUG: applied rule ->R to sequent [N |- (N ⇒ I) ⇒ User[N,I]], new sequents [N ⇒ I; N |- User[N,I]]
DEBUG: applied rule _&R to sequent [N ⇒ I; N |- User[N,I]], new sequents [N ⇒ I; N |- (N, I)]
DEBUG: applied rule &R to sequent [N ⇒ I; N |- (N, I)], new sequents [N ⇒ I; N |- N]; [N ⇒ I; N |- I]
DEBUG: sequent [N ⇒ I; N |- N] followsFromAxioms: b ⇒ a ⇒ a
DEBUG: applied rule ->L1 to sequent [N ⇒ I; N |- N], new sequents [I; N |- N]
DEBUG: sequent [I; N |- N] followsFromAxioms: b ⇒ a ⇒ a
DEBUG: returning 1 terms:
 b ⇒ a ⇒ a ,
 for sequent [I; N |- N]
DEBUG: transformedProofs.map(_.simplify()).distinct took 1 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 b ⇒ a ⇒ a ,
 for sequent [N ⇒ I; N |- N]
DEBUG: applied rule ->L1 to sequent [N ⇒ I; N |- I], new sequents [I; N |- I]
DEBUG: sequent [I; N |- I] followsFromAxioms: a ⇒ b ⇒ a
DEBUG: returning 1 terms:
 a ⇒ b ⇒ a ,
 for sequent [I; N |- I]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 a ⇒ b ⇒ a b ,
 for sequent [N ⇒ I; N |- I]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule &R
DEBUG: returning 1 terms:
 b ⇒ a ⇒ (a, b a) ,
 for sequent [N ⇒ I; N |- (N, I)]
DEBUG: transformedProofs.map(_.simplify()).distinct took 1 ms and produced 1 terms out of 1 back-transformed terms; after rule _&R
DEBUG: returning 1 terms:
 b ⇒ a ⇒ User(a, b a) ,
 for sequent [N ⇒ I; N |- User[N,I]]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->R
DEBUG: returning 1 terms:
 a ⇒ b ⇒ User(a, b a) ,
 for sequent [N |- (N ⇒ I) ⇒ User[N,I]]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->R
DEBUG: returning 1 terms:
 a ⇒ b ⇒ User(a, b a) ,
 for sequent [ |- N ⇒ (N ⇒ I) ⇒ User[N,I]]
DEBUG: for main sequent [ |- N ⇒ (N ⇒ I) ⇒ User[N,I]], obtained 1 final proof terms:
 a ⇒ b ⇒ User(a, b a); score = ((),0,0.0,0.0,1): 0 unused args: Set(); unusedMatchClauseVars=0.0; unusedTupleParts=0; used tuple parts: Vector() . This took 88 ms
<console>:17: Returning term: ((\((a:N) ⇒ (b:N ⇒ I) ⇒ User(a, (b a))) userName) userIdGenerator)
       def makeUser[N, I](userName: N, userIdGenerator: N ⇒ I): User[N, I] = implement
                                                                             ^
<console>:17: Returning code: ((a: N) => ((b: _root_.scala.Function1[N, I]) => User[N, I](a, b(a))))(userName)(userIdGenerator)
       def makeUser[N, I](userName: N, userIdGenerator: N ⇒ I): User[N, I] = implement
                                                                             ^
makeUser: [N, I](userName: N, userIdGenerator: N => I)User[N,I]

scala> makeUser(123, (n: Int) ⇒ "id:" + (n * 100).toString)
res0: User[Int,String] = User(123,id:12300)
```

The library always prints the lambda-calculus term corresponding to the generated code.
In this example, the term is `User(userName, userIdGenerator userName)`.

The chosen notation for lambda-calculus terms supports tuples and named case classes.
Below we will see more examples of the generated terms.

## Curried functions

The `curryhoward` library, of course, works with _curried_ functions as well:

```scala
scala> def const[A, B]: A ⇒ B ⇒ A = implement
DEBUG: applied rule ->R to sequent [ |- A ⇒ B ⇒ A], new sequents [A |- B ⇒ A]
DEBUG: applied rule ->R to sequent [A |- B ⇒ A], new sequents [B; A |- A]
DEBUG: sequent [B; A |- A] followsFromAxioms: b ⇒ a ⇒ a
DEBUG: returning 1 terms:
 b ⇒ a ⇒ a ,
 for sequent [B; A |- A]
DEBUG: transformedProofs.map(_.simplify()).distinct took 1 ms and produced 1 terms out of 1 back-transformed terms; after rule ->R
DEBUG: returning 1 terms:
 a ⇒ b ⇒ a ,
 for sequent [A |- B ⇒ A]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->R
DEBUG: returning 1 terms:
 a ⇒ b ⇒ a ,
 for sequent [ |- A ⇒ B ⇒ A]
DEBUG: for main sequent [ |- A ⇒ B ⇒ A], obtained 1 final proof terms:
 a ⇒ b ⇒ a; score = ((),1,0.0,0.0,0): 1 unused args: Set(b); unusedMatchClauseVars=0.0; unusedTupleParts=0; used tuple parts: List() . This took 48 ms
<console>:15: Returning term: \((a:A) ⇒ (b:B) ⇒ a)
       def const[A, B]: A ⇒ B ⇒ A = implement
                                    ^
<console>:15: Returning code: ((a: A) => ((b: B) => a))
       def const[A, B]: A ⇒ B ⇒ A = implement
                                    ^
const: [A, B]=> A => (B => A)

scala> val f: String ⇒ Int = const(10)
f: String => Int = <function1>

scala> f("abc")
res1: Int = 10
```

The returned lambda-calculus term is `(a ⇒ b ⇒ a)`.

Here is a more complicated example that automatically implements the `fmap` function for the Reader monad:

```scala
scala> def fmap[E, A, B]: (A ⇒ B) ⇒ (E ⇒ A) ⇒ (E ⇒ B) = implement
DEBUG: applied rule ->R to sequent [ |- (A ⇒ B) ⇒ (E ⇒ A) ⇒ E ⇒ B], new sequents [A ⇒ B |- (E ⇒ A) ⇒ E ⇒ B]
DEBUG: applied rule ->R to sequent [A ⇒ B |- (E ⇒ A) ⇒ E ⇒ B], new sequents [E ⇒ A; A ⇒ B |- E ⇒ B]
DEBUG: applied rule ->R to sequent [E ⇒ A; A ⇒ B |- E ⇒ B], new sequents [E; E ⇒ A; A ⇒ B |- B]
DEBUG: applied rule ->L1 to sequent [E; E ⇒ A; A ⇒ B |- B], new sequents [A; E; A ⇒ B |- B]
DEBUG: applied rule ->L1 to sequent [A; E; A ⇒ B |- B], new sequents [B; A; E |- B]
DEBUG: sequent [B; A; E |- B] followsFromAxioms: a ⇒ b ⇒ c ⇒ a
DEBUG: returning 1 terms:
 a ⇒ b ⇒ c ⇒ a ,
 for sequent [B; A; E |- B]
DEBUG: transformedProofs.map(_.simplify()).distinct took 1 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 b ⇒ c ⇒ a ⇒ a b ,
 for sequent [A; E; A ⇒ B |- B]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 c ⇒ b ⇒ a ⇒ a (b c) ,
 for sequent [E; E ⇒ A; A ⇒ B |- B]
DEBUG: transformedProofs.map(_.simplify()).distinct took 1 ms and produced 1 terms out of 1 back-transformed terms; after rule ->R
DEBUG: returning 1 terms:
 b ⇒ a ⇒ c ⇒ a (b c) ,
 for sequent [E ⇒ A; A ⇒ B |- E ⇒ B]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->R
DEBUG: returning 1 terms:
 a ⇒ b ⇒ c ⇒ a (b c) ,
 for sequent [A ⇒ B |- (E ⇒ A) ⇒ E ⇒ B]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->R
DEBUG: returning 1 terms:
 a ⇒ b ⇒ c ⇒ a (b c) ,
 for sequent [ |- (A ⇒ B) ⇒ (E ⇒ A) ⇒ E ⇒ B]
DEBUG: for main sequent [ |- (A ⇒ B) ⇒ (E ⇒ A) ⇒ E ⇒ B], obtained 1 final proof terms:
 a ⇒ b ⇒ c ⇒ a (b c); score = ((),0,0.0,0.0,0): 0 unused args: Set(); unusedMatchClauseVars=0.0; unusedTupleParts=0; used tuple parts: List() . This took 63 ms
<console>:15: Returning term: \((a:A ⇒ B) ⇒ (b:E ⇒ A) ⇒ (c:E) ⇒ (a (b c)))
       def fmap[E, A, B]: (A ⇒ B) ⇒ (E ⇒ A) ⇒ (E ⇒ B) = implement
                                                        ^
<console>:15: Returning code: ((a: _root_.scala.Function1[A, B]) => ((b: _root_.scala.Function1[E, A]) => ((c: E) => a(b(c)))))
       def fmap[E, A, B]: (A ⇒ B) ⇒ (E ⇒ A) ⇒ (E ⇒ B) = implement
                                                        ^
fmap: [E, A, B]=> (A => B) => ((E => A) => (E => B))

scala> val f: Int ⇒ Int = _ + 10
f: Int => Int = <function1>

scala> def eaeb[E]: (E ⇒ Int) ⇒ (E ⇒ Int) = fmap(f)
eaeb: [E]=> (E => Int) => (E => Int)

scala> val ea: Double ⇒ Int = x ⇒ (x + 0.5).toInt
ea: Double => Int = <function1>

scala> eaeb(ea)(1.9)
res2: Int = 12
```

In this example, the returned lambda-calculus term is `(a ⇒ b ⇒ c ⇒ a (b c))`.

One can freely mix the curried and the conventional Scala function syntax.
Here is the applicative `map2` function for the Reader monad:

```scala
scala> def map2[E, A, B, C](readerA: E ⇒ A, readerB: E ⇒ B, f: A ⇒ B ⇒ C): E ⇒ C = implement
DEBUG: applied rule ->R to sequent [ |- (E ⇒ A) ⇒ (E ⇒ B) ⇒ (A ⇒ B ⇒ C) ⇒ E ⇒ C], new sequents [E ⇒ A |- (E ⇒ B) ⇒ (A ⇒ B ⇒ C) ⇒ E ⇒ C]
DEBUG: applied rule ->R to sequent [E ⇒ A |- (E ⇒ B) ⇒ (A ⇒ B ⇒ C) ⇒ E ⇒ C], new sequents [E ⇒ B; E ⇒ A |- (A ⇒ B ⇒ C) ⇒ E ⇒ C]
DEBUG: applied rule ->R to sequent [E ⇒ B; E ⇒ A |- (A ⇒ B ⇒ C) ⇒ E ⇒ C], new sequents [A ⇒ B ⇒ C; E ⇒ B; E ⇒ A |- E ⇒ C]
DEBUG: applied rule ->R to sequent [A ⇒ B ⇒ C; E ⇒ B; E ⇒ A |- E ⇒ C], new sequents [E; A ⇒ B ⇒ C; E ⇒ B; E ⇒ A |- C]
DEBUG: applied rule ->L1 to sequent [E; A ⇒ B ⇒ C; E ⇒ B; E ⇒ A |- C], new sequents [B; E; A ⇒ B ⇒ C; E ⇒ A |- C]
DEBUG: applied rule ->L1 to sequent [B; E; A ⇒ B ⇒ C; E ⇒ A |- C], new sequents [A; B; E; A ⇒ B ⇒ C |- C]
DEBUG: applied rule ->L1 to sequent [A; B; E; A ⇒ B ⇒ C |- C], new sequents [B ⇒ C; A; B; E |- C]
DEBUG: applied rule ->L1 to sequent [B ⇒ C; A; B; E |- C], new sequents [C; A; B; E |- C]
DEBUG: sequent [C; A; B; E |- C] followsFromAxioms: a ⇒ b ⇒ c ⇒ d ⇒ a
DEBUG: returning 1 terms:
 a ⇒ b ⇒ c ⇒ d ⇒ a ,
 for sequent [C; A; B; E |- C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 2 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 a ⇒ c ⇒ b ⇒ d ⇒ a b ,
 for sequent [B ⇒ C; A; B; E |- C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 b ⇒ c ⇒ d ⇒ a ⇒ a b c ,
 for sequent [A; B; E; A ⇒ B ⇒ C |- C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 1 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 d ⇒ c ⇒ a ⇒ b ⇒ a (b c) d ,
 for sequent [B; E; A ⇒ B ⇒ C; E ⇒ A |- C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: applied rule ->L1 to sequent [E; A ⇒ B ⇒ C; E ⇒ B; E ⇒ A |- C], new sequents [A; E; A ⇒ B ⇒ C; E ⇒ B |- C]
DEBUG: applied rule ->L1 to sequent [A; E; A ⇒ B ⇒ C; E ⇒ B |- C], new sequents [B ⇒ C; A; E; E ⇒ B |- C]
DEBUG: applied rule ->L1 to sequent [B ⇒ C; A; E; E ⇒ B |- C], new sequents [B; B ⇒ C; A; E |- C]
DEBUG: applied rule ->L1 to sequent [B; B ⇒ C; A; E |- C], new sequents [C; B; A; E |- C]
DEBUG: sequent [C; B; A; E |- C] followsFromAxioms: a ⇒ b ⇒ c ⇒ d ⇒ a
DEBUG: returning 1 terms:
 a ⇒ b ⇒ c ⇒ d ⇒ a ,
 for sequent [C; B; A; E |- C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 b ⇒ a ⇒ c ⇒ d ⇒ a b ,
 for sequent [B; B ⇒ C; A; E |- C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 a ⇒ d ⇒ c ⇒ b ⇒ a (b c) ,
 for sequent [B ⇒ C; A; E; E ⇒ B |- C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: applied rule ->L1 to sequent [A; E; A ⇒ B ⇒ C; E ⇒ B |- C], new sequents [B; A; E; A ⇒ B ⇒ C |- C]
DEBUG: applied rule ->L1 to sequent [B; A; E; A ⇒ B ⇒ C |- C], new sequents [B ⇒ C; B; A; E |- C]
DEBUG: applied rule ->L1 to sequent [B ⇒ C; B; A; E |- C], new sequents [C; B; A; E |- C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 a ⇒ b ⇒ c ⇒ d ⇒ a b ,
 for sequent [B ⇒ C; B; A; E |- C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 c ⇒ b ⇒ d ⇒ a ⇒ a b c ,
 for sequent [B; A; E; A ⇒ B ⇒ C |- C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 b ⇒ d ⇒ a ⇒ c ⇒ a b (c d) ,
 for sequent [A; E; A ⇒ B ⇒ C; E ⇒ B |- C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 c ⇒ a ⇒ d ⇒ b ⇒ a (b c) (d c) ,
 for sequent [E; A ⇒ B ⇒ C; E ⇒ B; E ⇒ A |- C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->R
DEBUG: returning 1 terms:
 a ⇒ d ⇒ b ⇒ c ⇒ a (b c) (d c) ,
 for sequent [A ⇒ B ⇒ C; E ⇒ B; E ⇒ A |- E ⇒ C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->R
DEBUG: returning 1 terms:
 d ⇒ b ⇒ a ⇒ c ⇒ a (b c) (d c) ,
 for sequent [E ⇒ B; E ⇒ A |- (A ⇒ B ⇒ C) ⇒ E ⇒ C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->R
DEBUG: returning 1 terms:
 b ⇒ d ⇒ a ⇒ c ⇒ a (b c) (d c) ,
 for sequent [E ⇒ A |- (E ⇒ B) ⇒ (A ⇒ B ⇒ C) ⇒ E ⇒ C]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->R
DEBUG: returning 1 terms:
 b ⇒ d ⇒ a ⇒ c ⇒ a (b c) (d c) ,
 for sequent [ |- (E ⇒ A) ⇒ (E ⇒ B) ⇒ (A ⇒ B ⇒ C) ⇒ E ⇒ C]
DEBUG: for main sequent [ |- (E ⇒ A) ⇒ (E ⇒ B) ⇒ (A ⇒ B ⇒ C) ⇒ E ⇒ C], obtained 1 final proof terms:
 b ⇒ d ⇒ a ⇒ c ⇒ a (b c) (d c); score = ((),0,0.0,0.0,1): 0 unused args: Set(); unusedMatchClauseVars=0.0; unusedTupleParts=0; used tuple parts: List() . This took 96 ms
<console>:15: Returning term: (((\((b:E ⇒ A) ⇒ (d:E ⇒ B) ⇒ (a:A ⇒ B ⇒ C) ⇒ (c:E) ⇒ ((a (b c)) (d c))) readerA) readerB) f)
       def map2[E, A, B, C](readerA: E ⇒ A, readerB: E ⇒ B, f: A ⇒ B ⇒ C): E ⇒ C = implement
                                                                                   ^
<console>:15: Returning code: ((b: _root_.scala.Function1[E, A]) => ((d: _root_.scala.Function1[E, B]) => ((a: _root_.scala.Function1[A, _root_.scala.Function1[B, C]]) => ((c: E) => a(b(c))(d(c))))))(readerA)(readerB)(f)
       def map2[E, A, B, C](readerA: E ⇒ A, readerB: E ⇒ B, f: A ⇒ B ⇒ C): E ⇒ C = implement
                                                                                   ^
map2: [E, A, B, C](readerA: E => A, readerB: E => B, f: A => (B => C))E => C
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

```scala
scala> ofType[User[Int, String]](123, (n: Int) ⇒ "id:" + (n * 100).toString)
DEBUG: applied rule ->R to sequent [ |- <c>Int ⇒ (<c>Int ⇒ <c>String) ⇒ User[<c>Int,<c>String]], new sequents [<c>Int |- (<c>Int ⇒ <c>String) ⇒ User[<c>Int,<c>String]]
DEBUG: applied rule ->R to sequent [<c>Int |- (<c>Int ⇒ <c>String) ⇒ User[<c>Int,<c>String]], new sequents [<c>Int ⇒ <c>String; <c>Int |- User[<c>Int,<c>String]]
DEBUG: applied rule _&R to sequent [<c>Int ⇒ <c>String; <c>Int |- User[<c>Int,<c>String]], new sequents [<c>Int ⇒ <c>String; <c>Int |- (<c>Int, <c>String)]
DEBUG: applied rule &R to sequent [<c>Int ⇒ <c>String; <c>Int |- (<c>Int, <c>String)], new sequents [<c>Int ⇒ <c>String; <c>Int |- <c>Int]; [<c>Int ⇒ <c>String; <c>Int |- <c>String]
DEBUG: sequent [<c>Int ⇒ <c>String; <c>Int |- <c>Int] followsFromAxioms: b ⇒ a ⇒ a
DEBUG: applied rule ->L1 to sequent [<c>Int ⇒ <c>String; <c>Int |- <c>Int], new sequents [<c>String; <c>Int |- <c>Int]
DEBUG: sequent [<c>String; <c>Int |- <c>Int] followsFromAxioms: b ⇒ a ⇒ a
DEBUG: returning 1 terms:
 b ⇒ a ⇒ a ,
 for sequent [<c>String; <c>Int |- <c>Int]
DEBUG: transformedProofs.map(_.simplify()).distinct took 1 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 b ⇒ a ⇒ a ,
 for sequent [<c>Int ⇒ <c>String; <c>Int |- <c>Int]
DEBUG: applied rule ->L1 to sequent [<c>Int ⇒ <c>String; <c>Int |- <c>String], new sequents [<c>String; <c>Int |- <c>String]
DEBUG: sequent [<c>String; <c>Int |- <c>String] followsFromAxioms: a ⇒ b ⇒ a
DEBUG: returning 1 terms:
 a ⇒ b ⇒ a ,
 for sequent [<c>String; <c>Int |- <c>String]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 a ⇒ b ⇒ a b ,
 for sequent [<c>Int ⇒ <c>String; <c>Int |- <c>String]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule &R
DEBUG: returning 1 terms:
 b ⇒ a ⇒ (a, b a) ,
 for sequent [<c>Int ⇒ <c>String; <c>Int |- (<c>Int, <c>String)]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule _&R
DEBUG: returning 1 terms:
 b ⇒ a ⇒ User(a, b a) ,
 for sequent [<c>Int ⇒ <c>String; <c>Int |- User[<c>Int,<c>String]]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->R
DEBUG: returning 1 terms:
 a ⇒ b ⇒ User(a, b a) ,
 for sequent [<c>Int |- (<c>Int ⇒ <c>String) ⇒ User[<c>Int,<c>String]]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->R
DEBUG: returning 1 terms:
 a ⇒ b ⇒ User(a, b a) ,
 for sequent [ |- <c>Int ⇒ (<c>Int ⇒ <c>String) ⇒ User[<c>Int,<c>String]]
DEBUG: for main sequent [ |- <c>Int ⇒ (<c>Int ⇒ <c>String) ⇒ User[<c>Int,<c>String]], obtained 1 final proof terms:
 a ⇒ b ⇒ User(a, b a); score = ((),0,0.0,0.0,1): 0 unused args: Set(); unusedMatchClauseVars=0.0; unusedTupleParts=0; used tuple parts: Vector() . This took 88 ms
<console>:18: Returning term: ((\((a:<c>Int) ⇒ (b:<c>Int ⇒ <c>String) ⇒ User(a, (b a))) arg1) arg2)
       ofType[User[Int, String]](123, (n: Int) ⇒ "id:" + (n * 100).toString)
                                ^
<console>:18: Returning code: ((a: Int) => ((b: _root_.scala.Function1[Int, String]) => User[Int, String](a, b(a))))(123)(((n: scala.Int) => "id:".+(n.*(100).toString())))
       ofType[User[Int, String]](123, (n: Int) ⇒ "id:" + (n * 100).toString)
                                ^
res3: User[Int,String] = User(123,id:12300)
```

The macro `ofType[T](x, y, ..., z)` generates an expression of type `T` built up from the given values `x`, `y`, ..., `z`.
The values `x`, `y`, ..., `z` can have any type (but their type must be known or specified at that point).

Unlike `implement`, the macro `ofType()` is designed to be used within expressions, and so we are required to write an explicit type parameter that designates the desired result type.
The macro `ofType()` will not work without specifying a type expression as its type parameter:

```scala
scala> val x: Int = ofType(123)
DEBUG: applied rule ->R to sequent [ |- <c>Int ⇒ 0], new sequents [<c>Int |- 0]
DEBUG: returning no terms for sequent [<c>Int |- 0]
DEBUG: transformedProofs.map(_.simplify()).distinct took 1 ms and produced 0 terms out of 0 back-transformed terms; after rule ->R
DEBUG: returning no terms for sequent [ |- <c>Int ⇒ 0]
DEBUG: for main sequent [ |- <c>Int ⇒ 0], obtained no final proof terms. This took 38 ms
<console>:15: error: type <c>Int ⇒ 0 cannot be implemented
       val x: Int = ofType(123)
                          ^
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

```scala
scala> def map[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ (S ⇒ (B, S)) = implement
DEBUG: applied rule ->R to sequent [ |- (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ S ⇒ (B, S)], new sequents [S ⇒ (A, S) |- (A ⇒ B) ⇒ S ⇒ (B, S)]
DEBUG: applied rule ->R to sequent [S ⇒ (A, S) |- (A ⇒ B) ⇒ S ⇒ (B, S)], new sequents [A ⇒ B; S ⇒ (A, S) |- S ⇒ (B, S)]
DEBUG: applied rule ->R to sequent [A ⇒ B; S ⇒ (A, S) |- S ⇒ (B, S)], new sequents [S; A ⇒ B; S ⇒ (A, S) |- (B, S)]
DEBUG: applied rule &R to sequent [S; A ⇒ B; S ⇒ (A, S) |- (B, S)], new sequents [S; A ⇒ B; S ⇒ (A, S) |- B]; [S; A ⇒ B; S ⇒ (A, S) |- S]
DEBUG: applied rule ->L1 to sequent [S; A ⇒ B; S ⇒ (A, S) |- B], new sequents [(A, S); S; A ⇒ B |- B]
DEBUG: applied rule &L to sequent [(A, S); S; A ⇒ B |- B], new sequents [A; S; S; A ⇒ B |- B]
DEBUG: applied rule ->L1 to sequent [A; S; S; A ⇒ B |- B], new sequents [B; A; S; S |- B]
DEBUG: sequent [B; A; S; S |- B] followsFromAxioms: a ⇒ b ⇒ c ⇒ d ⇒ a
DEBUG: returning 1 terms:
 a ⇒ b ⇒ c ⇒ d ⇒ a ,
 for sequent [B; A; S; S |- B]
DEBUG: transformedProofs.map(_.simplify()).distinct took 1 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 b ⇒ c ⇒ d ⇒ a ⇒ a b ,
 for sequent [A; S; S; A ⇒ B |- B]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule &L
DEBUG: returning 1 terms:
 b ⇒ c ⇒ a ⇒ a b._1 ,
 for sequent [(A, S); S; A ⇒ B |- B]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 1 terms:
 c ⇒ a ⇒ b ⇒ a b c._1 ,
 for sequent [S; A ⇒ B; S ⇒ (A, S) |- B]
DEBUG: sequent [S; A ⇒ B; S ⇒ (A, S) |- S] followsFromAxioms: a ⇒ b ⇒ c ⇒ a
DEBUG: applied rule ->L1 to sequent [S; A ⇒ B; S ⇒ (A, S) |- S], new sequents [(A, S); S; A ⇒ B |- S]
DEBUG: sequent [(A, S); S; A ⇒ B |- S] followsFromAxioms: b ⇒ a ⇒ c ⇒ a
DEBUG: applied rule &L to sequent [(A, S); S; A ⇒ B |- S], new sequents [A; S; S; A ⇒ B |- S]
DEBUG: sequent [A; S; S; A ⇒ B |- S] followsFromAxioms: b ⇒ a ⇒ c ⇒ d ⇒ a; b ⇒ c ⇒ a ⇒ d ⇒ a
DEBUG: applied rule ->L1 to sequent [A; S; S; A ⇒ B |- S], new sequents [B; A; S; S |- S]
DEBUG: sequent [B; A; S; S |- S] followsFromAxioms: b ⇒ c ⇒ a ⇒ d ⇒ a; b ⇒ c ⇒ d ⇒ a ⇒ a
DEBUG: returning 2 terms:
 b ⇒ c ⇒ a ⇒ d ⇒ a ;
 b ⇒ c ⇒ d ⇒ a ⇒ a ,
 for sequent [B; A; S; S |- S]
DEBUG: transformedProofs.map(_.simplify()).distinct took 4 ms and produced 2 terms out of 2 back-transformed terms; after rule ->L1
DEBUG: returning 2 terms:
 b ⇒ a ⇒ c ⇒ d ⇒ a ;
 b ⇒ c ⇒ a ⇒ d ⇒ a ,
 for sequent [A; S; S; A ⇒ B |- S]
DEBUG: transformedProofs.map(_.simplify()).distinct took 2 ms and produced 2 terms out of 2 back-transformed terms; after rule &L
DEBUG: returning 2 terms:
 b ⇒ a ⇒ c ⇒ a ;
 a ⇒ b ⇒ c ⇒ a._2 ,
 for sequent [(A, S); S; A ⇒ B |- S]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 2 terms out of 2 back-transformed terms; after rule ->L1
DEBUG: returning 2 terms:
 b ⇒ c ⇒ a ⇒ a b._2 ;
 a ⇒ b ⇒ c ⇒ a ,
 for sequent [S; A ⇒ B; S ⇒ (A, S) |- S]
DEBUG: transformedProofs.map(_.simplify()).distinct took 8 ms and produced 2 terms out of 2 back-transformed terms; after rule &R
DEBUG: returning 2 terms:
 c ⇒ a ⇒ b ⇒ (a b c._1, b c._2) ;
 c ⇒ a ⇒ b ⇒ (a b c._1, c) ,
 for sequent [S; A ⇒ B; S ⇒ (A, S) |- (B, S)]
DEBUG: transformedProofs.map(_.simplify()).distinct took 1 ms and produced 2 terms out of 2 back-transformed terms; after rule ->R
DEBUG: returning 2 terms:
 a ⇒ b ⇒ c ⇒ (a b c._1, b c._2) ;
 a ⇒ b ⇒ c ⇒ (a b c._1, c) ,
 for sequent [A ⇒ B; S ⇒ (A, S) |- S ⇒ (B, S)]
DEBUG: transformedProofs.map(_.simplify()).distinct took 1 ms and produced 2 terms out of 2 back-transformed terms; after rule ->R
DEBUG: returning 2 terms:
 b ⇒ a ⇒ c ⇒ (a b c._1, b c._2) ;
 b ⇒ a ⇒ c ⇒ (a b c._1, c) ,
 for sequent [S ⇒ (A, S) |- (A ⇒ B) ⇒ S ⇒ (B, S)]
DEBUG: transformedProofs.map(_.simplify()).distinct took 1 ms and produced 2 terms out of 2 back-transformed terms; after rule ->R
DEBUG: returning 2 terms:
 b ⇒ a ⇒ c ⇒ (a b c._1, b c._2) ;
 b ⇒ a ⇒ c ⇒ (a b c._1, c) ,
 for sequent [ |- (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ S ⇒ (B, S)]
DEBUG: for main sequent [ |- (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ S ⇒ (B, S)], obtained 2 final proof terms:
 b ⇒ a ⇒ c ⇒ (a b c._1, b c._2); score = ((),0,0.0,0.0,2): 0 unused args: Set(); unusedMatchClauseVars=0.0; unusedTupleParts=0; used tuple parts: List((a b,1), (a b,2)) ;
 b ⇒ a ⇒ c ⇒ (a b c._1, c); score = ((),0,1.0,0.0,1): 0 unused args: Set(); unusedMatchClauseVars=0.0; unusedTupleParts=1; used tuple parts: List((a b,1)) . This took 106 ms
<console>:15: warning: type (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ S ⇒ (B, S) has 2 implementations (laws need checking?):
 b ⇒ a ⇒ c ⇒ (a b c._1, b c._2) [score: ((),0,0.0,0.0,2)];
 b ⇒ a ⇒ c ⇒ (a b c._1, c) [score: ((),0,1.0,0.0,1)].
       def map[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ (S ⇒ (B, S)) = implement
                                                                 ^
<console>:15: Returning term: \((b:S ⇒ (A, S)) ⇒ (a:A ⇒ B) ⇒ (c:S) ⇒ ((a (b c)._1), (b c)._2))
       def map[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ (S ⇒ (B, S)) = implement
                                                                 ^
<console>:15: Returning code: ((b: _root_.scala.Function1[S, scala.Tuple2[A, S]]) => ((a: _root_.scala.Function1[A, B]) => ((c: S) => scala.Tuple2(a(b(c)._1), b(c)._2))))
       def map[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ (S ⇒ (B, S)) = implement
                                                                 ^
map: [S, A, B]=> (S => (A, S)) => ((A => B) => (S => (B, S)))
```

The warning shows that there exist two inequivalent implementations of the `map` function.
The first implementation was automatically chosen and returned as code.

The difference between the two implementations is that the initial state `s: S` can be either transformed using the given function `S ⇒ (A, S)`, or it can be left unchanged and returned in the pair `(B, S)`.
The first implementation is the correct functor instance for the State monad.
The second implementation "loses information" because the transformed value of type `S` has been computed and ignored.

It appears that information-losing functions are less likely to be useful in practice.
The implementation that is the lowest level of information loss will be more likely, for instance, to satisfy applicable algebraic laws.

In the hopes of producing a sensible and useful answer, the algorithm in `curryhoward` will choose the implementation that loses the least amount of information.
If there are several such implementations, no sensible choice is possible, and the macro will generate a compile-time error:


```scala
scala> def ff[A, B]: A ⇒ A ⇒ (A ⇒ B) ⇒ B = implement
DEBUG: applied rule ->R to sequent [ |- A ⇒ A ⇒ (A ⇒ B) ⇒ B], new sequents [A |- A ⇒ (A ⇒ B) ⇒ B]
DEBUG: applied rule ->R to sequent [A |- A ⇒ (A ⇒ B) ⇒ B], new sequents [A; A |- (A ⇒ B) ⇒ B]
DEBUG: applied rule ->R to sequent [A; A |- (A ⇒ B) ⇒ B], new sequents [A ⇒ B; A; A |- B]
DEBUG: applied rule ->L1 to sequent [A ⇒ B; A; A |- B], new sequents [B; A; A |- B]
DEBUG: sequent [B; A; A |- B] followsFromAxioms: a ⇒ b ⇒ c ⇒ a
DEBUG: returning 1 terms:
 a ⇒ b ⇒ c ⇒ a ,
 for sequent [B; A; A |- B]
DEBUG: transformedProofs.map(_.simplify()).distinct took 1 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: applied rule ->L1 to sequent [A ⇒ B; A; A |- B], new sequents [B; A; A |- B]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 1 terms out of 1 back-transformed terms; after rule ->L1
DEBUG: returning 2 terms:
 a ⇒ b ⇒ c ⇒ a b ;
 a ⇒ c ⇒ b ⇒ a b ,
 for sequent [A ⇒ B; A; A |- B]
DEBUG: transformedProofs.map(_.simplify()).distinct took 3 ms and produced 2 terms out of 2 back-transformed terms; after rule ->R
DEBUG: returning 2 terms:
 b ⇒ c ⇒ a ⇒ a b ;
 c ⇒ b ⇒ a ⇒ a b ,
 for sequent [A; A |- (A ⇒ B) ⇒ B]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 2 terms out of 2 back-transformed terms; after rule ->R
DEBUG: returning 2 terms:
 c ⇒ b ⇒ a ⇒ a b ;
 b ⇒ c ⇒ a ⇒ a b ,
 for sequent [A |- A ⇒ (A ⇒ B) ⇒ B]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 2 terms out of 2 back-transformed terms; after rule ->R
DEBUG: returning 2 terms:
 c ⇒ b ⇒ a ⇒ a b ;
 b ⇒ c ⇒ a ⇒ a b ,
 for sequent [ |- A ⇒ A ⇒ (A ⇒ B) ⇒ B]
DEBUG: for main sequent [ |- A ⇒ A ⇒ (A ⇒ B) ⇒ B], obtained 2 final proof terms:
 c ⇒ b ⇒ a ⇒ a b; score = ((),1,0.0,0.0,0): 1 unused args: Set(c); unusedMatchClauseVars=0.0; unusedTupleParts=0; used tuple parts: List() ;
 b ⇒ c ⇒ a ⇒ a b; score = ((),1,0.0,0.0,0): 1 unused args: Set(c); unusedMatchClauseVars=0.0; unusedTupleParts=0; used tuple parts: List() . This took 68 ms
<console>:15: error: type A ⇒ A ⇒ (A ⇒ B) ⇒ B can be implemented in 2 inequivalent ways:
 c ⇒ b ⇒ a ⇒ a b [score: ((),1,0.0,0.0,0)];
 b ⇒ c ⇒ a ⇒ a b [score: ((),1,0.0,0.0,0)].
       def ff[A, B]: A ⇒ A ⇒ (A ⇒ B) ⇒ B = implement
                                           ^
```

In this case, `allOfType[]()` might be useful.

# Using `allOfType`

The macro `allOfType` will find the implementations that have the lowest information loss, and return a sequence of these implementations if there are more than one.
User's code can then examine each of them and check laws or other properties, selecting the implementation with the desired properties.

As a simple example, consider a function of type `Int ⇒ Int ⇒ Int`: 

```scala
scala> val fs = allOfType[Int ⇒ Int ⇒ Int]
DEBUG: applied rule ->R to sequent [ |- <c>Int ⇒ <c>Int ⇒ <c>Int], new sequents [<c>Int |- <c>Int ⇒ <c>Int]
DEBUG: applied rule ->R to sequent [<c>Int |- <c>Int ⇒ <c>Int], new sequents [<c>Int; <c>Int |- <c>Int]
DEBUG: sequent [<c>Int; <c>Int |- <c>Int] followsFromAxioms: a ⇒ b ⇒ a; b ⇒ a ⇒ a
DEBUG: returning 2 terms:
 a ⇒ b ⇒ a ;
 b ⇒ a ⇒ a ,
 for sequent [<c>Int; <c>Int |- <c>Int]
DEBUG: transformedProofs.map(_.simplify()).distinct took 6 ms and produced 2 terms out of 2 back-transformed terms; after rule ->R
DEBUG: returning 2 terms:
 b ⇒ a ⇒ a ;
 a ⇒ b ⇒ a ,
 for sequent [<c>Int |- <c>Int ⇒ <c>Int]
DEBUG: transformedProofs.map(_.simplify()).distinct took 0 ms and produced 2 terms out of 2 back-transformed terms; after rule ->R
DEBUG: returning 2 terms:
 b ⇒ a ⇒ a ;
 a ⇒ b ⇒ a ,
 for sequent [ |- <c>Int ⇒ <c>Int ⇒ <c>Int]
DEBUG: for main sequent [ |- <c>Int ⇒ <c>Int ⇒ <c>Int], obtained 2 final proof terms:
 b ⇒ a ⇒ a; score = ((),1,0.0,0.0,0): 1 unused args: Set(b); unusedMatchClauseVars=0.0; unusedTupleParts=0; used tuple parts: List() ;
 a ⇒ b ⇒ a; score = ((),1,0.0,0.0,0): 1 unused args: Set(b); unusedMatchClauseVars=0.0; unusedTupleParts=0; used tuple parts: List() . This took 46 ms
<console>:15: Returning term: \((b:<c>Int) ⇒ (a:<c>Int) ⇒ a)
       val fs = allOfType[Int ⇒ Int ⇒ Int]
                         ^
<console>:15: Returning code: ((b: Int) => ((a: Int) => a))
       val fs = allOfType[Int ⇒ Int ⇒ Int]
                         ^
<console>:15: Returning term: \((a:<c>Int) ⇒ (b:<c>Int) ⇒ a)
       val fs = allOfType[Int ⇒ Int ⇒ Int]
                         ^
<console>:15: Returning code: ((a: Int) => ((b: Int) => a))
       val fs = allOfType[Int ⇒ Int ⇒ Int]
                         ^
fs: Seq[Int => (Int => Int)] = List(<function1>, <function1>)

scala> fs.map(f ⇒ f(1)(2))
res4: Seq[Int] = List(2, 1)
```

The list `fs` contains the two chosen implementations, both of them having equal levels of information loss.

These two implementations differ on which of the `Int` values is chosen as the result, the first one or the second one.
This is clear by looking at the function code printed above.

Both implementations could be desirable desired in different circumstances.
User code can be written to examine all available implementations and to select a desired one (at run time).

# Debugging and logging

The logging options are controlled via the JVM property `"curryhoward.log"`.
The value of this property is a comma-separated list of keywords.
The full logging is switched on by putting `-Dcurryhoward.log=macros,terms,prover` on the Java or SBT command line.

The `macros` logging option will print the code that the macro functions generate.
The `prover` logging option will print the steps in the proof search, including the new sequents generated by each applied rule.
The `trace` logging option will print more debugging information about the proof search.
The `terms` logging option will print the terms generated, in the short notation.

With none of these options given, only minimal diagnostic messages are printed, with terms in a short notation:

- information message when a term is returned
- warning message when several implementations are found with the same (lowest) information loss
