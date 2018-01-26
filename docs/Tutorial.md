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
<console>:17: Returning term: (a ⇒ b ⇒ User(a, b a)) userName userIdGenerator
       def makeUser[N, I](userName: N, userIdGenerator: N ⇒ I): User[N, I] = implement
                                                                             ^
makeUser: [N, I](userName: N, userIdGenerator: N => I)User[N,I]

scala> makeUser(123, (n: Int) ⇒ "id:" + (n * 100).toString)
res0: User[Int,String] = User(123,id:12300)
```

The library always prints the lambda-calculus term corresponding to the generated code.
In this example, the term is `User(userName, userIdGenerator userName)`.

The chosen notation for lambda-calculus terms supports tuples and named case classes.
Below we will see more examples of the generated terms printed using the lambda-calculus notation.

## Curried functions

The `curryhoward` library, of course, works with _curried_ functions as well:

```scala
scala> def const[A, B]: A ⇒ B ⇒ A = implement
<console>:15: Returning term: a ⇒ b ⇒ a
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
<console>:15: Returning term: a ⇒ b ⇒ c ⇒ a (b c)
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
<console>:15: Returning term: (a ⇒ b ⇒ c ⇒ d ⇒ c (a d) (b d)) readerA readerB f
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
<console>:18: Returning term: (a ⇒ b ⇒ User(a, b a)) arg1 arg2
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
<console>:15: error: type <c>Int ⇒ 0 cannot be implemented
       val x: Int = ofType(123)
                          ^
```

# Choosing between different possible implementations

Some types have more than one implementation.

There are many ways in which this can happen:

1. The type involves a function with several arguments of the same type, for example `X ⇒ X ⇒ ...`.
If this type can be implemented, there will be at least two implementations that differ only by the choice of the argument of type `X`.
This ambiguity cannot be resolved in any reasonable way, except by making types different.
2. One implementation ignores some function argument(s), while another does not.
For example, the "Church numeral" type `(X ⇒ X) ⇒ X ⇒ X` can be implemented in infinitely many ways: `_ ⇒ x ⇒ x`, `f ⇒ x ⇒ f x`, `f ⇒ x ⇒ f (f x)`, etc. Of these ways, the most likely candidate is `f ⇒ x ⇒ f x` because it is the identity on the type `X ⇒ X`, which was most likely what is intended here. The implementation `_ ⇒ x ⇒ x` should be probably rejected because it ignores some part of the given input.
3. Some arguments can be used more than once, and different implementations use them differently. Again, the "Church numerals" are an example.
The type `(X ⇒ X) ⇒ X ⇒ X` can be implemented as `f ⇒ x ⇒ f x` or as `f ⇒ x ⇒ f (f x)` or as `f ⇒ x ⇒ f (f (f x))`, etc.
There are infinitely many possible implementations that differ in how many times the argument `f` was used.
In this example, probably the desired implementation is that which uses `f` only once.
4. The type involves a tuple or a case class with several parts of the same type.
Implementing such a type will always have an ordering ambiguity.
For example, the type `(X, X, X) ⇒ (X, X, X)` can be implemented as `a ⇒ (a._1, a._2, a._3)` or as `a ⇒ (a._2, a._1, a._3)` or as `a ⇒ (a._1, a._1, a._1)`, etc.
In this example, the first implementation (preserving the ordering) is probably the desired one.
It is, however, not clear what implementation is desired for a type such as `(X, X, X, X) ⇒ (X, X)`.
5. The type involves a disjunction with several parts of the same type.
Implementing such a type will always have an ordering ambiguity.
For example, the type `Either[X, X] ⇒ Either[X, X]` can be implemented in four ways that differ by the ordering of the parts of the disjunctions: 

```scala
def f1[X]: Either[X, X] ⇒ Either[X, X] = { // identity[Either[X, X]]
  case Left(x) ⇒ Left(x)
  case Right(x) ⇒ Right(x)
}
def f2[X]: Either[X, X] ⇒ Either[X, X] = { // switch left and right
  case Left(x) ⇒ Right(x)
  case Right(x) ⇒ Left(x)
}
def f3[X]: Either[X, X] ⇒ Either[X, X] = { // always return left
  case Left(x) ⇒ Left(x)
  case Right(x) ⇒ Left(x)
}
def f4[X]: Either[X, X] ⇒ Either[X, X] = { // always return right
  case Left(x) ⇒ Right(x)
  case Right(x) ⇒ Right(x)
}

```

In this example, the implementation `f1` is probably the desired one.

The `curryhoward` library implements the "information loss" heuristic for choosing the "most sensible" implementation when there are several possibilities.

As an example, consider the `map` function for the State monad:

```scala
scala> def map[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ (S ⇒ (B, S)) = implement
<console>:15: warning: type (S ⇒ Tuple2[A,S]) ⇒ (A ⇒ B) ⇒ S ⇒ Tuple2[B,S] has 2 implementations (laws need checking?):
 a ⇒ b ⇒ c ⇒ Tuple2(b a c._1, a c._2) [score: (0,0.0,0.0,2,2)];
 a ⇒ b ⇒ c ⇒ Tuple2(b a c._1, c) [score: (0,1.0,0.0,1,1)].
       def map[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ (S ⇒ (B, S)) = implement
                                                                 ^
<console>:15: Returning term: a ⇒ b ⇒ c ⇒ Tuple2(b a c._1, a c._2)
       def map[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ (S ⇒ (B, S)) = implement
                                                                 ^
map: [S, A, B]=> (S => (A, S)) => ((A => B) => (S => (B, S)))
```

The warning shows that there exist two inequivalent implementations of the `map` function.
The first implementation was automatically chosen and returned as code.

The difference between the two implementations is that the initial state `s: S` can be either transformed using the given function `S ⇒ (A, S)`, or it can be left unchanged and returned in the pair `(B, S)`.
The first implementation is the correct functor instance for the State monad.
The second implementation "loses information" because the transformed value of type `S` has been computed but then ignored.

The warning message lists all found implementations together with their "information loss score"

It appears that information-losing functions are less likely to be useful in practice.
The implementation having the smallest "information loss score" will be more likely, for instance, to satisfy equational laws.

In the hopes of producing a sensible and useful answer, `curryhoward` will choose the implementation with the smallest information loss score.
If there are several such implementations then no automatic choice is possible, and the macro will generate a compile-time error:

```scala
scala> def ff[A, B]: A ⇒ A ⇒ (A ⇒ B) ⇒ B = implement
<console>:15: error: type A ⇒ A ⇒ (A ⇒ B) ⇒ B can be implemented in 2 inequivalent ways:
 a ⇒ b ⇒ c ⇒ c b [score: (1,0.0,0.0,0,0)];
 a ⇒ b ⇒ c ⇒ c a [score: (1,0.0,0.0,0,0)].
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
<console>:15: Returning term: a ⇒ b ⇒ b
       val fs = allOfType[Int ⇒ Int ⇒ Int]
                         ^
<console>:15: Returning term: a ⇒ b ⇒ a
       val fs = allOfType[Int ⇒ Int ⇒ Int]
                         ^
fs: Seq[io.chymyst.ch.Function1Lambda[Int,Int => Int]] = List(<function1>, <function1>)

scala> fs.map(f ⇒ f(1)(2))
res4: Seq[Int] = List(2, 1)
```

The list `fs` contains the two chosen implementations, both of them having equal levels of "information loss".

These two implementations differ on which of the `Int` values is chosen as the result, the first one or the second one.
This is clear by looking at the function code printed above.

Both implementations could be desirable in different circumstances.
User code can be written to examine all available implementations and to select a desired one (at run time).

# Working with lambda-terms

An experimental API is provided to inspect lambda-terms generated by `allOfType` and `ofType` methods.

Lambda-terms are values of type `TermExpr`, which is a sealed trait with case classes such as `VarE`, `AppE`, `ConjunctE` and so on,
representing the different constructions of the simply-typed lambda-calculus (STLC).

When the macros `allOfType` and `ofType` are used to create a function type such as `A ⇒ B ⇒ C`, the returned value is actually of a special subclass of `Function1`, `Function2`, or `Function3`.
These subclasses are called `Function1Lambda`, `Function2Lambda`, `Function3Lambda` and carry not only the function's compiled code but also the STLC term corresponding to the function's code.

## When are lambda-terms available?

An STLC term is generated only if:

- an expression is generated of a pure function type (not function applied to arguments) using `ofType` or `allOfType`, and
- the generated STLC data structure is not too large (or else the JVM method size limit will break compilation).

The STLC term can be extracted using one of the two methods:

- `TermExpr.lambdaTerm : Any ⇒ Option[TermExpr]`. This is a safe way of finding out whether an expression has an associated STLC term.
- A syntax extension `.lambdaTerm` on `Any`. This is unsafe, since it will throw an exception if an expression does not have an associated STLC term.

Consider the function of type `Int ⇒ Int ⇒ Int` whose implementations we have just computed as `fs`.
Let us now look at the STLC terms corresponding to these implementations:

```scala
scala> fs(0).lambdaTerm
res5: io.chymyst.ch.TermExpr = \((a:<c>Int) ⇒ (b:<c>Int) ⇒ b)

scala> fs(1).lambdaTerm
res6: io.chymyst.ch.TermExpr = \((a:<c>Int) ⇒ (b:<c>Int) ⇒ a)
```

## What can we do with lambda-terms?

There are several ways in which we can use lambda-terms:

- print them in shorter or in longer form:

```scala
scala> fs(0).lambdaTerm.prettyPrint
res7: String = a ⇒ b ⇒ b

scala> fs(0).lambdaTerm.toString
res8: String = \((a:<c>Int) ⇒ (b:<c>Int) ⇒ b)
```

- perform symbolic computations with the lambda-terms, e.g. apply functions to arguments and simplify the resulting terms
- create fresh new symbolic variables of arbitrary types, as well as new symbolic lambda-terms (e.g. functions), for use in STLC computations
- check whether two lambda-terms are (syntactically) identical after simplification

To illustrate these facilities, let us determine which of the implementations `fs(0)` or `fs(1)` satisfies the law `f(x)(y) = x` for all `x` and `y`.
We could write unit tests (e.g. using `scalacheck`) for verifying this law on specific values of `x` and `y`, but this will take longer and will never give the same level of assurance as a symbolic mathematical proof.

To perform this symbolic computation, we need to create two symbolic variables `x` and `y` of type `Int`.
(Note that in STLC all expressions and all variables must have assigned types.)

Creating symbolic variables with known types is easy with the macro `freshVar`:

```scala
scala> val x = freshVar[Int]
x: io.chymyst.ch.VarE = x$1

scala> val y = freshVar[Int]
y: io.chymyst.ch.VarE = y$2
```

Now we can apply `fs(0)` and `fs(1)` to these variables and obtain the resulting symbolic terms:

```scala
scala> val results = fs.map ( f ⇒ f.lambdaTerm(x)(y) )    
results: Seq[io.chymyst.ch.TermExpr] = List(((\((a:<c>Int) ⇒ (b:<c>Int) ⇒ b) x$1) y$2), ((\((a:<c>Int) ⇒ (b:<c>Int) ⇒ a) x$1) y$2))
```

Note that the results are unevaluated STLC terms representing function applications:

```scala
scala> results.map(_.prettyPrint)
res9: Seq[String] = List((a ⇒ b ⇒ b) c d, (a ⇒ b ⇒ a) c d)
```

We can use the `.simplify` method to perform symbolic evaluation of these terms:

```scala
scala> results.map(_.simplify)
res10: Seq[io.chymyst.ch.TermExpr] = List(y$2, x$1)
```

To determine whether the required law holds, we can use the `.equiv` method that automatically performs simplification:

```scala
scala> results.filter(r ⇒ r.equiv(x))
res11: Seq[io.chymyst.ch.TermExpr] = List(((\((a:<c>Int) ⇒ (b:<c>Int) ⇒ a) x$1) y$2))
```

This leaves only one implementation that satisfies the law.

We can now rewrite this computation working directly on the functions `fs(0)` and `fs(1)` and selecting the one that satisfies the law:

```scala
scala> val goodF = fs.find { f ⇒ x equiv f.lambdaTerm(x)(y) }.get
goodF: io.chymyst.ch.Function1Lambda[Int,Int => Int] = <function1>

scala> goodF(123)(456)
res12: Int = 123
```

## How to use lambda-terms with type parameters?

Symbolic computations work also on expressions with type parameters, with some caveats.
The main limitation is that Scala values cannot be parameterized by types.
Also, STLC is not polymorphic and does not support type variables directly and naturally.
Nevertheless, these limitations can be overcome with some more work.

To consider an easy example, let us generate the functor method `fmap` for the parameterized type `Either[Int, T]`, and then verify the identity law using symbolic computations with lambda-terms.

We begin by auto-generating `fmap` using the `ofType` method. (The `implement` method will not generate lambda-terms.)

```scala
scala> def fmap[A, B] = ofType[ (A ⇒ B) ⇒ Either[Int, A] ⇒ Either[Int, B] ] 
<console>:15: Returning term: a ⇒ b ⇒ b match { c ⇒ (Left(c.value) + 0); d ⇒ (0 + Right(a d.value)) }
       def fmap[A, B] = ofType[ (A ⇒ B) ⇒ Either[Int, A] ⇒ Either[Int, B] ]
                              ^
fmap: [A, B]=> io.chymyst.ch.Function1Lambda[A => B,Either[Int,A] => scala.util.Either[Int,B]]

scala> val fmapT = fmap.lambdaTerm // No need to specify type parameters.
fmapT: io.chymyst.ch.TermExpr = \((a:A ⇒ B) ⇒ (b:Either[<c>Int,A]{Left[<c>Int,A] + Right[<c>Int,A]}) ⇒ (b match { \((c:Left[<c>Int,A]) ⇒ (Left(c.value) + 0)); \((d:Right[<c>Int,A]) ⇒ (0 + Right((a d.value))))}))

scala> fmapT.prettyPrint
res13: String = a ⇒ b ⇒ b match { c ⇒ (Left(c.value) + 0); d ⇒ (0 + Right(a d.value)) }
```

We have thus extracted the STLC term `fmapT` corresponding to the generated code of `fmap`. 

Note that the type parameter names `A` and `B` in the term `fmapT` are fixed by the macro `ofType` at compile time.
While the Scala method `fmap` has type parameters and can be called `fmap[Int, String]` and so on,
the term `fmapT` has fixed STLC type names `A` and `B`, which we cannot change by specifying Scala type parameters.
For this reason, we will have to manipulate these names explicitly in our symbolic computations.

The identity law is `fmap id = id`. To verify this law, we need to apply `fmap` to an identity function of type `A ⇒ A`, and to check that the result is an identity function of type `Either[Int, A] ⇒ Either[Int, A]`.

We create an identity function by first creating an STLC variable of type `A`, and then creating a function expression:

```scala
scala> def a[A] = freshVar[A]
a: [A]=> io.chymyst.ch.VarE

scala> val idA = a #> a
idA: io.chymyst.ch.TermExpr = \((a$3:A) ⇒ a$3)
```

The type parameter name `A` is fixed in the variable `a` since it is defined via a macro.
Calling `a[Int]` will return the same variable, still having type `A`.

Let us now apply the lambda-term `fmapT` to `idA`.

```scala
scala> val result = fmapT(idA)
java.lang.Exception: Internal error: Invalid head type in application (\((a:A ⇒ B) ⇒ (b:Either[<c>Int,A]{Left[<c>Int,A] + Right[<c>Int,A]}) ⇒ (b match { \((c:Left[<c>Int,A]) ⇒ (Left(c.value) + 0)); \((d:Right[<c>Int,A]) ⇒ (0 + Right((a d.value))))})) \((a$3:A) ⇒ a$3)): `(A ⇒ B) ⇒ Either[<c>Int,A]{Left[<c>Int,A] + Right[<c>Int,A]} ⇒ Either[<c>Int,B]{Left[<c>Int,B] + Right[<c>Int,B]}` must be a function with argument type `A ⇒ A`
  at io.chymyst.ch.AppE.<init>(TermExpr.scala:391)
  at io.chymyst.ch.TermExpr.apply(TermExpr.scala:233)
  at io.chymyst.ch.TermExpr.apply$(TermExpr.scala:233)
  at io.chymyst.ch.CurriedE.apply(TermExpr.scala:412)
  ... 44 elided
```

We get an error because `fmapT` expects an argument of type `A ⇒ B`, while we are giving it an argument `idA` of type `A ⇒ A`.
In Scala, the compiler would have automatically set `B = A` and resolved the types.
But the STLC evaluation right now does not support type variables directly in this way.

We need to rename the type variable `B` into `A` within the term `fmapT`.
To do this, we can use the method `substTypeVar`.

In order to get a handle on the type variables, we need to define some STLC terms (e.g. fresh variables) that have types `A` and `B`.
We already have `a` of type `A`.
The type expression of a lambda-term is obtained via the method `.tExpr`:

```scala
scala> a.tExpr
res14: io.chymyst.ch.TypeExpr = TP(A)
```

The type expression printed as `TP("A")` represents the type parameter with name `A`.
Let us define a variable `b` of type `B` and extract its type:

```scala
scala> def b[B] = freshVar[B]
b: [B]=> io.chymyst.ch.VarE

scala> b.tExpr
res15: io.chymyst.ch.TypeExpr = TP(B)
```

Now we can rename the type variables in `fmapT` and apply the resulting term to `idA` like this:

```scala
scala> val fmapAA = fmapT.substTypeVar(b, a)
fmapAA: io.chymyst.ch.TermExpr = \((a:A ⇒ A) ⇒ (b:Either[<c>Int,A]{Left[<c>Int,A] + Right[<c>Int,A]}) ⇒ (b match { \((c:Left[<c>Int,A]) ⇒ (Left(c.value) + 0)); \((d:Right[<c>Int,A]) ⇒ (0 + Right((a d.value))))}))

scala> val f2 = fmapAA(idA)
f2: io.chymyst.ch.TermExpr = (\((a:A ⇒ A) ⇒ (b:Either[<c>Int,A]{Left[<c>Int,A] + Right[<c>Int,A]}) ⇒ (b match { \((c:Left[<c>Int,A]) ⇒ (Left(c.value) + 0)); \((d:Right[<c>Int,A]) ⇒ (0 + Right((a d.value))))})) \((a$3:A) ⇒ a$3))
```

It remains to show that the STLC term `f2` is an identity function of type `Either[Int, A] ⇒ Either[Int, A]`.
We can first check that the type of this term is what we expect:

```scala
scala> f2.tExpr.prettyPrint
res16: String = Either[<c>Int,A]{Left[<c>Int,A] + Right[<c>Int,A]} ⇒ Either[<c>Int,A]{Left[<c>Int,A] + Right[<c>Int,A]}
```

The most straightforward way of verifying that `f2` is an identity function is to apply it to an arbitrary term of type `Either[Int, A]`.
Let us define a new variable of that type and apply `f2` to that variable.

```scala
scala> def optA[A] = freshVar[Either[Int, A]]
optA: [A]=> io.chymyst.ch.VarE

scala> f2(optA).simplify
res17: io.chymyst.ch.TermExpr = optA$5
```

We see that, after simplification, we obtain the original term `optA`.
We can verify this more quickly with

```scala
scala> optA equiv f2(optA)
res18: Boolean = true
```

This concludes the verification of the identity law.

## Summary of the lambda-term API

| Function  | Type  | Comment  |
|---|---|---|
| `TermExpr.lambdaTerm`  | `Any ⇒ Option[TermExpr]` | extract a lambda-term if present  |
| `a.lambdaTerm`  | `Any ⇒ TermExpr`  | extract a lambda-term, throw exception if not present  |
| `t.prettyPrint` | `TermExpr ⇒ String` and `TypeExpr ⇒ String` | produce a more readable string representation than `.toString` |
| `a.tExpr` | `TermExpr ⇒ TypeExpr` | get the type expression for a given term |
| `freshVar[T]` | `VarE` | create a STLC variable with assigned type expression `T` |
| `a #> b` | `VarE ⇒ TermExpr ⇒ TermExpr` | create a STLC function term ("abstraction") |
| `a(b)` | `TermExpr ⇒ TermExpr ⇒ TermExpr` | create a STLC application term -- the type of `b` must be the same as the argument type of `a` |
| `a.simplify` | `TermExpr ⇒ TermExpr` | perform symbolic simplification of STLC term
| `a equiv b` | `TermExpr ⇒ TermExpr ⇒ Boolean` | check whether two terms are syntactically equal after simplification |
| `a.substTypeVar(b, c)` | `TermExpr ⇒ (TermExpr, TermExpr) ⇒ TermExpr` | replace the type of `b` by the type of `c` in `a` -- the type of `b` must be a type variable |
