<link href="{{ site.github.url }}/tables.css" rel="stylesheet" />

## Setup

First, declare this library dependency in your `build.sbt`:

```scala
libraryDependencies += "io.chymyst" %% "curryhoward" % "latest.integration"

```

The `curryhoward` functionality becomes available once you add this import declaration:

```tut:silent
import io.chymyst.ch._
```

This imports all the necessary symbols such as `implement`, `ofType`, `allOfType` and so on.

In this tutorial, we will activate the `verbose` logging option:

```tut:silent
System.setProperty("curryhoward.log", "verbose")
```

The value of the `"curryhoward.log"` JVM property is a comma-separated list of options.
Other supported logging options include `macros`, `terms`, and `prover`.

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

With the `verbose` option set, the library will print the lambda-calculus term corresponding to the generated code.
In this example, the term is `User(userName, userIdGenerator userName)`.

The chosen notation for lambda-calculus terms supports tuples and named case classes.
Below we will see more examples of the generated terms printed using the lambda-calculus notation.

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

```tut
def map[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ (S ⇒ (B, S)) = implement
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

```tut:fail
def ff[A, B]: A ⇒ A ⇒ (A ⇒ B) ⇒ B = implement
```

In this case, `allOfType[]()` might be useful.

# Using `allOfType`

The macro `allOfType` will find the implementations that have the lowest information loss, and return a sequence of these implementations if there are more than one.
User's code can then examine each of them and check laws or other properties, selecting the implementation with the desired properties.

As a simple example, consider a function of type `Int ⇒ Int ⇒ Int`: 

```tut
val fs = allOfType[Int ⇒ Int ⇒ Int]
fs.map(f ⇒ f(1)(2))
```

The list `fs` contains the two chosen implementations, both of them having equal levels of "information loss".

These two implementations differ on which of the `Int` values is chosen as the result, the first one or the second one.
This is clear by looking at the function code printed above.

Both implementations could be desirable in different circumstances.
User code can be written to examine all available implementations and to select a desired one (at run time).

## Summary of the core API

| Function  | Type  | Comment  |
|---|---|---|
| `implement`  | `[T] ⇒ T` | implement an expression of type specified at the left-hand side; lambda-terms are not attached |
| `ofType[T]`  | `[T] ⇒ T`  | implement an expression of type specified as the type parameter; if the given type is a function type, attach the lambda-term to the function |
| `ofType[T](x...)`  | `[T] ⇒ Any* ⇒ T`  | implement an expression of type specified as the type parameter, using given values `x...` if necessary |
| `allOfType[T]`  | `[T] ⇒ Seq[T]`  | return all least-lossy implementations for an expression of type specified as the type parameter; if the given type is a function type, attach the lambda-terms to the resulting functions |
| `allOfType[T](x...)`  | `[T] ⇒ Any* ⇒ Seq[T]`  | return all least-lossy implementations for an expression of type specified as the type parameter; the given values `x...` can be used if necessary |


# Working with lambda-terms

An API is provided to inspect lambda-terms generated by `anyOfType`, `allOfType`, and `ofType` methods (but not by the `implement` method).

Lambda-terms are values of type `TermExpr`, which is a sealed trait with case classes such as `VarE`, `AppE`, `ConjunctE` and so on,
representing the different constructions of the simply-typed lambda-calculus (STLC).

All values of type `TermExpr` have the method `.t` that yields their type as a `TypeExpr` value.
`TypeExpr` is another sealed trait with case classes such as `BasicT` (representing a concrete, non-parameter type), `DisjunctT` and so on.

The version of STLC used in `curryhoward` includes the following features:

- variables, curried anonymous functions, function applications (this is standard in lambda-calculus)
- type expressions distinguish between type variables and ground types
- named conjunctions and named disjunctions; Scala tuples are named as in the standard library (e.g. `Tuple2` with accessors `_1`, `_2`)
- Java-style argument groups (e.g. `def f(x: A, y: B, z: C): D`) are supported as unnamed conjunctions, as opposed to tupled arguments `def f(t: (A, B, C)): D` or curried functions `def f: A ⇒ B ⇒ C ⇒ D` 

The syntax of STLC uses non-standard conventions for disjunctions:

- Disjunction types must be named, and each part of a disjunction must be a named conjunction.
- Disjunction types are printed with all top-level names, for example the type `Either[Int, A]` is printed as `Either[<c>Int,A]{Left[<c>Int,A] + Right[<c>Int,A]}`.
- Disjunction values are printed in the notation `x + 0` or `0 + x + 0`, etc., where `0` signifies the missing parts of the disjunction. For example, the concrete value `Left(x)` is printed as `Left(x) + 0`.
- The `match / case` expression is printed as `x match {a ⇒ ...; b ⇒ ...}` where each function after `match` represents a different `case` clause.   

## When are lambda-terms available?

When the macros `anyOfType`, `allOfType`, or `ofType` are used to create a function type such as `A ⇒ B ⇒ C`, the returned value is actually of a special subclass of `Function1`, `Function2`, or `Function3`.
These subclasses are called `Function1Lambda`, `Function2Lambda`, `Function3Lambda` and carry not only the function's compiled code but also the STLC term corresponding to the function's code.

An STLC term is generated only if:

- an expression is generated of a pure function type (not function applied to arguments) using `ofType[...]` or `allOfType[...]` or `anyOfType[...]()` without value arguments, and
- the generated STLC data structure is not too large (or else the JVM method size limit will break compilation).

The STLC term can be extracted using one of the two methods:

- `TermExpr.lambdaTerm : Any ⇒ Option[TermExpr]`. This is a safe way of finding out whether an expression has an associated STLC term.
- A syntax extension `.lambdaTerm` on `Any`. This is unsafe, since it will throw an exception if an expression does not have an associated STLC term.

Consider the function of type `Int ⇒ Int ⇒ Int` whose implementations we have just computed as `fs`.
Let us now look at the STLC terms corresponding to these implementations:

```tut
fs(0).lambdaTerm
fs(1).lambdaTerm

```

When using the `implement` macro, STLC terms are not generated.
This is because `implement` is intended for "production" use.

## What can we do with lambda-terms?

There are several ways in which we can use lambda-terms:

- print them in shorter or in longer form:

```tut
fs(0).lambdaTerm.prettyPrint
fs(0).lambdaTerm.toString
```

- perform symbolic computations with the lambda-terms, e.g. apply functions to arguments and simplify the resulting terms
- create fresh new symbolic variables of arbitrary types, as well as new symbolic lambda-terms (e.g. functions), for use in STLC computations
- check whether two lambda-terms are (syntactically) identical after simplification and renaming variables

To illustrate these facilities, let us determine which of the implementations `fs(0)` or `fs(1)` satisfies the law `f(x)(y) = x` for all `x` and `y`.
We could write unit tests (e.g. using `scalacheck`) for verifying this law on specific values of `x` and `y`, but this will take longer and will never give the same level of assurance as a symbolic mathematical proof.

To perform this symbolic computation, we need to create two symbolic variables `x` and `y` of type `Int`.
(Note that in STLC all expressions and all variables must have assigned types.)

Creating symbolic variables with known types is easy with the macro `freshVar`:

```tut
val x = freshVar[Int]
val y = freshVar[Int]
```

Now we can apply `fs(0)` and `fs(1)` to these variables and obtain the resulting symbolic terms:

```tut
val results = fs.map ( f ⇒ f.lambdaTerm(x)(y) )    
```

Note that the results are unevaluated STLC terms representing function applications:

```tut
results.map(_.prettyPrint)
```

We can use the `.simplify` method to perform symbolic evaluation of these terms:

```tut
results.map(_.simplify)
```

To determine whether the required law holds, we can use the `.equiv` method that automatically performs simplification:

```tut
results.filter(r ⇒ r equiv x)
```

This leaves only one implementation that satisfies the law.

We can now rewrite this computation working directly on the functions `fs(0)` and `fs(1)` and selecting the one that satisfies the law:

```tut
val goodF = fs.find { f ⇒ x equiv f.lambdaTerm(x)(y) }.get
```

Now we can use the good implementation:

```tut
goodF(123)(456)
```

## How to use lambda-terms with type parameters?

Symbolic computations work also on expressions with type parameters, with some caveats.
The main limitation is that Scala values cannot be parameterized by types.
Also, the currently implemented STLC is not polymorphic and does not support type variables directly and naturally.
Nevertheless, these limitations can be overcome with some more work.

To consider an easy example, let us generate the functor method `fmap` for the parameterized type `Either[Int, T]`, and then verify the identity law using symbolic computations with lambda-terms.

We begin by auto-generating `fmap` using the `ofType` method. (The `implement` method will never generate lambda-terms.)

```tut
def fmap[A, B] = ofType[ (A ⇒ B) ⇒ Either[Int, A] ⇒ Either[Int, B] ] 

val fmapT = fmap.lambdaTerm // No need to specify type parameters here.
fmapT.prettyPrint
```

We have thus extracted the STLC term `fmapT` corresponding to the generated code of `fmap`. 

Note that the type parameter names `A` and `B` in the term `fmapT` are fixed by the macro `ofType` at compile time.
While the Scala method `fmap` has type parameters and can be called `fmap[Int, String]` and so on,
the STLC term `fmapT` has fixed STLC type names `A` and `B`, which we cannot change by specifying Scala type parameters.
For this reason, we will have to manipulate these names explicitly in our symbolic computations.

The identity law is `fmap id = id`. To verify this law, we need to apply `fmap` to an identity function of type `A ⇒ A`, and to check that the result is an identity function of type `Either[Int, A] ⇒ Either[Int, A]`.

We create an identity function by first creating an STLC variable of type `A`, and then creating a function expression using the operator `=>:`:

```tut
def a[A] = freshVar[A]
val idA = a =>: a
```

The type parameter name `A` is fixed in the variable `a` since it is defined at compile time by the `freshVar` macro.
For the same reason, calling `a[Int]` will return the same variable, still having type `A`.
Repeated calls to `a` will also return the same variable.

The operator `=>:` is right-associative:

```tut
val b = freshVar[Int]
a =>: b =>: a
```

Let us now apply the lambda-term `fmapT` to `idA`.

```tut:fail
val result = fmapT(idA)
```

We get an error because `fmapT` expects an argument of type `A ⇒ B`, while we are giving it an argument `idA` of type `A ⇒ A`.
In Scala, the compiler would have automatically set `B = A` and resolved the types.
But the STLC evaluation right now does not support type variables directly in this way.

We need to reassign the type variable `B` into `A` within the term `fmapT` when we apply `fmapT` to `idA`.
The general methods `.substTypeVar` and `.substTypeVars` are available for this purpose; however, these methods are somewhat cumbersome to use. 

To do the type variable reassignment easier, we can use the method `:@` like this:

```tut
val f2 = fmapT :@ idA
f2.t.prettyPrint
```

You can see from the type of `f2` that the type variables in `fmapT` have been automatically adjusted to match the given argument `idA`.
The type of `f2` is `Either[Int, A] ⇒ Either[Int, A]`

It remains to show that the STLC term `f2` is actually equal to an identity function of type `Either[Int, A] ⇒ Either[Int, A]`.

The most straightforward way of verifying that `f2` is an identity function is to apply it to an arbitrary term of type `Either[Int, A]`.
Let us define a new variable of that type and apply `f2` to that variable.

```tut
def optA[A] = freshVar[Either[Int, A]]
f2(optA).simplify
```

We see that, after simplification, we obtain the original term `optA`.
We can check this by using the `.equiv()` method:

```tut
assert(optA equiv f2(optA))
```

This concludes the verification of the identity law.
Here is the entire code once again, slightly shorter:

```scala
def fmap[A, B] = ofType[(A ⇒ B) ⇒ Either[Int, A] ⇒ Either[Int, B]]
val fmapT = fmap.lambdaTerm
def a[A] = freshVar[A]
def optA[A] = freshVar[Either[Int, A]]
assert((fmapT :@ (a =>: a))(optA) equiv optA)

```

We have seen that the operator `:@` helps reassign type variables when checking algebraic laws.
Another often used operation that needs type variable reassignment is the function composition.
The operators `:@@` and `@@:` are available to make this easier.

In the operators `:@`, `:@@`, and `@@:`, the colon `:` mnemonically shows the side that will automatically adjust its type.

To illustrate the use of these operators, consider the function `pure`, which is standard for the `Either` monad and can be implemented automatically:

```tut
def pure[A] = ofType[A ⇒ Either[Int, A]].lambdaTerm
```

Let us verify the naturality law for this function:

`f . pure = pure . fmap f`

In this law, `f` is an arbitrary function of type `A ⇒ B`. Let us therefore create a STLC variable of this type:

```tut
def f[A,B] = freshVar[A ⇒ B]
```

We will now compute both sides of the naturality equation, reassigning type variables automatically:

```tut
val leftSide = f @@: pure
val rightSide = pure :@@ (fmapT :@ f)
assert(leftSide equiv rightSide)
```



## How to construct other lambda-terms?

Symbolic verification of laws may require constructing arbitrary lambda-terms.
For instance, it may be necessary to manipulate lambda-terms corresponding to tuples, case classes, and sealed traits, and perform
operations such as matching on a case class within a sealed trait or accessing a field in a case class.

The extension of STLC supported by `curryhoward` supports sealed traits and case classes via "named conjunctions" and "named disjunctions".

A case class is represented as a **named conjunction**, that is, a conjunction that has a name for each part, and also a name for itself.
For example, consider the following case class:

```tut
final case class User(fullName: String, id: Long) 
```

This type is a conjunction of types `String` and `Long`, but it is a named conjunction: it has a name `User` for itself, and each part of the conjunction also has a name (in this example, these names are `fullName` and `id`).

A sealed trait with one or more case classes is represented as a **named disjunction**, that is, a disjunction that has a name for each part, and also a name for itself.
For example, the type `Either[Int, String]` is represented as a disjunction of types `Int` and `String`. The disjunction is named because it has the name `Either` for itself and also the names `Left` and `Right` for the two parts of the disjunction.

Sealed traits sometimes contain case objects (or case classes with no arguments).
These types are analogous to the `Unit` type, except that they have a name.
We will refer to these types as "named `Unit`" types, and regard them as named conjunctions having _zero_ parts.

For manipulating conjunction and disjunction types, the current API of the `curryhoward` library provides the following facilities:

- create a named conjunction from given parts
- decompose a given named conjunction into parts
- create a named disjunction from a given part
- match on a given named disjunction, mapping each part via a given function

## Example of working with lambda-terms

To illustrate these facilities, let us construct a lambda-term representing a function `getId` of type `Option[User] ⇒ Option[Long]` and apply that function to some test data.
We will then implement this function automatically using the `curryhoward` library, and compare the resulting lambda-terms.

We begin by creating a fresh variable of type `Option[User]`.

```tut
val ou = freshVar[Option[User]]
```

The type `Option[User]` is a disjunction type having two parts: `None.type` and `Some[User]`. 

We would like to define the lambda-term `getId` like this, `val getId = u =>: ...`

The body of the function `getId` must examine the value of `u` and match on the two parts of the disjunction.
To create the function body, we need to create a lambda-term that matches the value of `u` on the two parts of the disjunction.

In Scala, we would have written

```scala
(ou: Option[User]) ⇒ ou match {
  case None ⇒ None
  case Some(User(fullName, id)) ⇒ Some(id)
}
```

Now we are going to translate this code into lambda-terms.
Both `case` expressions are represented by functions.
The argument types of these functions are `None.type` and `Some[User]`.
Therefore, the next step for us is to create these functions as lambda-terms.
For that, we will need to create new fresh variables of these types.

```tut
val n = freshVar[None.type]
val su = freshVar[Some[User]]
```

We now need to create the case clauses. We might imagine to write them like this:
```scala
val case1 = n =>: ???
val case2 = su =>: ???
```

The first case clause is simple: it takes a value `None` of type `Option[User]` and returns a value `None` of type `Option[Long]`.
However, we must take care to assign correct types to all terms.
So we cannot just write `val case1 = n =>: n` because that function would have the return type `None.type` rather than `Option[Long]`.
We need to inject `None.type` into the disjunction `Option[Long]`.

This is done in three steps:

- create a fresh variable `n` of type `Option[Long]`
- create a value of type `None.type` using `n.t()` -- note that `None.type` is essentially a "named `Unit`", and we can always create values of a `Unit` type
- using the variable `ol`'s type expression, lift the value of type `None.type` into the disjunction type `Option[Long]` using `ol.t()`

```tut
val ol = freshVar[Option[Long]]
val case1 = n =>: ol.t(n.t())
```

To implement the second case clause, we need to decompose `s` of type `Some[User]`.

In STLC, the type `Some[User]` is a named conjunction consisting of a single part, of type `User`, which is again a named conjunction consisting of two parts.
To access the parts of conjunctions, we can use the `apply` method with a zero-based index or an accessor name.
Let us use the index `0` to access the value of `Some`, and the name `"id"` to access the part of the `User` value.
Thus, the user's `id` is accessed as `s(0)("id")`.

It remains to construct the value of type `Option[Long]` out of the user's `id`.
This is done using the following steps:

- create a fresh variable of type `Some[Long]`
- using that variable's type expression, create a named conjunction of type `Some[Long]` containing `id` as its only part
- inject that value into the disjunction type `Option[Long]`

```tut
val sl = freshVar[Some[Long]]
val case2 = su =>: ol.t(sl.t(su(0)("id")))
```

Now we are ready to write the match statement, which is done by using the `.cases` function on the disjunction value `u`:

```tut
val getId = ou =>: ou.cases(case1, case2)
getId.prettyPrint
```

Let us now apply this function term to some data and verify that it works as expected.
In STLC there are no concrete values of type `String` or `Long`; so we need to use fresh variables instead.

When constructing conjunction and disjunction terms, we may use a shortcut -- call `.apply` on the fresh variables themselves, rather than on their type expressions.
So, `dUser(dString, dLong)` is the same as `dUser.t(dString, dLong)` and constructs a new named conjunction term of type `User`. 

```tut
val dString = freshVar[String]
var dLong = freshVar[Long]
val u = freshVar[User]
val data = ou(su(u(dString, dLong)))
val result1 = getId(data).simplify
```

We have obtained the resulting term, and we can see that it is what we expected -- it represents `Some(dLong)` as the right part of the disjunction type `Option[Long]`.

We will now check that the same lambda-term is obtained when implementing the function `getId` automatically using `curryhoward`.

```tut
val getIdAuto = ofType[Option[User] ⇒ Option[Long]]
val getIdAutoTerm = getIdAuto.lambdaTerm
getIdAutoTerm.prettyPrint
getId.prettyPrint
``` 

The `prettyRename` method will rename all variables in a given term to names `a`, `b`, `c`, and so on.
Note that `prettyRenamePrint` performs a `prettyRename` before printing the term, and that `ofType` will always perform `prettyRename` as well, before returning a term.

Therefore, we need to run `prettyRename` on our term `getId` so that it becomes syntactically equal to `getIdAutoTerm`.
The method `equiv` will do this automatically:

```tut
getIdAutoTerm equiv getId.prettyRename
```

## Summary of the lambda-term API

| Function  | Type  | Comment  |
|---|---|---|
| `TermExpr.lambdaTerm`  | `Any ⇒ Option[TermExpr]` | extract a lambda-term if present  |
| `a.lambdaTerm`  | `Any ⇒ TermExpr`  | extract a lambda-term, throw exception if not present  |
| `t.prettyPrint` | `TermExpr ⇒ String` and `TypeExpr ⇒ String` | produce a more readable string representation than `.toString` |
| `t.prettyRename` | `TermExpr ⇒ TermExpr` | rename variables in a term to `a`, `b`, `c`, etc., so that the term becomes more readable |
| `t.prettyRenamePrint` | `TermExpr ⇒ String` | shorthand for `.prettyRename.prettyPrint` |
| `a.t` | `TermExpr ⇒ TypeExpr` | get the type expression for a given term |
| `freshVar[T]` | `VarE` | create a STLC variable with assigned type expression `T` -- here `T` can be a type parameter or a type expression such as `Int ⇒ Option[A]`  |
| `a =>: b` | `VarE ⇒ TermExpr ⇒ TermExpr` | create a STLC function term (lambda-calculus "abstraction") |
| `a(b)` | `TermExpr ⇒ TermExpr ⇒ TermExpr` | create a STLC "application" term -- the type of `a` must be a function and the type of `b` must be the same as the argument type of `a` |
| `a :@ b` | `TermExpr ⇒ TermExpr ⇒ TermExpr` | create a STLC "application" term with automatic substitution of type variables in `a` -- the type of `a` must be a function and the type of `b` must be the same as the argument type of `a` after some type variables in `a` have been substituted |
| `a andThen b` | `TermExpr ⇒ TermExpr ⇒ TermExpr` | compose functions `a` and `b` -- the argument type of `b` must be the same as the result type of `a`; no substitution of type variables is performed |
| `a :@@ b` | `TermExpr ⇒ TermExpr ⇒ TermExpr` | compose functions `a` and `b` -- the argument type of `b` must be the same as the result type of `a` after an automatic substitution of type variables in `a` |
| `a @@: b` | `TermExpr ⇒ TermExpr ⇒ TermExpr` | compose functions `a` and `b` -- the argument type of `b` must be the same as the result type of `a` after an automatic substitution of type variables in `b` |
| `a.simplify` | `TermExpr ⇒ TermExpr` | perform symbolic simplification of STLC term |
| `a equiv b` | `TermExpr ⇒ TermExpr ⇒ Boolean` | check whether two terms are syntactically equal after simplification and `prettyRename` |
| `a.substTypeVar(b, c)` | `TermExpr ⇒ (TermExpr, TermExpr) ⇒ TermExpr` | replace a type variable in `a`; the type variable is specified as the type of `b`, and the replacement type is specified as the type of `c` |
| `a.substTypeVars(s)` | `TermExpr ⇒ Map[TP, TypeExpr] ⇒ TermExpr` | replace all type variables in `a` according to the given substitution map `s` -- all type variables are substituted at once |
| `u()`  | `TermExpr ⇒ () ⇒ TermExpr` and `TypeExpr ⇒ () ⇒ TermExpr` | create a named unit term of type `u.t` -- the type of `u` must be a named unit type, e.g. `None.type` |
| `c(x...)`  | `TermExpr ⇒ TermExpr* ⇒ TermExpr` and `TypeExpr ⇒ TermExpr* ⇒ TermExpr` | create a named conjunction term of type `c.t` -- the type of `c` must be a conjunction whose parts match the types of the arguments `x...` |
| `d(x)`  |  `TermExpr ⇒ TermExpr ⇒ TermExpr` and `TypeExpr ⇒ TermExpr ⇒ TermExpr` | create a disjunction term of type `d.t` using term `x` -- the type of `x` must match one of the disjunction parts in the type `d`, which must be a disjunction type |
| `c(i)` | `TermExpr ⇒ Int ⇒ TermExpr` | project a conjunction term onto part with given zero-based index -- the type of `c` must be a conjunction with sufficiently many parts |
| `c("id")` | `TermExpr ⇒ String ⇒ TermExpr` | project a conjunction term onto part with given accessor name -- the type of `c` must be a named conjunction that supports this accessor |
| `d.cases(x =>: ..., y =>: ..., ...)` | `TermExpr ⇒ TermExpr* ⇒ TermExpr` | create a term that pattern-matches on the given disjunction term -- the type of `d` must be a disjunction whose arguments match the arguments `x`, `y`, ... of the given case clauses |
