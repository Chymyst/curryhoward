package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MoreMatchTypeSpec extends AnyFlatSpec with Matchers {

  behavior of "tuples"

  it should "get type with argument tuples" in {
    def result[A, B] = typeExpr[A ⇒ ((A, B)) ⇒ A]

    result.prettyPrint shouldEqual "A ⇒ Tuple2[A,B] ⇒ A"
  }

  it should "get type with nested argument tuples" in {
    def result[A, B] = typeExpr[(((A, B)) ⇒ A) ⇒ A]

    result.prettyPrint
    "(Tuple2[A,B] ⇒ A) ⇒ A"
  }

  it should "get a complicated type with argument tuples" in {
    def result[S, A, B] = typeExpr[(S ⇒ (A, S)) ⇒ (((A, S)) ⇒ (B, S)) ⇒ (S ⇒ (B, S))]

    result.prettyPrint
    "(S ⇒ Tuple2[A,S]) ⇒ (Tuple2[A,S] ⇒ Tuple2[B,S]) ⇒ S ⇒ Tuple2[B,S]"
  }

  behavior of "Java-style function argument groups"

  it should "get type with argument group" in {
    def result[A, B] = typeExpr[A ⇒ (A, B) ⇒ A]

    result.prettyPrint shouldEqual "A ⇒ (A, B) ⇒ A"
  }

  it should "get type with argument group in higher-order function" in {
    def result[A, B, C] = typeExpr[A ⇒ ((A, B, C) ⇒ A) ⇒ B]

    result.prettyPrint shouldEqual "A ⇒ ((A, B, C) ⇒ A) ⇒ B"
  }

  it should "use ConjunctT to emit code for Java-style argument group" in {
    freshVar[(Int, (Int, Boolean), (Int, String) ⇒ Double) ⇒ Boolean].t shouldEqual #->(ConjunctT(List(BasicT("Int"), NamedConjunctT("Tuple2", List(BasicT("Int"), BasicT("Boolean")), List("_1", "_2"), List(BasicT("Int"), BasicT("Boolean"))), #->(ConjunctT(List(BasicT("Int"), BasicT("String"))), BasicT("Double")))), BasicT("Boolean"))
  }

  behavior of "type parameters"

  it should "substitute type parameters into nested case classes" in {
    case class User[T](t: T)
    case class Data[U, V](u: User[U], v: V)

    def r[A, B] = freshVar[Data[A, B]].t

    val A = TP("A")
    val B = TP("B")
    val T = TP("T")
    val V = TP("V")
    val u = "u"
    val v = "v"
    val t = "t"

    r shouldEqual NamedConjunctT("Data", List(A, B), List(u, v), List(NamedConjunctT("User", List(A), List(t), List(A)), B))
  }

  it should "match type with nested type parameters" in {
    def r[U, V] = freshVar[Option[U] ⇒ Option[Option[V]]].t

    r.prettyPrintVerbose shouldEqual "Option[U]{None.type + Some[U]} ⇒ Option[Option[V]{None.type + Some[V]}]{None.type + Some[Option[V]{None.type + Some[V]}]}"

    val A = "A"
    val B = "B"
    val value = "value"
    val x = "x"

    case class OOption[A](x: Option[Option[A]])

    def r2[B] = freshVar[OOption[B]].t

    r2 shouldEqual NamedConjunctT("OOption", List(TP(B)), List(x), List(DisjunctT("Option", List(DisjunctT("Option", List(TP(B)), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(TP(B)), List(value), List(TP(B)))))), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(DisjunctT("Option", List(TP(B)), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(TP(B)), List(value), List(TP(B)))))), List(value), List(DisjunctT("Option", List(TP(B)), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(TP(B)), List(value), List(TP(B)))))))))))
  }

  behavior of "other syntax"

  it should "get type of conventional syntax for function" in {
    def result[A, B, C] = typeExpr[A ⇒ C]

    result.prettyPrint shouldEqual "A ⇒ C"
  }

  behavior of "recursive types"

  it should "not hang on List type" in {
    def result[A](x: List[A]) = typeExpr[A ⇒ List[A]]

    val r = result(List(10))
    r.prettyPrint shouldEqual "A ⇒ List[A]"

    def f[A]: A ⇒ List[A] = implement

    f(10) shouldEqual List()
    f.lambdaTerm.prettyPrint shouldEqual "a ⇒ (0 + Nil())"
  }

  it should "process List[A] ⇒ List[A]" in {
    freshVar[List[Int] ⇒ List[Int]].t shouldEqual #->(DisjunctT("List", List(BasicT("Int")), List(NamedConjunctT("::", List(BasicT("Int")), List("head", "tl$access$1"), List(BasicT("Int"), RecurseT("List", List(BasicT("Int"))))), NamedConjunctT("Nil", List(), List(), List()))), DisjunctT("List", List(BasicT("Int")), List(NamedConjunctT("::", List(BasicT("Int")), List("head", "tl$access$1"), List(BasicT("Int"), RecurseT("List", List(BasicT("Int"))))), NamedConjunctT("Nil", List(), List(), List()))))


    def f[P] = freshVar[List[P] ⇒ List[P]].t

    f[Int] shouldEqual #->(DisjunctT("List", List(TP("P")), List(NamedConjunctT("::", List(TP("P")), List("head", "tl$access$1"), List(TP("P"), RecurseT("List", List(TP("P"))))), NamedConjunctT("Nil", List(), List(), List()))), DisjunctT("List", List(TP("P")), List(NamedConjunctT("::", List(TP("P")), List("head", "tl$access$1"), List(TP("P"), RecurseT("List", List(TP("P"))))), NamedConjunctT("Nil", List(), List(), List()))))

  }

  it should "process case classes containing List" in {

    final case class Data[Q](f: List[(Q, Q, Int)], g: Int)

    def result[A] = typeExpr[A ⇒ Data[A]]

    result.prettyPrint shouldEqual "A ⇒ Data[A]"
  }

  it should "process Either containing List" in {
    def result[P] = typeExpr[P ⇒ Either[List[P], Option[P]]]

    result.prettyPrint shouldEqual "P ⇒ Either[List[P],Option[P]]"
  }

  it should "process a recursive case class (infinite product)" in {
    final case class InfiniteProduct(x: Int, p: InfiniteProduct)
    val r = freshVar[InfiniteProduct].t
    r shouldEqual NamedConjunctT("InfiniteProduct", List(), List("x", "p"), List(BasicT("Int"), RecurseT("InfiniteProduct", Nil)))
    r.prettyPrint shouldEqual "InfiniteProduct"
  }

  it should "process a recursive case class (infinite product with tuple)" in {
    final case class InfiniteProduct(x: Int, p: (Double, InfiniteProduct))
    val r = freshVar[InfiniteProduct].t
    r shouldEqual NamedConjunctT("InfiniteProduct", List(), List("x", "p"), List(BasicT("Int"), NamedConjunctT("Tuple2", List(BasicT("Double"), RecurseT("InfiniteProduct", Nil)), List("_1", "_2"), List(BasicT("Double"), RecurseT("InfiniteProduct", Nil)))))
    r.prettyPrint shouldEqual "InfiniteProduct"
  }

  it should "process a recursive case class (infinite sum)" in {
    sealed trait InfiniteSum[T]
    final case class Element[U](t: U) extends InfiniteSum[U]
    final case class RecursiveSum[V](s: InfiniteSum[V]) extends InfiniteSum[V]

    val r = freshVar[InfiniteSum[Int]].t
    r shouldEqual DisjunctT("InfiniteSum", List(BasicT("Int")), List(NamedConjunctT("Element", List(BasicT("Int")), List("t"), List(BasicT("Int"))), NamedConjunctT("RecursiveSum", List(BasicT("Int")), List("s"), List(RecurseT("InfiniteSum", List(BasicT("Int")))))))

    r.prettyPrint shouldEqual "InfiniteSum[<c>Int]"
    r.prettyPrintVerbose shouldEqual "InfiniteSum[<c>Int]{Element[<c>Int] + RecursiveSum[<c>Int]}"

    RecurseT("Abc", List(TP("A"))).prettyPrint shouldEqual "<rec>Abc[A]"
    RecurseT("Abc", Nil).caseObjectName shouldEqual None
  }

  it should "process a recursive type (infinite implication)" in {
    final case class InfImplication[T](i: T ⇒ InfImplication[T])

    val r = freshVar[InfImplication[Int]].t

    r shouldEqual NamedConjunctT("InfImplication", List(BasicT("Int")), List("i"), List(#->(BasicT("Int"), RecurseT("InfImplication", List(BasicT("Int"))))))

    r.prettyPrint shouldEqual "InfImplication[<c>Int]"
  }

  it should "process mutually recursive disjunctions" in {

    sealed trait A[T]
    sealed trait B[U]

    final case class A1[R1](b1: B[R1]) extends A[R1]
    final case class A2[R2](a2: A[R2]) extends A[R2]

    final case class B1[S1](a1: A[S1]) extends B[S1]
    final case class B2[S2](b2: B[S2]) extends B[S2]

    def result[Z] = freshVar[Z ⇒ Either[A[Z], Option[B[Z]]]].t

    val r = result[Int]
    r.prettyPrintVerbose shouldEqual "Z ⇒ Either[A[Z]{A1[Z] + A2[Z]},Option[B[Z]{B1[Z] + B2[Z]}]{None.type + Some[B[Z]{B1[Z] + B2[Z]}]}]{Left[A[Z]{A1[Z] + A2[Z]},Option[B[Z]{B1[Z] + B2[Z]}]{None.type + Some[B[Z]{B1[Z] + B2[Z]}]}] + Right[A[Z]{A1[Z] + A2[Z]},Option[B[Z]{B1[Z] + B2[Z]}]{None.type + Some[B[Z]{B1[Z] + B2[Z]}]}]}"

    val typeAInt = freshVar[A[Int]].t
    typeAInt shouldEqual DisjunctT("A", List(BasicT("Int")), List(NamedConjunctT("A1", List(BasicT("Int")), List("b1"), List(DisjunctT("B", List(BasicT("Int")), List(NamedConjunctT("B1", List(BasicT("Int")), List("a1"), List(RecurseT("A", List(BasicT("Int"))))), NamedConjunctT("B2", List(BasicT("Int")), List("b2"), List(RecurseT("B", List(BasicT("Int"))))))))), NamedConjunctT("A2", List(BasicT("Int")), List("a2"), List(RecurseT("A", List(BasicT("Int")))))))
    typeAInt.prettyPrintVerbose shouldEqual "A[<c>Int]{A1[<c>Int] + A2[<c>Int]}"
  }

  it should "generate code containing case objects" in {
    sealed trait A[X]
    final case class A1[X]() extends A[X]
    case object A0 extends A[Nothing]
    final case class A2[X](q: X) extends A[X]

    def f[X]: A[X] ⇒ A[X] = implement

    def g[X, Y]: (X ⇒ Y) ⇒ A[X] ⇒ A[Y] = implement
  }
}
