package io.chymyst.ch.unit

import io.chymyst.ch._
import io.chymyst.ch.Macros._
import org.scalatest.{FlatSpec, Matchers}

class MatchTypeSpec2 extends FlatSpec with Matchers {

  behavior of "tuples"

  it should "get type with argument tuples" in {
    def result[A, B]: (String, String) = testType[A ⇒ ((A, B)) ⇒ A]

    val r = result

    r._2 shouldEqual "(<c>String, <c>String)"
    r._1 shouldEqual "A ⇒ (A, B) ⇒ A"
  }

  it should "get type with nested argument tuples" in {
    def result[A, B]: (String, String) = testType[(((A, B)) ⇒ A) ⇒ A]

    val r = result

    r._2 shouldEqual "(<c>String, <c>String)"
    r._1 shouldEqual "((A, B) ⇒ A) ⇒ A"
  }

  it should "get a complicated type with argument tuples" in {
    def result[S, A, B]: (String, String) = testType[(S ⇒ (A, S)) ⇒ (((A, S)) ⇒ (B, S)) ⇒ (S ⇒ (B, S))]

    val r = result

    r._2 shouldEqual "(<c>String, <c>String)"
    r._1 shouldEqual "(S ⇒ (A, S)) ⇒ ((A, S) ⇒ (B, S)) ⇒ S ⇒ (B, S)"
  }

  behavior of "type parameters"

  it should "substitute type parameters into nested case classes" in {
    case class User[T](t: T)
    case class Data[U, V](u: User[U], v: V)

    def r[A, B] = testReifyType[Data[A, B]]

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
    def r[U, V] = testReifyType[Option[U] ⇒ Option[Option[V]]]

    r.prettyPrint shouldEqual "Option[U]{None.type + Some[U]} ⇒ Option[Option[V]{None.type + Some[V]}]{None.type + Some[Option[V]{None.type + Some[V]}]}"

    val A = "A"
    val B = "B"
    val value = "value"
    val x = "x"

    case class OOption[A](x: Option[Option[A]])

    def r2[B] = testReifyType[OOption[B]]

    r2 shouldEqual NamedConjunctT("OOption", List(TP(B)), List(x), List(DisjunctT("Option", List(DisjunctT("Option", List(TP(B)), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(TP(B)), List(value), List(TP(B)))))), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(DisjunctT("Option", List(TP(B)), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(TP(B)), List(value), List(TP(B)))))), List(value), List(DisjunctT("Option", List(TP(B)), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(TP(B)), List(value), List(TP(B)))))))))))
  }

  behavior of "other syntax"

  it should "get type of conventional syntax for function" in {
    def result[A, B, C](x: A, y: B)(z: (C, C))(implicit t: Int): (String, String) = testType[A ⇒ C]

    val r = result(0, 0)((0, 0))(0)
    r._1 shouldEqual "A ⇒ C"
    r._2 shouldEqual "<tc>[A, B, C](x: A, y: B)(z: (C, C))(implicit t: Int)(String, String)"
  }

  behavior of "recursive types"

  it should "not hang on List type" in {
    def result[A](x: List[A]): (String, String) = testType[A ⇒ List[A]]

    val r = result(List(0))
    r._1 shouldEqual "A ⇒ List[A]{::[B] + Nil.type}"
    r._2 shouldEqual "<tc>[A](x: List[A])(String, String)"
  }

  it should "process List[A] ⇒ List[A]" in {
    testReifyType[List[Int] ⇒ List[Int]] shouldEqual #->(DisjunctT("List", List(BasicT("Int")), List(NamedConjunctT("::", List(TP("B")), List("head", "tl$access$1"), List(TP("B"), RecurseT("List", List(TP("B"))))), NamedConjunctT("Nil", List(), List(), List()))), DisjunctT("List", List(BasicT("Int")), List(NamedConjunctT("::", List(TP("B")), List("head", "tl$access$1"), List(TP("B"), RecurseT("List", List(TP("B"))))), NamedConjunctT("Nil", List(), List(), List()))))


    def f[P] = testReifyType[List[P] ⇒ List[P]]

    f[Int] shouldEqual #->(DisjunctT("List", List(TP("P")), List(NamedConjunctT("::", List(TP("B")), List("head", "tl$access$1"), List(TP("B"), RecurseT("List", List(TP("B"))))), NamedConjunctT("Nil", List(), List(), List()))), DisjunctT("List", List(TP("P")), List(NamedConjunctT("::", List(TP("B")), List("head", "tl$access$1"), List(TP("B"), RecurseT("List", List(TP("B"))))), NamedConjunctT("Nil", List(), List(), List()))))

  }

  it should "process case classes containing List" in {

    final case class Data[Q](f: List[(Q, Q, Int)], g: Int)

    def result[A](x: Data[A]): (String, String) = testType[A ⇒ Data[A]]

    val r = result(Data(List((0, 0, 1)), 123))
    r._1 shouldEqual "A ⇒ Data[A]"
    r._2 shouldEqual "<tc>[A](x: Data[A])(String, String)"
  }

  it should "process Either containing List" in {
    def result[P](): (String, String) = testType[P ⇒ Either[List[P], Option[P]]]

    val r = result()
    // TODO: fix type parameters - should be P and not B
    r._1 shouldEqual "P ⇒ Either[List[P]{::[B] + Nil.type},Option[P]{None.type + Some[P]}]{Left[List[P]{::[B] + Nil.type},Option[P]{None.type + Some[P]}] + Right[List[P]{::[B] + Nil.type},Option[P]{None.type + Some[P]}]}"
  }

  it should "process a recursive case class (infinite product)" in {
    final case class InfiniteProduct(x: Int, p: InfiniteProduct)
    val r = testReifyType[InfiniteProduct]
    r shouldEqual NamedConjunctT("InfiniteProduct", List(), List("x", "p"), List(BasicT("Int"), RecurseT("InfiniteProduct", Nil)))
    r.prettyPrint shouldEqual "InfiniteProduct"
  }

  it should "process a recursive case class (infinite product with tuple)" in {
    final case class InfiniteProduct(x: Int, p: (Double, InfiniteProduct))
    val r = testReifyType[InfiniteProduct]
    r shouldEqual NamedConjunctT("InfiniteProduct", List(), List("x", "p"), List(BasicT("Int"), ConjunctT(List(BasicT("Double"), RecurseT("InfiniteProduct", Nil)))))
    r.prettyPrint shouldEqual "InfiniteProduct"
  }

  it should "process a recursive case class (infinite sum)" in {
    sealed trait InfiniteSum[T]
    final case class Element[U](t: U) extends InfiniteSum[U]
    final case class RecursiveSum[V](s: InfiniteSum[V]) extends InfiniteSum[V]

    val r = testReifyType[InfiniteSum[Int]]
    r shouldEqual DisjunctT("InfiniteSum", List(BasicT("Int")), List(NamedConjunctT("Element", List(TP("U")), List("t"), List(TP("U"))), NamedConjunctT("RecursiveSum", List(TP("V")), List("s"), List(RecurseT("InfiniteSum", List(TP("V")))))))

    r.prettyPrint shouldEqual "InfiniteSum[<c>Int]{Element[U] + RecursiveSum[V]}"

    RecurseT("Abc", List(TP("A"))).prettyPrint shouldEqual "<rec>Abc[A]"
    RecurseT("Abc", Nil).caseObjectName shouldEqual None
  }

  it should "process a recursive type (infinite implication)" in {
    final case class InfImplication[T](i: T ⇒ InfImplication[T])

    val r = testReifyType[InfImplication[Int]]

    r shouldEqual NamedConjunctT("InfImplication", List(BasicT("Int")), List("i"), List(#->(BasicT("Int"),RecurseT("InfImplication", List(TP("T"))))))

    r.prettyPrint shouldEqual "InfImplication[<c>Int]"
  }

  it should "process mutually recursive disjunctions" in {

    sealed trait A[T]
    sealed trait B[U]

    final case class A1[R1](b1: B[R1]) extends A[R1]
    final case class A2[R2](a2: A[R2]) extends A[R2]

    final case class B1[S1](a1: A[S1]) extends B[S1]
    final case class B2[S2](b2: B[S2]) extends B[S2]

    def result[Z] = testReifyType[Z ⇒ Either[A[Z], Option[B[Z]]]]

    val r = result[Int]
    r.prettyPrint shouldEqual "Z ⇒ Either[A[Z]{A1[R1] + A2[R2]},Option[B[Z]{B1[S1] + B2[S2]}]{None.type + Some[B[Z]{B1[S1] + B2[S2]}]}]{Left[A[Z]{A1[R1] + A2[R2]},Option[B[Z]{B1[S1] + B2[S2]}]{None.type + Some[B[Z]{B1[S1] + B2[S2]}]}] + Right[A[Z]{A1[R1] + A2[R2]},Option[B[Z]{B1[S1] + B2[S2]}]{None.type + Some[B[Z]{B1[S1] + B2[S2]}]}]}"

    // TODO: fix type parameter names - we should not have any S1 or R1 in these type expressions!
    val typeAInt = testReifyType[A[Int]]
    typeAInt shouldEqual DisjunctT("A", List(BasicT("Int")), List(NamedConjunctT("A1", List(TP("R1")), List("b1"), List(DisjunctT("B", List(TP("R1")), List(NamedConjunctT("B1", List(TP("S1")), List("a1"), List(RecurseT("A", List(TP("S1"))))), NamedConjunctT("B2", List(TP("S2")), List("b2"), List(RecurseT("B", List(TP("S2"))))))))), NamedConjunctT("A2", List(TP("R2")), List("a2"), List(RecurseT("A", List(TP("R2")))))))
    typeAInt.prettyPrint shouldEqual "A[<c>Int]{A1[R1] + A2[R2]}"
  }

}
