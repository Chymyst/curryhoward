package io.chymyst.ch.unit

import io.chymyst.ch._
import io.chymyst.ch.Macros._
import org.scalatest.{FlatSpec, Matchers}

class MoreMatchTypeSpec extends FlatSpec with Matchers {

  behavior of "tuples"

  it should "get type with argument tuples" in {
    def result[A, B]: (String, String) = testType[A ⇒ ((A, B)) ⇒ A]

    val r = result

    r._2 shouldEqual "Tuple2[<c>String,<c>String]"
    r._1 shouldEqual "A ⇒ Tuple2[A,B] ⇒ A"
  }

  it should "get type with nested argument tuples" in {
    def result[A, B]: (String, String) = testType[(((A, B)) ⇒ A) ⇒ A]

    val r = result

    r._2 shouldEqual "Tuple2[<c>String,<c>String]"
    r._1 shouldEqual "(Tuple2[A,B] ⇒ A) ⇒ A"
  }

  it should "get a complicated type with argument tuples" in {
    def result[S, A, B]: (String, String) = testType[(S ⇒ (A, S)) ⇒ (((A, S)) ⇒ (B, S)) ⇒ (S ⇒ (B, S))]

    val r = result

    r._2 shouldEqual "Tuple2[<c>String,<c>String]"
    r._1 shouldEqual "(S ⇒ Tuple2[A,S]) ⇒ (Tuple2[A,S] ⇒ Tuple2[B,S]) ⇒ S ⇒ Tuple2[B,S]"
  }

  behavior of "Java-style function argument groups"

  it should "get type with argument group" in {
    def result[A, B]: (String, String) = testType[A ⇒ (A, B) ⇒ A]

    val r = result

    r._1 shouldEqual "A ⇒ (A, B) ⇒ A"
  }

  it should "get type with argument group in higher-order function" in {
    def result[A, B, C]: (String, String) = testType[A ⇒ ((A, B, C) ⇒ A) ⇒ B]

    val r = result

    r._1 shouldEqual "A ⇒ ((A, B, C) ⇒ A) ⇒ B"
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

    r.prettyPrint shouldEqual "Option[U]{None.type + Some[U]} ⇒ Option[Option[V]{None.type + Some[V]}]{None.type + Some[Option[V]{None.type + Some[V]}]}"

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
    def result[A, B, C](x: A, y: B)(z: (C, C))(implicit t: Int): (String, String) = testType[A ⇒ C]

    val r = result(0, 0)((0, 0))(0)
    r._1 shouldEqual "A ⇒ C"
    r._2 shouldEqual "<tc>[A, B, C](x: A, y: B)(z: (C, C))(implicit t: Int)(String, String)"
  }

  behavior of "recursive types"

  it should "not hang on List type" in {
    def result[A](x: List[A]): (String, String) = testType[A ⇒ List[A]]

    val r = result(List(0))
    r._1 shouldEqual "A ⇒ List[A]{::[A] + Nil.type}"
    r._2 shouldEqual "<tc>[A](x: List[A])(String, String)"
  }

  it should "process List[A] ⇒ List[A]" in {
    freshVar[List[Int] ⇒ List[Int]].t shouldEqual #->(DisjunctT("List", List(BasicT("Int")), List(NamedConjunctT("::", List(BasicT("Int")), List("head", "tl$access$1"), List(BasicT("Int"), RecurseT("List", List(BasicT("Int"))))), NamedConjunctT("Nil", List(), List(), List()))), DisjunctT("List", List(BasicT("Int")), List(NamedConjunctT("::", List(BasicT("Int")), List("head", "tl$access$1"), List(BasicT("Int"), RecurseT("List", List(BasicT("Int"))))), NamedConjunctT("Nil", List(), List(), List()))))


    def f[P] = freshVar[List[P] ⇒ List[P]].t

    f[Int] shouldEqual #->(DisjunctT("List", List(TP("P")), List(NamedConjunctT("::", List(TP("P")), List("head", "tl$access$1"), List(TP("P"), RecurseT("List", List(TP("P"))))), NamedConjunctT("Nil", List(), List(), List()))), DisjunctT("List", List(TP("P")), List(NamedConjunctT("::", List(TP("P")), List("head", "tl$access$1"), List(TP("P"), RecurseT("List", List(TP("P"))))), NamedConjunctT("Nil", List(), List(), List()))))

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

    r._1 shouldEqual "P ⇒ Either[List[P]{::[P] + Nil.type},Option[P]{None.type + Some[P]}]{Left[List[P]{::[P] + Nil.type},Option[P]{None.type + Some[P]}] + Right[List[P]{::[P] + Nil.type},Option[P]{None.type + Some[P]}]}"
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

    r.prettyPrint shouldEqual "InfiniteSum[<c>Int]{Element[<c>Int] + RecursiveSum[<c>Int]}"

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
    r.prettyPrint shouldEqual "Z ⇒ Either[A[Z]{A1[Z] + A2[Z]},Option[B[Z]{B1[Z] + B2[Z]}]{None.type + Some[B[Z]{B1[Z] + B2[Z]}]}]{Left[A[Z]{A1[Z] + A2[Z]},Option[B[Z]{B1[Z] + B2[Z]}]{None.type + Some[B[Z]{B1[Z] + B2[Z]}]}] + Right[A[Z]{A1[Z] + A2[Z]},Option[B[Z]{B1[Z] + B2[Z]}]{None.type + Some[B[Z]{B1[Z] + B2[Z]}]}]}"

    val typeAInt = freshVar[A[Int]].t
    typeAInt shouldEqual DisjunctT("A", List(BasicT("Int")), List(NamedConjunctT("A1", List(BasicT("Int")), List("b1"), List(DisjunctT("B", List(BasicT("Int")), List(NamedConjunctT("B1", List(BasicT("Int")), List("a1"), List(RecurseT("A", List(BasicT("Int"))))), NamedConjunctT("B2", List(BasicT("Int")), List("b2"), List(RecurseT("B", List(BasicT("Int"))))))))), NamedConjunctT("A2", List(BasicT("Int")), List("a2"), List(RecurseT("A", List(BasicT("Int")))))))
    typeAInt.prettyPrint shouldEqual "A[<c>Int]{A1[<c>Int] + A2[<c>Int]}"
  }

}
