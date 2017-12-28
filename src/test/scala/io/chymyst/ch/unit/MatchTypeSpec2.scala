package io.chymyst.ch.unit

import io.chymyst.ch._
import io.chymyst.ch.Macros.testType
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

    def r[A, B] = Macros.testReifyType[Data[A, B]]

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
    def r[U, V] = Macros.testReifyType[Option[U] ⇒ Option[Option[V]]]

    r.prettyPrint shouldEqual "Option[U]{None.type + Some[U]} ⇒ Option[Option[V]{None.type + Some[V]}]{None.type + Some[Option[V]{None.type + Some[V]}]}"

    val A = "A"
    val B = "B"
    val value = "value"
    val x = "x"

    case class OOption[A](x: Option[Option[A]])

    def r2[B] = Macros.testReifyType[OOption[B]]

    r2 shouldEqual NamedConjunctT("OOption", List(TP(B)), List(x), List(DisjunctT("Option", List(DisjunctT("Option", List(TP(B)), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(TP(B)), List(value), List(TP(B)))))), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(DisjunctT("Option", List(TP(B)), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(TP(B)), List(value), List(TP(B)))))), List(value), List(DisjunctT("Option", List(TP(B)), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(TP(B)), List(value), List(TP(B)))))))))))
  }

  behavior of "other syntax"

  it should "get type of conventional syntax for function" in {
    def result[A, B, C](x: A, y: B)(z: (C, C))(implicit t: Int): (String, String) = testType[A ⇒ C]

    val r = result(0, 0)((0, 0))(0)
    r._1 shouldEqual "A ⇒ C"
    r._2 shouldEqual "<tc>[A, B, C](x: A, y: B)(z: (C, C))(implicit t: Int)(String, String)"
  }
}
