package io.chymyst.ch.unit

import io.chymyst.ch.Macros.testType
import org.scalatest.{FlatSpec, Matchers}

class MatchTypeSpec extends FlatSpec with Matchers {
  behavior of "type parameter introspection"

  it should "get printable representation of fixed types with _" in {
    def result[A, B, C]: (String, String) = testType[_ ⇒ B]

    val res = result._1
    res shouldEqual "<c>_ ⇒ B"
  }

  it should "get printable representation of enclosing owner's type" in {
    def result[A, B, C]: (String, String) = testType[Int]

    result._2 shouldEqual "Tuple2[<c>String,<c>String]"
  }

  it should "get printable representation of enclosing owner's type with function syntax" in {
    def result[A, B, C](x: A, y: B)(z: C): (String, String) = testType
    // TODO: implement detection of types on the LHS
    result[Int, Boolean, Double](123, true)(1.0)._2 shouldEqual "<tc>[A, B, C](x: A, y: B)(z: C)(String, String)"
  }

  it should "get printable representation of basic types" in {
    def result[A, B, C]: (String, String) = testType[Int]

    result._1 shouldEqual "<c>Int"
  }

  it should "get printable representation of parametric type" in {
    def result[A, B, C]: (String, String) = testType[A]

    result._1 shouldEqual "A"
  }

  it should "get printable representation of function types" in {
    def result[A, B, C]: (String, String) = testType[A ⇒ B]

    result._1 shouldEqual "A ⇒ B"
  }

  it should "get printable representation of fixed types with type constructors" in {
    def result[P, Q, R]: (String, String) = testType[Option[Seq[Int]] ⇒ Option[IndexedSeq[Set[P]]] ⇒ Q]

    result._1 shouldEqual "Option[<tc>Seq[Int]]{None.type + Some[<tc>Seq[Int]]} ⇒ Option[<tc>IndexedSeq[Set[P]]]{None.type + Some[<tc>IndexedSeq[Set[P]]]} ⇒ Q"
  }

  it should "get printable representation of fixed types with type constructors with [_]" in {
    def result[P, Q, R]: (String, String) = testType[Option[_] ⇒ Q]

    val res = result._1
    res shouldEqual "Option[<c>_]{None.type + Some[<c>_]} ⇒ Q"
  }

  it should "get printable representation of Option types" in {
    def result[P, Q, R]: (String, String) = testType[Option[P] ⇒ Either[P, Q]]

    result._1 shouldEqual "Option[P]{None.type + Some[P]} ⇒ Either[P,Q]{Left[P,Q] + Right[P,Q]}"
  }

  it should "get printable representation of Any, Unit, and Nothing types" in {
    def result[A, B, C]: (String, String) = testType[Any ⇒ Nothing ⇒ Unit]

    result._1 shouldEqual "<c>_ ⇒ 0 ⇒ Unit"
  }

  it should "not confuse a type parameter with a type inheriting from Any" in {
    class Q

    def result[A, B, C]: (String, String) = testType[A ⇒ Q]

    result._1 shouldEqual "A ⇒ <c>Q"
  }

  it should "get printable representation of tuple types" in {
    def result[A, B, C]: (String, String) = testType[(Any, Nothing, Unit, A, B, C)]

    result._1 shouldEqual "Tuple6[<c>_,0,Unit,A,B,C]"
  }

  it should "get printable representation of tuple as function argument" in {
    def result[A, B, C]: (String, String) = testType[((A, B)) ⇒ C]

    result._1 shouldEqual "Tuple2[A,B] ⇒ C"
  }

  val basicTypes = List("Int", "String", "Boolean", "Float", "Double", "Long", "Symbol", "Char")

  it should "get printable representation of tuple of basic types" in {
    def result[A, B, C]: (String, String) = testType[(Int, String, Boolean, Float, Double, Long, Symbol, Char)]

    result._1 shouldEqual "Tuple" + basicTypes.length + "[" + basicTypes.map("<c>" + _).mkString(",") + "]"
  }

  it should "get printable representation of single case class" in {

    def result[T, U]: (String, String) = testType[Wrap1[T, U]]

    result._1 shouldEqual "Wrap1[T,U]"
  }

  it should "get printable representation of sealed trait with type parameters" in {
    def result[T, U]: (String, String) = testType[SimpleChoice[T]]

    result._1 shouldEqual "SimpleChoice[T]{SimpleChoice1[T] + SimpleChoice2[T]}"
  }

  it should "get printable representation of sealed trait without type parameters" in {
    def result[T, U]: (String, String) = testType[Wrap2]

    result._1 shouldEqual "Wrap2{Wrap2a + Wrap2b + Wrap2c.type + Wrap2d[A] + Wrap2e[A]}"
  }

  it should "get printable representation of case class" in {
    sealed trait Test1[T]
    case class A[T](x: Int, y: T) extends Test1[T]
    case class B(y: String) extends Test1[String]

    sealed abstract class Test2
    case class C(x: Int)(y: Double) extends Test2
    case class D(y: String) extends Test2

    def result[T]: (String, String) = testType[Test1[T] ⇒ Test2]

    result._1 shouldEqual "Test1[T]{A[T] + B} ⇒ Test2{C + D}"
  }
}
