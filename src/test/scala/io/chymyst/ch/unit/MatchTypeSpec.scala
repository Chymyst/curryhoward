package io.chymyst.ch.unit

import io.chymyst.ch.Macros.typeExpr
import org.scalatest.{FlatSpec, Matchers}

class MatchTypeSpec extends FlatSpec with Matchers {
  behavior of "type parameter introspection"

  it should "get printable representation of fixed types with _" in {
    def result[A, B, C] = typeExpr[_ ⇒ B]

    result.prettyPrint shouldEqual "<c>_ ⇒ B"
  }


  it should "get printable representation of basic types" in {
    def result[A, B, C] = typeExpr[Int]

    result.prettyPrint shouldEqual "<c>Int"
  }

  it should "get printable representation of parametric type" in {
    def result[A, B, C] = typeExpr[A]

    result.prettyPrint shouldEqual "A"
  }

  it should "get printable representation of function types" in {
    def result[A, B, C] = typeExpr[A ⇒ B]

    result.prettyPrint shouldEqual "A ⇒ B"
  }

  it should "get printable representation of fixed types with type constructors" in {
    def result[P, Q, R] = typeExpr[Option[Seq[Int]] ⇒ Option[IndexedSeq[Set[P]]] ⇒ Q]

    result.prettyPrint shouldEqual "Option[<tc>Seq[<c>Int]] ⇒ Option[<tc>IndexedSeq[<tc>Set[P]]] ⇒ Q"
  }

  it should "get printable representation of fixed types with type constructors with [_]" in {
    def result[P, Q, R] = typeExpr[Option[_] ⇒ Q]

    result.prettyPrint shouldEqual "Option[<c>_] ⇒ Q"
  }

  it should "get printable representation of Option types" in {
    def result[P, Q, R] = typeExpr[Option[P] ⇒ Either[P, Q]]

    result.prettyPrint shouldEqual "Option[P] ⇒ Either[P,Q]"
  }

  it should "get printable representation of Any, Unit, and Nothing types" in {
    def result[A, B, C] = typeExpr[Any ⇒ Nothing ⇒ Unit]

    result.prettyPrint shouldEqual "<c>_ ⇒ 0 ⇒ Unit"
  }

  it should "not confuse a type parameter with a type inheriting from Any" in {
    class Q

    def result[A, B, C] = typeExpr[A ⇒ Q]

    result.prettyPrint shouldEqual "A ⇒ <c>Q"
  }

  it should "get printable representation of tuple types" in {
    def result[A, B, C] = typeExpr[(Any, Nothing, Unit, A, B, C)]

    result.prettyPrint shouldEqual "Tuple6[<c>_,0,Unit,A,B,C]"
  }

  it should "get printable representation of tuple as function argument" in {
    def result[A, B, C] = typeExpr[((A, B)) ⇒ C]

    result.prettyPrint shouldEqual "Tuple2[A,B] ⇒ C"
  }

  val basicTypes = List("Int", "String", "Boolean", "Float", "Double", "Long", "Symbol", "Char")

  it should "get printable representation of tuple of basic types" in {
    def result[A, B, C] = typeExpr[(Int, String, Boolean, Float, Double, Long, Symbol, Char)]

    result.prettyPrint shouldEqual "Tuple" + basicTypes.length + "[" + basicTypes.map("<c>" + _).mkString(",") + "]"
  }

  it should "get printable representation of single case class" in {

    def result[T, U] = typeExpr[Wrap1[T, U]]

    result.prettyPrint shouldEqual "Wrap1[T,U]"
  }

  it should "get printable representation of sealed trait with type parameters" in {
    def result[T, U] = typeExpr[SimpleChoice[T]]

    result.prettyPrint shouldEqual "SimpleChoice[T]"
  }

  it should "get printable representation of sealed trait without type parameters" in {
    def result[T, U] = typeExpr[Wrap2]

    result.prettyPrint shouldEqual "Wrap2"
    result.prettyPrintVerbose shouldEqual "Wrap2{Wrap2a + Wrap2b + Wrap2c.type + Wrap2d[A] + Wrap2e[A]}"
  }

  it should "get printable representation of case class" in {
    sealed trait Test1[T]
    case class A[T](x: Int, y: T) extends Test1[T]
    case class B(y: String) extends Test1[String]

    sealed abstract class Test2
    case class C(x: Int)(y: Double) extends Test2
    case class D(y: String) extends Test2

    def result[T] = typeExpr[Test1[T] ⇒ Test2]

    result.prettyPrint shouldEqual "Test1[T] ⇒ Test2"
    result.prettyPrintVerbose shouldEqual "Test1[T]{A[T] + B} ⇒ Test2{C + D}"
  }
}
