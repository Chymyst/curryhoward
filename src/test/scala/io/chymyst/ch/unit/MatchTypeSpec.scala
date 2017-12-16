package io.chymyst.ch.unit

import io.chymyst.ch.CurryHowardMacros
import io.chymyst.ch.CurryHowardMacros.testType
import org.scalatest.{FlatSpec, Matchers}

class MatchTypeSpec extends FlatSpec with Matchers {
  behavior of "type parameter introspection"

  it should "get printable representation of fixed types with _" in {
    def result[A, B, C]: (String, String) = testType[_ ⇒ B]

    val res = result._1
    res shouldEqual "(<oc>_) → B"
  }

  it should "get printable representation of enclosing owner's type" in {
    def result[A, B, C]: (String, String) = testType[Int]

    result._2 shouldEqual "(<c>String, <c>String)"
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

    result._1 shouldEqual "(A) → B"
  }

  it should "get printable representation of fixed types with type constructors" in {
    def result[A, B, C]: (String, String) = testType[Option[Seq[Int]] ⇒ Option[List[Set[A]]] ⇒ B]

    result._1 shouldEqual "(None + <tc>Seq[Int]) → (None + <tc>List[Set[A]]) → B"
  }

  it should "get printable representation of fixed types with type constructors with [_]" in {
    def result[A, B, C]: (String, String) = testType[Option[_] ⇒ B]

    val res = result._1
    res shouldEqual "(None + <oc>_) → B"
  }

  it should "get printable representation of Option types" in {
    def result[A, B, C]: (String, String) = testType[Option[A] ⇒ Either[A, B]]

    result._1 shouldEqual "(None + A) → A + B"
  }

  it should "get printable representation of Any, Unit, and Nothing types" in {
    def result[A, B, C]: (String, String) = testType[Any ⇒ Nothing ⇒ Unit]

    result._1 shouldEqual "(<oc>_) → (0) → Unit"
  }

  it should "not confuse a type parameter with a type inheriting from Any" in {
    class Q

    def result[A, B, C]: (String, String) = testType[A ⇒ Q]

    result._1 shouldEqual "(A) → <oc>Q"
  }

  it should "get printable representation of tuple types" in {
    def result[A, B, C]: (String, String) = testType[(Any, Nothing, Unit, A, B, C)]

    result._1 shouldEqual "(<oc>_, 0, Unit, A, B, C)"
  }

  it should "get printable representation of tuple as function argument" in {
    def result[A, B, C]: (String, String) = testType[((A, B)) ⇒ C]

    result._1 shouldEqual "((A, B)) → C"
  }

  it should "get printable representation of tuple of basic types" in {
    def result[A, B, C]: (String, String) = testType[(Int, String, Boolean, Float, Double, Long, Symbol, Char)]

    result._1 shouldEqual "(" + CurryHowardMacros.basicTypes.map("<c>" + _).mkString(", ") + ")"
  }

  it should "get printable representation of case class" in {
    sealed trait Test1[T]
    case class A[T](x: Int, y: T) extends Test1[T]
    case class B(y: String) extends Test1[String]

    sealed abstract class Test2
    case class C(x: Int)(y: Double) extends Test2
    case class D(y: String) extends Test2

    def result[T]: (String, String) = testType[Test1[T] ⇒ Test2]

    result._1 shouldEqual "(<tc>Test1[T]) → <oc>Test2"
  }
}
