package io.chymyst.ch.unit

import io.chymyst.ch.CurryHowardMacros._
import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class CurryHowardSpec extends FlatSpec with Matchers {
/* Not used now.
  behavior of "syntax for untyped functions"

  it should "define syntax for untyped lambda calculus with products" in {
    // This is for testing only. We need to represent terms by a polynomial type, not by functions.
    sealed trait Term {
      def apply[T](x: T): Term = this // dummy implementation to simplify code
    }

    final case class \[T](v: Term ⇒ T) extends Term

    final case class \:[T](v: Any ⇒ T) extends Term

    "(x => x)" shouldNot compile

    val a1 = \(x ⇒ \(y ⇒ x))

    def f1[X, Y]: X => Y => X = x => y => x

    val a2 = \(x ⇒ \(y ⇒ x(y)))
    val a3 = \(x ⇒ \(y ⇒ y((x, x))))
    val a4 = \(x ⇒ \(y ⇒ \(z ⇒ x(y(z)))))
    val a5 = \(x ⇒ \(y ⇒ \(z ⇒ z(x(y)))))
    val a6 = \(x ⇒ (x, \(y ⇒ (x, y, x(y))))) // can use tuples
    val qqq = \ { x ⇒ x(()) } // can apply to unit
    val a7 = \: { case (x: Term, y: Term) ⇒ x(y(x)) } // use tuples as argument types, need annotations
  }
*/

  behavior of "syntax of `implement` and `typeOf`"

  it should "compile" in {
    def f1[X, Y]: X ⇒ Y ⇒ X = implement

    // This does not work because `implement` needs to access the type of the enclosing owner!
    // The compiler error is "recursive method f2 needs type".
    " def f2a[X, Y] = implement[X ⇒ Y ⇒ X] " shouldNot compile

    def f2[X, Y] = ofType[X ⇒ Y ⇒ X]

    // does not work because ofType[Nothing] is instantiated: the macro does not use the enclosing owner.
    " def f3[X, Y]: X ⇒ Y ⇒ X = ofType " shouldNot compile
  }

  it should "get the list of propositions" in {
    TermExpr.propositions(CurriedE(List(PropE("A", TP("A"))), AppE(PropE("B", TP("B") ->: TP("A")), PropE("B", TP("B"))))) shouldEqual Set(PropE("A", TP("A")), PropE("B", TP("B")), PropE("B", TP("B") ->: TP("A")))
  }

  it should "generate correct code for the identity function with `ofType[]` syntax" in {
    def f1[X] = ofType[X ⇒ X]

    f1(123) shouldEqual 123
    f1("abc") shouldEqual "abc"
    f1(true) shouldEqual true
  }

  it should "generate correct code for the const function with `ofType[]` syntax" in {
    def f2[X, Y] = ofType[X ⇒ Y ⇒ X]

    val cTrue = f2(true)
    cTrue(123) shouldEqual true
    cTrue("abc") shouldEqual true
    cTrue(true) shouldEqual true

    val c3 = f2(3)
    c3(123) shouldEqual 3
    c3("abc") shouldEqual 3
    c3(true) shouldEqual 3
  }

  it should "generate correct code for the permuted const function with `ofType[]` syntax" in {
    def f2[X, Y]: X ⇒ Y ⇒ Y = implement

    f2(123)("true") shouldEqual "true"
    f2(false)(1.0) shouldEqual 1.0
  }

  it should "generate correct code for the identity function with standard syntax" in {
    def f1[X]: X ⇒ X = implement

    f1(123) shouldEqual 123
    f1("abc") shouldEqual "abc"
    f1(true) shouldEqual true
  }

  it should "generate correct code for the const function with standard syntax" in {
    def f2[X, Y]: X ⇒ Y ⇒ X = implement

    val cTrue = f2(true)
    cTrue(123) shouldEqual true
    cTrue("abc") shouldEqual true
    cTrue(true) shouldEqual true

    val c3 = f2(3)
    c3(123) shouldEqual 3
    c3("abc") shouldEqual 3
    c3(true) shouldEqual 3
  }

  it should "fail to compile when two possible implementations are equally good" in {
    "def f1[X, A, B]: X ⇒ A ⇒ X ⇒ X = implement" shouldNot compile
  }

  it should "generate correct code for the const function with extra unused arguments" in {
    def f1[X, A, B]: X ⇒ A ⇒ B ⇒ X = implement

    f1(123)("q")(true) shouldEqual 123
    f1("abc")(Some((1, 1)))(Map()) shouldEqual "abc"
    f1(true)(123.0)('blah) shouldEqual true
  }

  it should "generate correct code for the identity function on a=>b" in {
    def f2[X, Y]: (X ⇒ Y) ⇒ X ⇒ Y = implement

        val printInt: Int ⇒ String = _.toString

        f2(printInt)(123) shouldEqual "123"
  }

  it should "generate correct code for the const function with more unused arguments of coincident type" in {
    def f1[X, A, B]: A ⇒ X ⇒ A ⇒ B ⇒ X = implement

    f1("b")(123)("q")(true) shouldEqual 123
    f1(Some((3, 4)))("abc")(Some((1, 1)))(Map()) shouldEqual "abc"
    f1(0.0)(true)(123.0)('blah) shouldEqual true
  }

  // TODO: make this work too!
  it should "generate correct code for the identity function with explicit arguments" in {
    "def f1[X](x: X): X = implement" shouldNot compile
    //
    //    f1(123) shouldEqual 123
    //    f1("abc") shouldEqual "abc"
    //    f1(true) shouldEqual true
  }
}
