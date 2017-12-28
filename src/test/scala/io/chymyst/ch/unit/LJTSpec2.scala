package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class LJTSpec2 extends FlatSpec with Matchers {

  behavior of "proof search, rule ->L1"

  it should "inhabit type using ->L1" in {
    val typeExpr = TP(1) ->: ((TP(1) ->: TP(2)) ->: TP(2))
    val proofs = TheoremProver.findProofs(typeExpr)._1
    proofs.length shouldEqual 1
  }

  behavior of "functions using Unit"

  it should "generate code for a function returning Unit" in {
    def f1[A, B]: A ⇒ B ⇒ Unit = implement

    f1("abc")(123) shouldEqual (())

    def f2[A, B]: A ⇒ Unit ⇒ B ⇒ Unit = implement

    f2("abc")(())(123) shouldEqual (())

    def f3[A, B]: A ⇒ Unit ⇒ B ⇒ A = implement

    f3("abc")(())(123) shouldEqual "abc"
  }

  behavior of "implicational fragment"

  it should "generate code for modus ponens" in {
    def f1[A, B]: A ⇒ (A ⇒ B) ⇒ B = implement

    def f2: Int ⇒ String = _.toString

    f1(123)(f2) shouldEqual "123"
  }

  it should "generate code for the 'stepping stone lemma'" in {
    def f1[A, B, C]: ((A ⇒ B) ⇒ C) ⇒ B ⇒ C = implement

    val result: String => Boolean = f1[Int, String, Boolean](f ⇒ f(0) == "0")

    result("0") shouldEqual true

    result("abc") shouldEqual false

    "def f2[A, B,C] = ofType[((A ⇒ B) ⇒ C) ⇒ A ⇒ B]" shouldNot compile
  }

  it should "generate code for the 'Vorobieff lemma'" in {
    "def fVorobieff1[A, B, C]: (((A ⇒ B) ⇒ C) ⇒ A ⇒ B) ⇒ (B ⇒ C) ⇒ (A ⇒ B) = implement" shouldNot compile

    def fVorobieff2[A, B, C]: ((B ⇒ C) ⇒ (A ⇒ B)) ⇒ (((A ⇒ B) ⇒ C) ⇒ A ⇒ B) = implement
  }

  it should "deterministically rename variables in the proof terms before comparing" in {
    // This should not generate two different proof terms.
    def f2a[A, B, C]: ((B ⇒ C) ⇒ (A ⇒ B)) ⇒ (((A ⇒ B) ⇒ C) ⇒ B ⇒ C) = implement
  }

  it should "generate code for modus ponens with implication in premise" in {
    def f1[A, B, C] = ofType[(A ⇒ B) ⇒ ((A ⇒ B) ⇒ C) ⇒ C]
  }

  it should "generate the weak Peirce's law and related laws" in {
    // Weak Peirce's law.
    def f[A, B]: ((((A ⇒ B) ⇒ A) ⇒ A) ⇒ B) ⇒ B = implement

    // This cannot be implemented (weak double negation reduction).
    def h[A, B] = allOfType[((((A ⇒ B) ⇒ B) ⇒ A) ⇒ B) ⇒ B].length

    h[Int, String] shouldEqual 0
  }

  behavior of "product type projectors"

  it should "generate code that produces product types" in {
    // TODO: fix "notype" problem
    def f[A, B] = ofType[A ⇒ B ⇒ (A, B)]

    f(123)("abc") shouldEqual ((123, "abc"))

    def g[A, B]: A ⇒ B ⇒ (A, B) = implement

    g(123)("abc") shouldEqual ((123, "abc"))
  }

  val g: Int ⇒ String = _.toString

  it should "generate code that consumes product types" in {
    def f[A, B, C] = ofType[A ⇒ ((A ⇒ B, C)) ⇒ B]

    f(123)((g, "abc")) shouldEqual "123"
  }

  it should "generate code that consumes and produces product types" in {
    def f[A, B, C] = ofType[A ⇒ ((A ⇒ B, C)) ⇒ (B, C)]

    f(123)((g, "abc")) shouldEqual (("123", "abc"))

    def h[A, B, C] = ofType[A ⇒ ((A ⇒ B, A ⇒ C)) ⇒ (B, C)]

    h(123)((g, (i: Int) ⇒ i.toString + "abc")) shouldEqual (("123", "123abc"))
  }

  behavior of "constant types"

  it should "generate code that uses constant types" in {
    def f[A]: A ⇒ Int ⇒ A = implement

    f("abc")(123) shouldEqual "abc"

    def g[A]: A ⇒ Int ⇒ Int = implement

    g("abc")(123) shouldEqual 123
  }

  it should "generate code that uses constant types, product types, and function types" in {
    def f[A, B]: A ⇒ Int ⇒ (A, Int) = implement

    f("abc")(123) shouldEqual (("abc", 123))

    def g[A, B]: A ⇒ (Int ⇒ Int) ⇒ (A, Int ⇒ Int) = implement

    g("abc")(_ + 1)._2(100) shouldEqual 101
  }

  behavior of "misc. proof terms"

  it should "select implementation by argument usage counts" in {
    def f[A]: A ⇒ (A ⇒ A) ⇒ A = implement // Implement as b ⇒ a ⇒ a b rather than b ⇒ _ ⇒ b.

    f(123)(_ + 1) shouldEqual 124

    // Triple negation is equivalent to single negation. The "preferred" implementation is b ⇒ a ⇒ b (c ⇒ c a).
    def g[A, B]: (((A ⇒ B) ⇒ B) ⇒ B) ⇒ A ⇒ B = implement
  }

  behavior of "other examples"

  it should "generate code for reader monad's fmap" in {
    def f[E, A, B]: (A ⇒ B) ⇒ (E ⇒ A) ⇒ (E ⇒ B) = implement
  }

  it should "generate code using rule ->L2" in {
    def f[S, T, A, B]: ((A, S)) ⇒ (((A, S)) ⇒ (B, T)) ⇒ (B, T) = implement
  }

  it should "compute information loss penalty for tuples" in {
    def f[S, A, B]: ((A, S)) ⇒ (((A, S)) ⇒ (B, S)) ⇒ (B, S) = implement
  }

  it should "generate identity for tuples" in {
    def f1[A, B]: ((A, B)) ⇒ (A, B) = implement

    def f2[A, B, C]: A ⇒ ((A ⇒ B, C)) ⇒ (A, B) = implement
  }

  it should "generate code for state monad's fmap" in {
    def f[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ (S ⇒ (B, S)) = implement
  }

  it should "generate code for state monad update-mapping function using rule ->L2" in {
    def f[S, A, B]: (S ⇒ (A, S)) ⇒ (((A, S)) ⇒ (B, S)) ⇒ (S ⇒ (B, S)) = implement
  }

}
