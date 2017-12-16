package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class LJTSpec2 extends FlatSpec with Matchers {

  behavior of "proof search, rule ->L1"

  it should "inhabit type using ->L1" in {
    val typeExpr = TP(1) ->: ((TP(1) ->: TP(2)) ->: TP(2))
    val proofs = TheoremProver.findProofs(typeExpr)
    proofs.length shouldEqual 1
  }

  behavior of "implicational fragment"

  it should "generate code for modus ponens" in {
    def f1[A, B]: A ⇒ (A ⇒ B) ⇒ B = implement

    def f2: Int ⇒ String = _.toString

    f1(123)(f2) shouldEqual "123"
  }
  it should "generate code for the 'lemma'" in {
    def f1[A, B, C]: ((A ⇒ B) ⇒ C) ⇒ B ⇒ C = implement

    val result: String => Boolean = f1[Int, String, Boolean](f ⇒ f(0) == "0")

    result("0") shouldEqual true

    result("abc") shouldEqual false

    "def f2[A, B,C] = ofType[((A ⇒ B) ⇒ C) ⇒ A ⇒ B]" shouldNot compile
  }

  it should "generate code for modus ponens with implication in premise" in {
    def f1[A, B, C] = ofType[(A ⇒ B) ⇒ ((A ⇒ B) ⇒ C) ⇒ C]
  }

  it should "generate the weak Peirce law" in {
    def f[A, B]: ((((A ⇒ B) ⇒ A) ⇒ A) ⇒ B) ⇒ B = implement

    "def f[A,B]: ((((A ⇒ B) ⇒ B) ⇒ A) ⇒ B) ⇒ B = implement" shouldNot compile
  }

}
