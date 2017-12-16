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
  /* TODO: fix spurious repeated inhabitation of this

  Information:12/15/17, 23:56 - Compilation completed with 1 error and 21 warnings in 1s 129ms
  Warning:scalac: debug: applied rule ->R to sequent [() |- (A) → ((A) → B) → B], new sequents List([(A) |- ((A) → B) → B])
  Warning:scalac:
  Warning:scalac: debug: applied rule ->R to sequent [(A) |- ((A) → B) → B], new sequents List([((A) → B, A) |- B])
  Warning:scalac: debug: applied rule ->L1 to sequent [((A) → B, A) |- B], new sequents List([(B, A) |- B])
  Warning:scalac: debug: sequent [(B, A) |- B] followsFromAxioms: List(\((x4:B) -> (x5:A) -> (x4:B)))
  Warning:scalac: debug: transformed proof terms List(\((x2:(A) → B) -> (x3:A) -> ((\((x4:B) -> (x5:A) -> (x4:B)))(((x2:(A) → B))((x3:A))))((x3:A))))
  Warning:scalac: debug: transformed proof terms List(\((x3:A) -> (x2:(A) → B) -> ((\((x4:B) -> (x5:A) -> (x4:B)))(((x2:(A) → B))((x3:A))))((x3:A))))
  Warning:scalac: DEBUG: Term found: \((x3:A) -> (x2:(A) → B) -> ((\((x4:B) -> (x5:A) -> (x4:B)))(((x2:(A) → B))((x3:A))))((x3:A))), propositions: Set((x3:A), (x2:(A) → B), (x4:B), (x5:A))
  Warning:scalac: DEBUG: returning code: ((t_x3: A) => ((t_x2: _root_.scala.Function1[A, B]) => ((t_x4: B) => ((t_x5: A) => t_x4))(t_x2(t_x3))(t_x3)))
  Warning:scalac: debug: applied rule ->R to sequent [() |- (((A) → B) → C) → (B) → C], new sequents List([(((A) → B) → C) |- (B) → C])
  Warning:scalac: debug: applied rule ->R to sequent [(((A) → B) → C) |- (B) → C], new sequents List([(B, ((A) → B) → C) |- C])
  Warning:scalac: debug: applied rule ->L4 to sequent [(B, ((A) → B) → C) |- C], new sequents List([((B) → C, B) |- (A) → B], [(C, B) |- C])
  Warning:scalac: debug: applied rule ->R to sequent [((B) → C, B) |- (A) → B], new sequents List([(A, (B) → C, B) |- B])
  Warning:scalac: debug: sequent [(A, (B) → C, B) |- B] followsFromAxioms: List(\((x11:A) -> (x12:(B) → C) -> (x13:B) -> (x13:B)))
  Warning:scalac: debug: applied rule ->L1 to sequent [(A, (B) → C, B) |- B], new sequents List([(C, A, B) |- B])
  Warning:scalac: debug: sequent [(C, A, B) |- B] followsFromAxioms: List(\((x14:C) -> (x15:A) -> (x16:B) -> (x16:B)))
  Warning:scalac: debug: transformed proof terms List(\((x11:A) -> (x12:(B) → C) -> (x13:B) -> (((\((x14:C) -> (x15:A) -> (x16:B) -> (x16:B)))(((x12:(B) → C))((x13:B))))((x11:A)))((x13:B))))
  Warning:scalac: debug: transformed proof terms List(\((x12:(B) → C) -> (x13:B) -> (x11:A) -> (((\((x14:C) -> (x15:A) -> (x16:B) -> (x16:B)))(((x12:(B) → C))((x13:B))))((x11:A)))((x13:B))), \((x12:(B) → C) -> (x13:B) -> (x11:A) -> (x13:B)))
  Warning:scalac: debug: sequent [(C, B) |- C] followsFromAxioms: List(\((x17:C) -> (x18:B) -> (x17:C)))
  Warning:scalac: debug: transformed proof terms List(\((x7:B) -> (x8:((A) → B) → C) -> ((\((x12:(B) → C) -> (x13:B) -> (x11:A) -> (((\((x14:C) -> (x15:A) -> (x16:B) -> (x16:B)))(((x12:(B) → C))((x13:B))))((x11:A)))((x13:B))))(((x8:((A) → B) → C))(((\((x17:C) -> (x18:B) -> (x17:C)))(\((x19:B) -> ((x8:((A) → B) → C))(\((x20:A) -> (x19:B))))))((x7:B)))))((x7:B))), \((x7:B) -> (x8:((A) → B) → C) -> ((\((x12:(B) → C) -> (x13:B) -> (x11:A) -> (x13:B)))(((x8:((A) → B) → C))(((\((x17:C) -> (x18:B) -> (x17:C)))(\((x21:B) -> ((x8:((A) → B) → C))(\((x22:A) -> (x21:B))))))((x7:B)))))((x7:B))))
  Warning:scalac: debug: transformed proof terms List(\((x8:((A) → B) → C) -> (x7:B) -> ((\((x12:(B) → C) -> (x13:B) -> (x11:A) -> (((\((x14:C) -> (x15:A) -> (x16:B) -> (x16:B)))(((x12:(B) → C))((x13:B))))((x11:A)))((x13:B))))(((x8:((A) → B) → C))(((\((x17:C) -> (x18:B) -> (x17:C)))(\((x19:B) -> ((x8:((A) → B) → C))(\((x20:A) -> (x19:B))))))((x7:B)))))((x7:B))), \((x8:((A) → B) → C) -> (x7:B) -> ((\((x12:(B) → C) -> (x13:B) -> (x11:A) -> (x13:B)))(((x8:((A) → B) → C))(((\((x17:C) -> (x18:B) -> (x17:C)))(\((x21:B) -> ((x8:((A) → B) → C))(\((x22:A) -> (x21:B))))))((x7:B)))))((x7:B))))
  /Users/sergei.winitzki/Code/curryhoward/src/test/scala/io/chymyst/ch/unit/LJTSpec2.scala
  Error:(52, 46) type (((A) → B) → C) → (B) → C can be inhabited in 2 different ways
      def f1[A, B, C]: ((A ⇒ B) ⇒ C) ⇒ B ⇒ C = implement
  */
  it should "generate code for the 'lemma'" in {
    def f1[A, B, C]: ((A ⇒ B) ⇒ C) ⇒ B ⇒ C = implement

    val result: String => Boolean = f1[Int, String, Boolean](f ⇒ f(0) == "0")

    result("0") shouldEqual true

    result("abc") shouldEqual false

    "def f2[A, B,C] = ofType[((A ⇒ B) ⇒ C) ⇒ A ⇒ B]" shouldNot compile
  }

//  it should "generate code for modus ponens with implication in premise" in {
//    def f1[A, B, C] = ofType[(A ⇒ B) ⇒ ((A ⇒ B) ⇒ C) ⇒ C]
//  }

}
