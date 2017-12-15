package io.chymyst.ch.unit

import io.chymyst.ch.LJT.followsFromAxioms
import io.chymyst.ch.{:->, ConjunctT, CurriedE, DisjunctE, DisjunctT, FreshIdents, PropE, Sequent, TP, TheoremProver, TypeExpr}

import org.scalatest.{FlatSpec, Matchers}

class LJTSpec extends FlatSpec with Matchers {


  // This is now only for testing. Subformula property may not hold in a further iteration of the theorem prover,
  // and it is not bringing an obvious advantage to the implementation.
  object Subformulas {
    private val freshSubformulas = new FreshIdents(prefix = "f")

    def subformulas[T](typeStructure: TypeExpr[T]): Set[TypeExpr[T]] = Set(typeStructure) ++ (typeStructure match {
      case DisjunctT(terms) ⇒ terms.flatMap(subformulas)
      case ConjunctT(terms) ⇒ terms.flatMap(subformulas)
      case head :-> body ⇒ subformulas(head) ++ subformulas(body) ++ (head match {
        case DisjunctT(terms) ⇒ terms.flatMap(t ⇒ subformulas(:->(t, body)))
        case ConjunctT(terms) ⇒ subformulas(terms.foldRight(body) { case (t, prev) ⇒ t :-> prev })
        case _ :-> bd ⇒ subformulas(bd :-> body) // Special subformula case for implication of the form (hd ⇒ bd) ⇒ body
        case _ ⇒ Seq() // `head` is an atomic type
      })
      case _ ⇒ Seq() // `typeStructure` is an atomic type
    }).toSet
  }

  import Subformulas._

  behavior of "proof search - internal details"

  it should "correctly explode sequences of integers" in {
    TheoremProver.explode[Int](Seq(Seq(1, 2))) shouldEqual Seq(Seq(1), Seq(2))
    TheoremProver.explode[Int](Seq(Seq(1, 2), Seq())) shouldEqual Seq()
    TheoremProver.explode[Int](Seq(Seq())) shouldEqual Seq()
    TheoremProver.explode[Int](Seq()) shouldEqual Seq(Seq())
    TheoremProver.explode[Int](Seq(Seq(1, 2), Seq(10, 20, 30))) shouldEqual Seq(Seq(1, 10), Seq(1, 20), Seq(1, 30), Seq(2, 10), Seq(2, 20), Seq(2, 30))
  }

  private val freshVar = TheoremProver.freshVar

  it should "correctly produce proofs from the Id axiom" in {
    followsFromAxioms(Sequent[Int](List(TP(3), TP(2), TP(1)), TP(0), freshVar)) shouldEqual Seq()

    followsFromAxioms(Sequent[Int](List(TP(3), TP(2), TP(1)), TP(1), freshVar)) shouldEqual Seq(
      CurriedE(List(PropE("x4", TP(3)), PropE("x5", TP(2)), PropE("x6", TP(1))), PropE("x6", TP(1)))
    )
  }
  it should "produce several proofs from the Id axiom" in {
    followsFromAxioms(Sequent[Int](List(TP(1), TP(2), TP(1)), TP(1), freshVar)) shouldEqual Seq(
      CurriedE(List(PropE("x7", TP(1)), PropE("x8", TP(2)), PropE("x9", TP(1))), PropE("x7", TP(1))),
      CurriedE(List(PropE("x7", TP(1)), PropE("x8", TP(2)), PropE("x9", TP(1))), PropE("x9", TP(1)))
    )
  }

  // Note: we no longer use the subformula dictionary in the theorem prover.
  it should "correctly compute LJT subformulas" in {
    subformulas[Int](TP(1) :-> TP(1)) shouldEqual Set(TP(1) :-> TP(1), TP(1))
    subformulas[Int](TP(1) :-> TP(2)) shouldEqual Set(TP(1) :-> TP(2), TP(1), TP(2))
    subformulas[Int](TP(1) :-> (TP(2) :-> TP(3))) shouldEqual Set(TP(1) :-> (TP(2) :-> TP(3)), TP(2) :-> TP(3), TP(1), TP(2), TP(3))

    // Example from the paper.
    subformulas[String](((TP("A") :-> TP("A")) :-> TP("C")) :-> TP("C")) shouldEqual Set(
      ((TP("A") :-> TP("A")) :-> TP("C")) :-> TP("C"),
      (TP("A") :-> TP("A")) :-> TP("C"),
      TP("A") :-> TP("A"),
      TP("A") :-> TP("C"),
      TP("C") :-> TP("C"),
      TP("A"), TP("C")
    )

    // Disjunctions.
    subformulas[Int](DisjunctT(Seq(TP(1), TP(2))) :-> TP(3)) shouldEqual Set(
      DisjunctT(Seq(TP(1), TP(2))) :-> TP(3),
      DisjunctT(Seq(TP(1), TP(2))),
      TP(1) :-> TP(3),
      TP(2) :-> TP(3),
      TP(1), TP(2), TP(3)
    )

    // Conjunctions.
    subformulas[Int](ConjunctT(Seq(TP(1), TP(2), TP(3))) :-> TP(4)) shouldEqual Set(
      ConjunctT(Seq(TP(1), TP(2), TP(3))) :-> TP(4),
      ConjunctT(Seq(TP(1), TP(2), TP(3))),
      TP(1) :-> (TP(2) :-> (TP(3) :-> TP(4))),
      TP(2) :-> (TP(3) :-> TP(4)),
      TP(3) :-> TP(4),
      TP(1), TP(2), TP(3), TP(4)
    )
  }

  it should "find proof term for given sequent with premises" in {
    val sequent = Sequent(List(TP(1)), TP(1), freshVar)
    TheoremProver.findProofTerms(sequent) shouldEqual Seq(CurriedE(List(PropE("x10", TP(1))), PropE("x10", TP(1))))
    val sequent2 = Sequent(List(TP(3), TP(2), TP(1)), TP(2), freshVar)
    TheoremProver.findProofTerms(sequent2) shouldEqual Seq(
      CurriedE(List(PropE("x11", TP(3)), PropE("x12", TP(2)), PropE("x13", TP(1))), PropE("x12", TP(2)))
    )
  }

  behavior of "proof search - high-level API, rule ->R"

  it should "find proof term for the I combinator using rule ->R" in {
    val typeExpr = TP(1) :-> TP(1)
    val proofs = TheoremProver.findProofs(typeExpr)
    proofs shouldEqual Seq(CurriedE(List(PropE("x14", TP(1))), PropE("x14", TP(1))))
  }

  it should "find proof term for the K combinator using rule ->R" in {
    val typeExpr = TP(1) :-> (TP(2) :-> TP(1))
    val proofs = TheoremProver.findProofs(typeExpr)
    proofs shouldEqual Seq(
      CurriedE(List(PropE("x16", TP(2)), PropE("x17", TP(1))), PropE("x17", TP(1)))
    )
  }

  behavior of "proof search - high-level API, rule +Rn"

  it should "find proof term for simple instance of +Rn" in {
    val typeExpr = TP(1) :-> DisjunctT(Seq(TP(1), TP(2)))
    val proofs = TheoremProver.findProofs(typeExpr)
    proofs shouldEqual Seq(CurriedE(List(PropE("x19", TP(1))), DisjunctE(0, 2, PropE("x19", TP(1)), DisjunctT(Seq(TP(1), TP(2))))))
  }

  it should "find proof term for simple instance of +Rn with several disjuncts" in {
    val typeExpr = TP(2) :-> DisjunctT(Seq(TP(1), TP(2), TP(3)))
    val proofs = TheoremProver.findProofs(typeExpr)
    proofs shouldEqual Seq(CurriedE(List(PropE("x23", TP(2))), DisjunctE(1, 3, PropE("x23", TP(2)), DisjunctT(Seq(TP(1), TP(2), TP(3))))))
  }

  it should "inhabit type using +Rn" in {
    "def f[X]: X ⇒ Option[X] = implement" shouldNot compile

    //    f(1) shouldEqual Some(1)

    //    def f[X, Y]: X ⇒ Either[X, Y] = implement
  }

}

