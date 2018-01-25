package io.chymyst.ch.unit

import io.chymyst.ch.LJT.followsFromAxioms
import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class LJTSpec extends FlatSpec with Matchers {

  private val freshVar = new FreshIdents(prefix = "x")

  /*

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

  // Note: we no longer use the subformula dictionary in the theorem prover.
  it should "correctly compute LJT subformulas" in {
    subformulas[Int](TP("1") :-> TP("1")) shouldEqual Set(TP("1") :-> TP("1"), TP("1"))
    subformulas[Int](TP("1") :-> TP("2")) shouldEqual Set(TP("1") :-> TP("2"), TP("1"), TP("2"))
    subformulas[Int](TP("1") :-> (TP("2") :-> TP("3"))) shouldEqual Set(TP("1") :-> (TP("2") :-> TP("3")), TP("2") :-> TP("3"), TP("1"), TP("2"), TP("3"))

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
    subformulas[Int](DisjunctT(Seq(TP("1"), TP("2"))) :-> TP("3")) shouldEqual Set(
      DisjunctT(Seq(TP("1"), TP("2"))) :-> TP("3"),
      DisjunctT(Seq(TP("1"), TP("2"))),
      TP("1") :-> TP("3"),
      TP("2") :-> TP("3"),
      TP("1"), TP("2"), TP("3")
    )

    // Conjunctions.
    subformulas[Int](ConjunctT(Seq(TP("1"), TP("2"), TP("3"))) :-> TP(4)) shouldEqual Set(
      ConjunctT(Seq(TP("1"), TP("2"), TP("3"))) :-> TP(4),
      ConjunctT(Seq(TP("1"), TP("2"), TP("3"))),
      TP("1") :-> (TP("2") :-> (TP("3") :-> TP(4))),
      TP("2") :-> (TP("3") :-> TP(4)),
      TP("3") :-> TP(4),
      TP("1"), TP("2"), TP("3"), TP(4)
    )
  }
*/

  behavior of "proof search - internal details"

  it should "correctly explode sequences of integers" in {
    TheoremProver.explode[Int](Seq(Seq(1, 2))) shouldEqual Seq(Seq(1), Seq(2))
    TheoremProver.explode[Int](Seq(Seq(1, 2), Seq())) shouldEqual Seq()
    TheoremProver.explode[Int](Seq(Seq())) shouldEqual Seq()
    TheoremProver.explode[Int](Seq()) shouldEqual Seq(Seq())
    TheoremProver.explode[Int](Seq(Seq(1, 2), Seq(10, 20, 30))) shouldEqual Seq(Seq(1, 10), Seq(1, 20), Seq(1, 30), Seq(2, 10), Seq(2, 20), Seq(2, 30))
  }

  it should "correctly produce proofs from the Id axiom" in {
    followsFromAxioms(Sequent(List(TP("3"), TP("2"), TP("1")), TP("0"), freshVar)) shouldEqual ((Seq(), Seq()))

    followsFromAxioms(Sequent(List(TP("3"), TP("2"), TP("1")), TP("1"), freshVar)) shouldEqual ((Seq(
      CurriedE(List(VarE("x4", TP("3")), VarE("x5", TP("2")), VarE("x6", TP("1"))), VarE("x6", TP("1")))
    ), Seq()))
  }

  it should "produce several proofs from the Id axiom" in {
    followsFromAxioms(Sequent(List(TP("1"), TP("2"), TP("1")), TP("1"), freshVar)) shouldEqual ((Seq(
      CurriedE(List(VarE("x7", TP("1")), VarE("x8", TP("2")), VarE("x9", TP("1"))), VarE("x7", TP("1"))),
      CurriedE(List(VarE("x7", TP("1")), VarE("x8", TP("2")), VarE("x9", TP("1"))), VarE("x9", TP("1")))
    ), Seq()))
  }

  it should "find proof term for given sequent with premises" in {
    val sequent = Sequent(List(TP("1")), TP("1"), freshVar)
    val terms = TheoremProver.findTermExprs(sequent)
    terms.length shouldEqual 1
    TermExpr.equiv(terms.head, CurriedE(List(VarE("x1", TP("1"))), VarE("x1", TP("1")))) shouldEqual true

    val sequent2 = Sequent(List(TP("3"), TP("2"), TP("1")), TP("2"), freshVar)
    val terms2 = TheoremProver.findTermExprs(sequent2)
    terms2.length shouldEqual 1
    TermExpr.equiv(terms2.head,
      CurriedE(List(VarE("x2", TP("3")), VarE("x3", TP("2")), VarE("x4", TP("1"))), VarE("x3", TP("2")))
    ) shouldEqual true
  }

  it should "find proof term for the K combinator" in {
    val sequent = Sequent(List(TP("1"), TP("2")), TP("1"), freshVar)
    val terms = TheoremProver.findTermExprs(sequent)
    terms.length shouldEqual 1
    TermExpr.equiv(terms.head, CurriedE(List(VarE("a", TP("1")), VarE("b", TP("2"))), VarE("a", TP("1")))) shouldEqual true
  }

  behavior of "proof search - high-level API, rule ->R"

  it should "find proof term for the I combinator using rule ->R" in {
    val typeExpr = TP("1") ->: TP("1")
    val proofs = TheoremProver.findProofs(typeExpr)._1
    proofs.length shouldEqual 1
    TermExpr.equiv(proofs.head, CurriedE(List(VarE("x", TP("1"))), VarE("x", TP("1")))) shouldEqual true
  }

  it should "find proof term for the K combinator using rule ->R" in {
    val typeExpr = TP("1") ->: TP("2") ->: TP("1")
    val proofs = TheoremProver.findProofs(typeExpr)._1
    proofs.length shouldEqual 1
    println(proofs.head)
    TermExpr.equiv(proofs.head, CurriedE(List(VarE("x", TP("1")), VarE("y", TP("2"))), VarE("x", TP("1")))) shouldEqual true
  }

  it should "find proof term for the flipped K combinator using rule ->R" in {
    val typeExpr = TP("2") ->: TP("1") ->: TP("1")
    val proofs = TheoremProver.findProofs(typeExpr)._1
    proofs.length shouldEqual 1
    println(proofs.head)
    TermExpr.equiv(proofs.head, CurriedE(List(VarE("x", TP("2")), VarE("y", TP("1"))), VarE("y", TP("1")))) shouldEqual true
  }

  it should "find proof term for the constant function with 2 arguments using rule ->R" in {
    val typeExpr = TP("0") ->: TP("1") ->: TP("2") ->: TP("0")
    val proofs = TheoremProver.findProofs(typeExpr)._1
    proofs.length shouldEqual 1
    println(proofs.head)
    TermExpr.equiv(proofs.head, CurriedE(List(VarE("x", TP("0")), VarE("y", TP("1")), VarE("z", TP("2"))), VarE("x", TP("0")))) shouldEqual true
  }

  it should "find proof term for the 1-switched constant function with 2 arguments using rule ->R" in {
    val typeExpr = TP("0") ->: TP("1") ->: TP("2") ->: TP("1")
    val proofs = TheoremProver.findProofs(typeExpr)._1
    proofs.length shouldEqual 1
    println(proofs.head)
    TermExpr.equiv(proofs.head, CurriedE(List(VarE("x", TP("0")), VarE("y", TP("1")), VarE("z", TP("2"))), VarE("y", TP("1")))) shouldEqual true
  }

  it should "find proof term for the 2-switched constant function with 2 arguments using rule ->R" in {
    val typeExpr = TP("0") ->: TP("1") ->: TP("2") ->: TP("2")
    val proofs = TheoremProver.findProofs(typeExpr)._1
    proofs.length shouldEqual 1
    println(proofs.head)
    TermExpr.equiv(proofs.head, CurriedE(List(VarE("x", TP("0")), VarE("y", TP("1")), VarE("z", TP("2"))), VarE("z", TP("2")))) shouldEqual true
  }

  behavior of "proof search - high-level API, rule +Rn"

  it should "find proof term for simple instance of +Rn" in {
    val disjunctT = DisjunctT("12", Seq(), Seq(TP("1"), TP("2")))
    val typeExpr = TP("1") ->: disjunctT
    val proofs = TheoremProver.findProofs(typeExpr)._1
    proofs.length shouldEqual 1
    TermExpr.equiv(proofs.head, CurriedE(List(VarE("x", TP("1"))), DisjunctE(0, 2, VarE("x", TP("1")), disjunctT))) shouldEqual true
  }

  it should "find proof term for simple instance of +Rn with several disjuncts" in {
    val disjunctT = DisjunctT("123", Seq(), Seq(TP("1"), TP("2"), TP("3")))
    val typeExpr = TP("2") ->: disjunctT
    val proofs = TheoremProver.findProofs(typeExpr)._1
    proofs.length shouldEqual 1
    TermExpr.equiv(proofs.head, CurriedE(List(VarE("x", TP("2"))), DisjunctE(1, 3, VarE("x", TP("2")), disjunctT))) shouldEqual true
  }

  it should "inhabit type using +Rn" in {
    def f[X]: X ⇒ Option[X] = implement

    f(1) shouldEqual Some(1)

    def g[X, Y]: X ⇒ Either[X, Y] = implement

    g("abc") shouldEqual Left("abc")
  }

}

