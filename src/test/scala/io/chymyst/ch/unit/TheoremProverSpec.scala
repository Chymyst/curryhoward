package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TheoremProverSpec extends AnyFlatSpec with Matchers {

  private val freshVar = new FreshIdents(prefix = "x")

  behavior of "constant types"

  it should "treat constant types as unique type variables" in {
    val sequent = Sequent(List(BasicT("1"), BasicT("2")), BasicT("1"), freshVar)
    val terms = TheoremProver.findTermExprs(sequent)
    terms.length shouldEqual 1
    TermExpr.equiv(terms.head, CurriedE(List(VarE("a", BasicT("1")), VarE("b", BasicT("2"))), VarE("a", BasicT("1")))) shouldEqual true
  }

  it should "treat constant types as unique type variables and obtain two implementations" in {
    val sequent = Sequent(List(BasicT("1"), BasicT("1")), BasicT("1"), freshVar)
    val terms = TheoremProver.findTermExprs(sequent)
    terms.length shouldEqual 2
    TermExpr.equiv(terms.head, CurriedE(List(VarE("a", BasicT("1")), VarE("b", BasicT("1"))), VarE("a", BasicT("1")))) shouldEqual true
    TermExpr.equiv(terms(1), CurriedE(List(VarE("a", BasicT("1")), VarE("b", BasicT("1"))), VarE("b", BasicT("1")))) shouldEqual true
  }

  behavior of "named conjunctions"

  it should "implement trivial unwrapping" in {
    val aT = TP("1")
    val wrappedT = NamedConjunctT("MyWrapper", List(), List("name"), List(aT))

    val sequent1 = Sequent(List(wrappedT), aT, freshVar)
    val terms1 = TheoremProver.findTermExprs(sequent1)
    terms1.length shouldEqual 1
    terms1.head.prettyRenamePrint shouldEqual "a ⇒ a.name"
  }

  it should "implement trivial wrapping" in {
    val aT = TP("1")
    val wrappedT = NamedConjunctT("MyWrapper", List(), List("name"), List(aT))

    val sequent2 = Sequent(List(aT), wrappedT, freshVar)
    val terms2 = TheoremProver.findTermExprs(sequent2)
    terms2.length shouldEqual 1
    terms2.head.prettyRenamePrint shouldEqual "a ⇒ MyWrapper(a)"
    terms2.head.toString shouldEqual "\\((x7:1) ⇒ MyWrapper(x7))"
  }

  it should "implement wrapping and unwrapping for higher multiplicity and nested usage" in {
    val aT = TP("1")
    val bT = TP("2")
    val cT = TP("3")
    val wrapped12T = NamedConjunctT("MyWrapper12", List(), List("a1", "b1"), List(aT, bT))
    val wrapped23T = NamedConjunctT("MyWrapper23", List(), List("b2", "c3"), List(bT, cT))
    val wrapped123abT = NamedConjunctT("MyWrapper123ab", List(), List("ab3", "c3"), List(wrapped12T, cT))
    val wrapped123bcT = NamedConjunctT("MyWrapper123bc", List(), List("a4", "bc4"), List(aT, wrapped23T))

    val sequent = Sequent(List(wrapped123abT), wrapped123bcT, freshVar)
    val terms = TheoremProver.findTermExprs(sequent)
    terms.length shouldEqual 1
    terms.head.prettyRenamePrint shouldEqual "a ⇒ MyWrapper123bc(a.ab3.a1, MyWrapper23(a.ab3.b1, a.c3))"
  }
}
