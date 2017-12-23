package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class TheoremProverSpec extends FlatSpec with Matchers {

  private val freshVar = new FreshIdents(prefix = "x")

  behavior of "constant types"

  it should "treat constant types as unique type variables" in {
    val sequent = Sequent(List(OtherT(1), BasicT(2)), OtherT(1), freshVar)
    val terms = TheoremProver.findProofTerms(sequent)
    terms.length shouldEqual 1
    TermExpr.equiv(terms.head, CurriedE(List(PropE("a", OtherT(1)), PropE("b", BasicT(2))), PropE("a", OtherT(1)))) shouldEqual true
  }

  it should "treat constant types as unique type variables and obtain two implementations" in {
    val sequent = Sequent(List(BasicT(1), BasicT(1)), BasicT(1), freshVar)
    val terms = TheoremProver.findProofTerms(sequent)
    terms.length shouldEqual 2
    TermExpr.equiv(terms.head, CurriedE(List(PropE("a", BasicT(1)), PropE("b", BasicT(1))), PropE("a", BasicT(1)))) shouldEqual true
    TermExpr.equiv(terms(1), CurriedE(List(PropE("a", BasicT(1)), PropE("b", BasicT(1))), PropE("b", BasicT(1)))) shouldEqual true
  }
}
