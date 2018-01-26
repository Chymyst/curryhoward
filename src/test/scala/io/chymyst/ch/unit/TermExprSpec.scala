package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class TermExprSpec extends FlatSpec with Matchers {

  behavior of "TermExpr#renameVar"

  val termExpr1 = CurriedE(List(VarE("x2", TP("3")), VarE("x3", TP("2")), VarE("x4", TP("1"))), VarE("x3", TP("2")))
  val termExpr2 = CurriedE(List(VarE("x2", TP("3")), VarE("x3", TP("2")), VarE("x4", TP("1"))), VarE("x1", TP("2")))
  val termExpr3 = CurriedE(List(VarE("x1", TP("2"))), termExpr2)

  it should "rename one variable" in {
    termExpr1.renameAllVars(Seq("x2"), Seq("y2")) shouldEqual CurriedE(List(VarE("y2", TP("3")), VarE("x3", TP("2")), VarE("x4", TP("1"))), VarE("x3", TP("2")))
    termExpr1.renameAllVars(Seq("x3"), Seq("y3")) shouldEqual CurriedE(List(VarE("x2", TP("3")), VarE("y3", TP("2")), VarE("x4", TP("1"))), VarE("y3", TP("2")))
    termExpr2.renameAllVars(Seq("x1"), Seq("y1")) shouldEqual CurriedE(List(VarE("x2", TP("3")), VarE("x3", TP("2")), VarE("x4", TP("1"))), VarE("y1", TP("2")))
    termExpr3.renameAllVars(Seq("x1"), Seq("y1")) shouldEqual CurriedE(List(VarE("y1", TP("2"))), CurriedE(List(VarE("x2", TP("3")), VarE("x3", TP("2")), VarE("x4", TP("1"))), VarE("y1", TP("2"))))
  }

  it should "rename multiple variables" in {
    termExpr1.renameAllVars(Seq("x2", "x3", "x4"), Seq("y2", "y3", "y4")) shouldEqual CurriedE(List(VarE("y2", TP("3")), VarE("y3", TP("2")), VarE("y4", TP("1"))), VarE("y3", TP("2")))
  }

  behavior of "TermExpr#propositions"

  it should "get the list of propositions" in {
    TermExpr.propositions(CurriedE(List(VarE("A", TP("A"))), AppE(VarE("B", TP("B") ->: TP("A")), VarE("B", TP("B"))))) shouldEqual Seq(VarE("A", TP("A")), VarE("B", TP("B") ->: TP("A")), VarE("B", TP("B")))
  }

  behavior of "TermExpr#prettyPrint"

  it should "rename variables without name clash" in {
    val a = VarE("a", TP("A") ->: TP("B"))
    val c = VarE("c", TP("A"))
    val b = VarE("b", ((TP("A") ->: TP("B")) ->: TP("B")) ->: TP("B"))
    // b ⇒ c ⇒ b (a ⇒ a c)  is of type (((A ⇒ B) ⇒ B) ⇒ B) ⇒ A ⇒ B
    val termExpr = CurriedE(List(b, c), AppE(b, CurriedE(List(a), AppE(a, c))))
    termExpr.toString shouldEqual "\\((b:((A ⇒ B) ⇒ B) ⇒ B) ⇒ (c:A) ⇒ (b \\((a:A ⇒ B) ⇒ (a c))))"
    termExpr.prettyPrint shouldEqual "a ⇒ b ⇒ a (c ⇒ c b)"
  }

  behavior of "TermExpr#freeVars"

  it should "detect free variables" in {
    termExpr1.freeVars shouldEqual Seq()
    termExpr2.freeVars shouldEqual Seq("x1")
    termExpr3.freeVars shouldEqual Seq()
  }

  behavior of "TermExpr#equiv"

  it should "detect equivalent terms" in {
    val termExpr1a = termExpr1.renameAllVars(Seq("x2", "x3", "x4"), Seq("y2", "y3", "y4"))
    TermExpr.equiv(termExpr1a, termExpr1) shouldEqual true
  }

  behavior of "information loss"

  it should "compute permutation score for conjunctions" in {
    val c = ConjunctE(Seq(VarE("a", TP("A")), VarE("b", TP("B"))))
    val t = ConjunctE(Seq(
      ProjectE(0, c),
      ProjectE(1, c)
    ))
    t.informationLossScore._3 shouldEqual 0

    ConjunctE(Seq(
      ProjectE(1, c),
      ProjectE(1, c)
    )).informationLossScore._3 shouldEqual 1

    ConjunctE(Seq(
      ProjectE(1, c),
      ProjectE(0, c)
    )).informationLossScore._3 shouldEqual 2

    TermExpr.findFirst(t) {
      case ProjectE(_, _) ⇒ "abc"
    } shouldEqual Some("abc")
  }

  behavior of "TermExpr#simplify"

  it should "simplify identity function application" in {
    val termExpr0 = VarE("y", TP("1"))
    val termExpr1 = CurriedE(List(VarE("x", TP("1") ->: TP("1"))), termExpr0) // x: A -> x
    val termExpr2 = AppE(termExpr1, VarE("z", TP("1") ->: TP("1")))
    termExpr2.simplifyOnce() shouldEqual termExpr0 // (x: A -> y)(z) == y
  }

  it should "simplify nested terms" in {
    val f1 = CurriedE(List(VarE("x4", TP("2")), VarE("x5", TP("1"))), VarE("x4", TP("2"))) // f1: (x4:B -> x5:A -> x4:B)
    val x2 = VarE("x2", TP("1") ->: TP("2")) // x2: A → B
    val x3 = VarE("x3", TP("1")) // x3: A
    val t1 = AppE(f1, VarE("y", TP("2")))
    t1.simplifyOnce() shouldEqual CurriedE(List(VarE("x5", TP("1"))), VarE("y", TP("2")))

    // x3:A -> (x2:A → B) -> (x4:B  -> x5:A -> x4:B) ( (x2:A → B)(x3:A) ) (x3:A)
    val termExpr4 = CurriedE(List(x3, x2), AppE(AppE(f1, AppE(x2, x3)), x3))
    termExpr4.simplifyOnce() shouldEqual CurriedE(List(x3, x2), AppE(x2, x3))
  }

  behavior of "Sequent#constructResultTerm"

  it should "produce result terms in correct order" in {
    val sequent = Sequent(List(TP("1"), TP("2"), TP("3")), TP("1"), new FreshIdents("t"))
    val premiseVars = sequent.premiseVars
    premiseVars shouldEqual List(VarE("t1", TP("1")), VarE("t2", TP("2")), VarE("t3", TP("3")))
    val term = sequent.constructResultTerm(premiseVars.head)
    term shouldEqual CurriedE(List(VarE("t1", TP("1")), VarE("t2", TP("2")), VarE("t3", TP("3"))), VarE("t1", TP("1")))
  }
}
