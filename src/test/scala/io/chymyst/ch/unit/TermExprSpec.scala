package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class TermExprSpec extends FlatSpec with Matchers {

  behavior of "TermExpr#renameVar"

  val termExpr1 = CurriedE(List(PropE("x2", TP(3)), PropE("x3", TP(2)), PropE("x4", TP(1))), PropE("x3", TP(2)))
  val termExpr2 = CurriedE(List(PropE("x2", TP(3)), PropE("x3", TP(2)), PropE("x4", TP(1))), PropE("x1", TP(2)))
  val termExpr3 = CurriedE(List(PropE("x1", TP(2))), termExpr2)

  it should "rename one variable" in {
    termExpr1.renameAllVars(Seq("x2"), Seq("y2")) shouldEqual CurriedE(List(PropE("y2", TP(3)), PropE("x3", TP(2)), PropE("x4", TP(1))), PropE("x3", TP(2)))
    termExpr1.renameAllVars(Seq("x3"), Seq("y3")) shouldEqual CurriedE(List(PropE("x2", TP(3)), PropE("y3", TP(2)), PropE("x4", TP(1))), PropE("y3", TP(2)))
    termExpr2.renameAllVars(Seq("x1"), Seq("y1")) shouldEqual CurriedE(List(PropE("x2", TP(3)), PropE("x3", TP(2)), PropE("x4", TP(1))), PropE("y1", TP(2)))
    termExpr3.renameAllVars(Seq("x1"), Seq("y1")) shouldEqual CurriedE(List(PropE("y1", TP(2))), CurriedE(List(PropE("x2", TP(3)), PropE("x3", TP(2)), PropE("x4", TP(1))), PropE("y1", TP(2))))
  }

  it should "rename multiple variables" in {
    termExpr1.renameAllVars(Seq("x2", "x3", "x4"), Seq("y2", "y3", "y4")) shouldEqual CurriedE(List(PropE("y2", TP(3)), PropE("y3", TP(2)), PropE("y4", TP(1))), PropE("y3", TP(2)))
  }

  behavior of "TermExpr#propositions"

  it should "get the list of propositions" in {
    TermExpr.propositions(CurriedE(List(PropE("A", TP("A"))), AppE(PropE("B", TP("B") ->: TP("A")), PropE("B", TP("B"))))) shouldEqual Seq(PropE("A", TP("A")), PropE("B", TP("B") ->: TP("A")), PropE("B", TP("B")))
  }

  behavior of "TermExpr#prettyPrint"

  it should "rename variables without name clash" in {
    val a = PropE("a", TP("A") ->: TP("B"))
    val c = PropE("c", TP("A"))
    val b = PropE("b", ((TP("A") ->: TP("B")) ->: TP("B")) ->: TP("B"))
    // b ⇒ c ⇒ b (a ⇒ a c)  is of type (((A ⇒ B) ⇒ B) ⇒ B) ⇒ A ⇒ B
    val termExpr = CurriedE(List(b, c), AppE(b, CurriedE(List(a), AppE(a, c))))
    termExpr.toString shouldEqual "\\((b:((A ⇒ B) ⇒ B) ⇒ B) ⇒ (c:A) ⇒ (b \\((a:A ⇒ B) ⇒ (a c))))"
    termExpr.prettyPrint shouldEqual "a ⇒ c ⇒ a (b ⇒ b c)"
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
    val c = ConjunctE(Seq(PropE("a", TP("A")), PropE("b", TP("B"))))
    val t = ConjunctE(Seq(
      ProjectE(0, c),
      ProjectE(1, c)
    ))
    t.informationLossScore._4 shouldEqual 0

    ConjunctE(Seq(
      ProjectE(1, c),
      ProjectE(1, c)
    )).informationLossScore._4 shouldEqual 1

    ConjunctE(Seq(
      ProjectE(1, c),
      ProjectE(0, c)
    )).informationLossScore._4 shouldEqual 2

    TermExpr.findFirst(t) {
      case ProjectE(_, _) ⇒ "abc"
    } shouldEqual Some("abc")
  }

  behavior of "TermExpr#simplify"

  it should "simplify identity function application" in {
    val termExpr0 = PropE("y", TP(1))
    val termExpr1 = CurriedE(List(PropE("x", TP(1) ->: TP(1))), termExpr0) // x: A -> x
    val termExpr2 = AppE(termExpr1, PropE("z", TP(1) ->: TP(1)))
    termExpr2.simplify() shouldEqual termExpr0 // (x: A -> y)(z) == y
  }

  it should "simplify nested terms" in {
    val f1 = CurriedE(List(PropE("x4", TP(2)), PropE("x5", TP(1))), PropE("x4", TP(2))) // f1: (x4:B -> x5:A -> x4:B)
    val x2 = PropE("x2", TP(1) ->: TP(2)) // x2: A → B
    val x3 = PropE("x3", TP(1)) // x3: A
    val t1 = AppE(f1, PropE("y", TP(2)))
    t1.simplify() shouldEqual CurriedE(List(PropE("x5", TP(1))), PropE("y", TP(2)))

    // x3:A -> (x2:A → B) -> (x4:B  -> x5:A -> x4:B) ( (x2:A → B)(x3:A) ) (x3:A)
    val termExpr4 = CurriedE(List(x3, x2), AppE(AppE(f1, AppE(x2, x3)), x3))
    termExpr4.simplify() shouldEqual CurriedE(List(x3, x2), AppE(x2, x3))
  }

  behavior of "Sequent#constructResultTerm"

  it should "produce result terms in correct order" in {
    val sequent = Sequent(List(TP(1), TP(2), TP(3)), TP(1), new FreshIdents("t"))
    val premiseVars = sequent.premiseVars
    premiseVars shouldEqual List(PropE("t1", TP(1)), PropE("t2", TP(2)), PropE("t3", TP(3)))
    val term = sequent.constructResultTerm(premiseVars.head)
    term shouldEqual CurriedE(List(PropE("t1", TP(1)), PropE("t2", TP(2)), PropE("t3", TP(3))), PropE("t1", TP(1)))
  }

  behavior of "reify terms API"

  it should "produce result terms" in {
    val terms1 = lambdaTerms[Int ⇒ Int]
    terms1.length shouldEqual 1
    terms1 shouldEqual Seq(CurriedE(List(PropE("a", BasicT("Int"))), PropE("a", BasicT("Int"))))

    val terms2 = lambdaTerms[Int ⇒ Int ⇒ Int]
    terms2.length shouldEqual 2

    val terms3 = lambdaTerms[Int ⇒ Int ⇒ (Int, Int)]
    terms3.length shouldEqual 2

    val terms4 = lambdaTerms[(Int, Int) ⇒ (Int, Int)]
    terms4.length shouldEqual 2

    def f : Unit = implement

    val t = lambdaTerms[Unit]

    t shouldEqual Seq(UnitE(UnitT("Unit")))

    def f0 = ofType[Unit]
    def f1 = allOfType[Unit]

    def f2[A] = allOfType[Either[A ⇒ A, Unit]].length shouldEqual 1
  }
}
