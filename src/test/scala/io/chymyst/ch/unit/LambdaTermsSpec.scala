package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class LambdaTermsSpec extends FlatSpec with Matchers {

  behavior of "lambdaTerms API"

  it should "produce result terms" in {
    val terms1 = lambdaTerms[Int ⇒ Int]
    terms1.length shouldEqual 1
    terms1 shouldEqual Seq(CurriedE(List(PropE("a", BasicT("Int"))), PropE("a", BasicT("Int"))))

    val terms2 = lambdaTerms[Int ⇒ Int ⇒ Int]
    terms2.length shouldEqual 2
    terms2.map(_.prettyPrint) shouldEqual Seq("b ⇒ a ⇒ a", "a ⇒ b ⇒ a")

    val terms3 = lambdaTerms[Int ⇒ Int ⇒ (Int, Int)]
    terms3.length shouldEqual 2
    terms3.map(_.prettyPrint) shouldEqual Seq("b ⇒ a ⇒ Tuple2(a, b)", "a ⇒ b ⇒ Tuple2(a, b)")

    val terms4 = lambdaTerms[(Int, Int) ⇒ (Int, Int)]
    terms4.length shouldEqual 2
    terms4.map(_.prettyPrint) shouldEqual Seq("a ⇒ Tuple2(a._1, a._2)", "a ⇒ Tuple2(a._2, a._1)")

    val u: Unit = implement

    u shouldEqual (())

    val ut = lambdaTerms[Unit]
    ut shouldEqual Seq(UnitE(UnitT("Unit")))

    val u0 = ofType[Unit]

    u0 shouldEqual (())

    val u0s = allOfType[Unit]

    u0s shouldEqual Seq(())

    def f2[A] = lambdaTerms[Either[A ⇒ A, Unit]]

    f2.length shouldEqual 2
    f2.map(_.prettyPrint) shouldEqual Seq("(Left(a ⇒ a) + 0)", "(Right(1) + 0)")

    def f3[A] = lambdaTerms[(A, A) ⇒ A]

    f3.length shouldEqual 2
    f3.map(_.prettyPrint) shouldEqual Seq("a ⇒ a._1", "a ⇒ a._2")
  }

  it should "symbolically lambda-verify identity law for map on Reader monad" in {
    type R[X, A] = X ⇒ A

    def mapReaderTerms[X, A, B] = lambdaTerms[R[X, A] ⇒ (A ⇒ B) ⇒ R[X, B]]

    mapReaderTerms.length shouldEqual 1

    val mapReaderTerm = mapReaderTerms.head

    mapReaderTerm.prettyPrint shouldEqual "b ⇒ a ⇒ c ⇒ a (b c)"

    def identityTerm[A] = lambdaTerms[A ⇒ A].head

    identityTerm.prettyPrint shouldEqual "a ⇒ a"

    val readerTerm = PropE("rxa", TP("X") ->: TP("A"))

    val appl1 = TermExpr.simplifyWithEtaUntilStable(AppE(AppE(TermExpr.substTypeVar(TP("B"), TP("A"), mapReaderTerm), readerTerm), identityTerm))

    appl1 shouldEqual readerTerm
  }

  it should "return lambda terms together with the function" in {
    def f2[A]: Unit ⇒ Either[A ⇒ A, Unit] = implement

    TermExpr.lambdaTerm(f2).map(_.prettyPrint) shouldEqual Some("a ⇒ (0 + Right(a))")
  }
}
