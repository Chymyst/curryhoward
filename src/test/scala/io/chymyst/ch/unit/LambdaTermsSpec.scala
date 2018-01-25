package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class LambdaTermsSpec extends FlatSpec with Matchers {

  behavior of "lambdaTerms API"

  it should "produce result terms" in {
    val terms1 = ofType[Int ⇒ Int]
    terms1.lambdaTerm shouldEqual CurriedE(List(VarE("a", BasicT("Int"))), VarE("a", BasicT("Int")))

    val terms2 = allOfType[Int ⇒ Int ⇒ Int]
    terms2.length shouldEqual 2
    terms2.map(_.lambdaTerm.prettyPrint) shouldEqual Seq("a ⇒ b ⇒ b", "a ⇒ b ⇒ a")

    val terms3 = allOfType[Int ⇒ Int ⇒ (Int, Int)]
    terms3.length shouldEqual 2
    terms3.map(_.lambdaTerm.prettyPrint) shouldEqual Seq("a ⇒ b ⇒ Tuple2(b, a)", "a ⇒ b ⇒ Tuple2(a, b)")

    val terms4 = allOfType[(Int, Int) ⇒ (Int, Int)]
    terms4.length shouldEqual 2
    terms4.map(_.lambdaTerm.prettyPrint) shouldEqual Seq("a ⇒ Tuple2(a._1, a._2)", "a ⇒ Tuple2(a._2, a._1)")

    val u: Unit = implement

    u shouldEqual (())

    val u0 = ofType[Unit]

    u0 shouldEqual (())

    val u0s = allOfType[Unit]

    u0s shouldEqual Seq(())

    def f2[A] = allOfType[Either[A ⇒ A, Unit]]

    f2.length shouldEqual 2

    // Check that we have no attached lambda-terms, since the result is not a function.

    // Obligatory type parameter here, or else lambdaTerm does not work.
    f2[Int].forall(TermExpr.lambdaTerm(_).isEmpty) shouldEqual true

    // Check that the returned terms are "(Left(a ⇒ a) + 0)", "(Right(1) + 0)".
    val Seq(f2a, f2b) = f2[String]

    f2a match {
      case Left(x) ⇒ x("abc") shouldEqual "abc"
    }

    f2b match {
      case Right(y) ⇒ y shouldEqual (())
    }

    def f3[A] = allOfType[(A, A) ⇒ A]

    f3.length shouldEqual 2
    f3[Int].map(_.lambdaTerm.prettyPrint) shouldEqual Seq("a ⇒ a._1", "a ⇒ a._2")
  }

  it should "produce result terms for functions of 2 and 3 arguments" in {
    val terms1 = allOfType[(Int, Int) ⇒ Int]
    terms1.length shouldEqual 2

    val f1 = allOfType[(Int, Int) ⇒ Int]
    f1.flatMap(TermExpr.lambdaTerm).length shouldEqual 2

    val terms2 = allOfType[(Int, Int, Int) ⇒ Int]
    terms2.length shouldEqual 3

    val f2 = allOfType[(Int, Int, Int) ⇒ Int]
    f2.flatMap(TermExpr.lambdaTerm).length shouldEqual 3
  }

  it should "produce no lambda-terms when `implement` is used" in {
    val terms1: Int ⇒ Int = implement

    TermExpr.lambdaTerm(terms1) shouldEqual None
  }

  it should "symbolically lambda-verify identity law for map on Reader monad" in {
    type R[X, A] = X ⇒ A

    def mapReader[X, A, B] = ofType[R[X, A] ⇒ (A ⇒ B) ⇒ R[X, B]]

    val mapReaderTerm = mapReader.lambdaTerm

    mapReaderTerm.prettyPrint shouldEqual "a ⇒ b ⇒ c ⇒ b (a c)"

    def idFunc[A] = ofType[A ⇒ A]

    idFunc.lambdaTerm.prettyPrint shouldEqual "a ⇒ a"

    val readerTerm = VarE("rxa", TP("X") ->: TP("A"))

    val appl1 = TermExpr.simplifyWithEtaUntilStable(AppE(AppE(TermExpr.substTypeVar(TP("B"), TP("A"), mapReaderTerm), readerTerm), idFunc.lambdaTerm))

    appl1 shouldEqual readerTerm
  }

  it should "symbolically lambda-verify composition law for map on Reader monad" in {
    type R[X, A] = X ⇒ A

    def fmapReader[X, A, B] = ofType[(A ⇒ B) ⇒ R[X, A] ⇒ R[X, B]]

    val fmapReaderTerm = TermExpr.lambdaTerm(fmapReader).get

    fmapReaderTerm.prettyPrint shouldEqual "a ⇒ b ⇒ c ⇒ a (b c)"

    val readerTerm = VarE("rxa", TP("X") ->: TP("A"))
    val aTerm = VarE("a", TP("A"))

    val f1Term = VarE("f1", TP("A") ->: TP("B"))
    val f2Term = VarE("f2", TP("B") ->: TP("C"))

    // fmap f1 . fmap f2 = fmap (f1 . f2)
    val fmapF1 = AppE(fmapReaderTerm, f1Term)
    val fmapAC = TermExpr.substTypeVar(TP("B"), TP("C"), fmapReaderTerm)
    val fmapF2 = AppE(TermExpr.substTypeVar(TP("A"), TP("B"), fmapAC), f2Term)
    val fmapf1f2rxa = AppE(AppE(fmapAC, CurriedE(List(aTerm), AppE(f2Term, AppE(f1Term, aTerm)))), readerTerm)

    val fmapF1fmapF2rxa = AppE(fmapF2, AppE(fmapF1, readerTerm))
    val appl1 = TermExpr.simplifyWithEtaUntilStable(fmapf1f2rxa)
    val appl2 = TermExpr.simplifyWithEtaUntilStable(fmapF1fmapF2rxa)
    appl1 shouldEqual appl2
  }

  it should "return lambda terms together with the function when using `ofType` but not when using `implement`" in {
    def f2[A] = ofType[Unit ⇒ Either[A ⇒ A, Unit]]

    TermExpr.lambdaTerm(f2).map(_.prettyPrint) shouldEqual Some("a ⇒ (0 + Right(a))")

    def f2a[A]: Unit ⇒ Either[A ⇒ A, Unit] = implement

    TermExpr.lambdaTerm(f2a) shouldEqual None
  }
}
