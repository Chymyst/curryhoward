package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{Assertion, FlatSpec, Matchers}

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

    val idTermA = idFunc.lambdaTerm
    idTermA.prettyPrint shouldEqual "a ⇒ a"

    def readerTerm[X, A] = freshVar[X ⇒ A]

    val mapReaderAA = TermExpr.substTypeVar(TP("B"), TP("A"), mapReaderTerm)

    // map(rxa)(id) = rxa
    mapReaderAA(readerTerm)(idTermA).equiv(readerTerm) shouldEqual true
  }

  it should "symbolically lambda-verify composition law for fmap on Reader monad" in {
    type R[X, A] = X ⇒ A

    def check[X, A, B, C](): Assertion = {
      val fmap = ofType[(A ⇒ B) ⇒ R[X, A] ⇒ R[X, B]]

      val fmapTerm = TermExpr.lambdaTerm(fmap).get

      fmapTerm.prettyPrint shouldEqual "a ⇒ b ⇒ c ⇒ a (b c)"

      val readerTerm = freshVar[X ⇒ A]
      val aTerm = freshVar[A]
      val f1Term = freshVar[A ⇒ B]
      val f2Term = freshVar[B ⇒ C]

      // fmap f1 . fmap f2 = fmap (f1 . f2)
      val fmapF1 = fmapTerm(f1Term)
      val fmapAC = TermExpr.substTypeVar(TP("B"), TP("C"), fmapTerm)

      val fmapf1f2rxa = fmapAC(aTerm #> f2Term(f1Term(aTerm)))(readerTerm)

      val fmapBC = TermExpr.substTypeVar(TP("A"), TP("B"), fmapAC)
      val fmapF2 = fmapBC(f2Term)

      val fmapF1fmapF2rxa = fmapF2(fmapF1(readerTerm))
      fmapf1f2rxa.equiv(fmapF1fmapF2rxa) shouldEqual true
    }

    check()
  }

  it should "symbolically lambda-verify associativity law for (A ⇒ A) composition monoid and choose correct implementations" in {
    type Mon[A] = A ⇒ A

    def check[A, B, C](): Assertion = {
      val adds = allOfType[Mon[A] ⇒ Mon[A] ⇒ Mon[A]]

      adds.map(_.lambdaTerm.prettyPrint) shouldEqual Seq("a ⇒ b ⇒ c ⇒ a (b c)", "a ⇒ b ⇒ c ⇒ b (a c)")

      // Associativity law: add x (add y z) == add (add x y) z

      val x = freshVar[Mon[A]]
      val y = freshVar[Mon[A]]
      val z = freshVar[Mon[A]]

      val goodImplementations = adds.filter { add ⇒
        val addT = add.lambdaTerm
        addT(x)(addT(y)(z)) equiv addT(addT(x)(y))(z)
      }

      goodImplementations.length shouldEqual 2
    }

    check()
  }

  it should "return lambda terms together with the function when using `ofType` but not when using `implement`" in {
    def f2[A] = ofType[Unit ⇒ Either[A ⇒ A, Unit]]

    TermExpr.lambdaTerm(f2).map(_.prettyPrint) shouldEqual Some("a ⇒ (0 + Right(a))")

    def f2a[A]: Unit ⇒ Either[A ⇒ A, Unit] = implement

    TermExpr.lambdaTerm(f2a) shouldEqual None
  }

  it should "support filter syntax" in {

    final case class C[A](d: Option[(A, A)]) {
      // greedy filter on the product
      def map[B](f: A ⇒ B): C[B] = implement

      def withFilter(p: A ⇒ Boolean): C[A] = C(d filter {
        case (x, y) ⇒ p(x) && p(y)
      })
    }

    val c = C(Some((123, 456)))
    val d: Int ⇒ C[Int] = limit ⇒ for {
      x ← c
      y = x * 2
      if y > limit
    } yield y

    c.map(x ⇒ x * 2) shouldEqual C(Some((246, 912)))
    d(200) shouldEqual C(None)
    d(500) shouldEqual C(Some((246, 912)))
  }
}
