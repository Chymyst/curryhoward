package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{Assertion, FlatSpec, Matchers}

class LambdaTermsSpec extends FlatSpec with Matchers {

  behavior of ".lambdaTerm API"

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

  it should "verify identity law for Either[Int, T] as in tutorial" in {
    def fmap[A, B] = ofType[(A ⇒ B) ⇒ Either[Int, A] ⇒ Either[Int, B]]
    val fmapT = fmap.lambdaTerm
    def a[A] = freshVar[A]
    def b[B] = freshVar[B]
    val fmapAA = fmapT.substTypeVar(b, a)
    def optA[A] = freshVar[Either[Int, A]]
    fmapAA(a #> a)(optA) equiv optA shouldEqual true
  }

  it should "verify identity law for Option[T]" in {
    def fmap[A, B] = ofType[(A ⇒ B) ⇒ Option[A] ⇒ Option[B]]

    val fmapT = fmap.lambdaTerm // No need to specify type parameters.
    def a[A] = freshVar[A]

    val idA = a #> a

    val apply1 = idA(a)

    def b[B] = freshVar[B]

    the[Exception] thrownBy idA(b) should have message "Internal error: Invalid head type in application (\\((a$12:A) ⇒ a$12) b$13): `A ⇒ A` must be a function with argument type `B`"

    val fmapAA = fmapT.substTypeVar(b, a)

    val f2 = fmapAA(idA)

    the[Exception] thrownBy fmapT.substTypeVar(idA, a) should have message "substTypeVar requires a type variable as type of expression \\((a$12:A) ⇒ a$12), but found A ⇒ A"

    def optA[A] = freshVar[Option[A]]

    f2(optA).simplify shouldEqual optA
  }

  behavior of "lambda terms manipulation"

  final case class User(fullName: String, id: Long)

  it should "correctly handle conjunctions and disjunctions for tutorial" in {
    val ou = freshVar[Option[User]]
    val n = freshVar[None.type]
    val su = freshVar[Some[User]]
    val ol = freshVar[Option[Long]]
    val case1 = n #> ol(n())
    val sl = freshVar[Some[Long]]
    val case2 = su #> ol.tExpr(sl.tExpr(su(0)("id")))
    val getId = ou #> ou.cases(case1, case2)
    val dString = freshVar[String]
    var dLong = freshVar[Long]
    val u = freshVar[User]

    val uData = u(dString, dLong)
    uData("id").simplify shouldEqual dLong

    val data = ou(su(uData))
    val result1 = getId(data).simplify

    val expected = ol(sl(dLong))
    result1 shouldEqual expected

    val getIdAuto = ofType[Option[User] ⇒ Option[Long]]
    val getIdAutoTerm = getIdAuto.lambdaTerm
    getIdAutoTerm.prettyPrint shouldEqual getId.prettyPrint
    getIdAutoTerm equiv getId.prettyRename shouldEqual true
  }

  it should "generate errors when types do not match" in {
    val x = freshVar[None.type]

    x.tExpr.typeParams shouldEqual Seq()
    x.tExpr.conjunctSize shouldEqual 0
    x.tExpr.caseObjectName shouldEqual Some("None")

    the[Exception] thrownBy x(1) should have message ".apply(1) is undefined since this conjunction type has only 0 parts"
    the[Exception] thrownBy x("a") should have message ".apply(a) is undefined since this conjunction type does not support this accessor (supported accessors: )"
    the[Exception] thrownBy x(x) should have message ".apply() must be called with 0 arguments on this type None.type but it was called with 1 arguments"

    val noneT1 = x.tExpr()

    the[Exception] thrownBy x.tExpr(x) should have message ".apply() must be called with 0 arguments on this type None.type but it was called with 1 arguments"

    the[Exception] thrownBy noneT1.cases() should have message ".cases() is not defined for this term of type None.type"

    val u1 = freshVar[User]

    u1.tExpr.typeParams shouldEqual Seq()
    u1.tExpr.conjunctSize shouldEqual 2
    u1.tExpr.caseObjectName shouldEqual None

    the[Exception] thrownBy u1(x, x) should have message "Some arguments have unexpected types [None.type; None.type] that do not match the types in User"
    the[Exception] thrownBy u1() should have message ".apply() must be called with 2 arguments on this type User but it was called with 0 arguments"

    the[Exception] thrownBy u1.tExpr(x, x) should have message "Some arguments have unexpected types [None.type; None.type] that do not match the types in User"
    the[Exception] thrownBy u1.tExpr() should have message ".apply() must be called with 2 arguments on this type User but it was called with 0 arguments"

    the[Exception] thrownBy u1(100) should have message ".apply(100) is undefined since this conjunction type has only 2 parts"

    the[Exception] thrownBy u1("abc") should have message ".apply(abc) is undefined since this conjunction type does not support this accessor (supported accessors: fullName, id)"

    val e1 = freshVar[Either[String, Int]]
    e1.tExpr.conjunctSize shouldEqual 1
    e1.tExpr.caseObjectName shouldEqual None

    e1.tExpr.typeParams shouldEqual Seq(BasicT("String"), BasicT("Int"))

    the[Exception] thrownBy e1.accessor(1) should have message "Internal error: Cannot perform projection for term e1$25 : Either[<c>String,<c>Int]{Left[<c>String,<c>Int] + Right[<c>String,<c>Int]} because its type is not a conjunction"

    the[Exception] thrownBy e1() should have message "Calling .apply() on type Either[<c>String,<c>Int]{Left[<c>String,<c>Int] + Right[<c>String,<c>Int]} requires one argument (disjunction injection value)"

    the[Exception] thrownBy e1.tExpr() should have message "Calling .apply() on type Either[<c>String,<c>Int]{Left[<c>String,<c>Int] + Right[<c>String,<c>Int]} requires one argument (disjunction injection value)"

    the[Exception] thrownBy e1(x) should have message "Cannot inject into disjunction since the given disjunction type Either[<c>String,<c>Int]{Left[<c>String,<c>Int] + Right[<c>String,<c>Int]} does not contain the type None.type of the given term x$23"

    the[Exception] thrownBy e1.cases() should have message "Case match on Either[<c>String,<c>Int]{Left[<c>String,<c>Int] + Right[<c>String,<c>Int]} must use a sequence of 2 functions with matching types of arguments (Left[<c>String,<c>Int]; Right[<c>String,<c>Int]) and bodies, but have "

    val u0 = freshVar[Unit]
    val u00 = u0()
    val u000 = u0.tExpr()
    u00 shouldEqual u000

    the[Exception] thrownBy u0(u0) should have message "Calling .apply() on type Unit requires zero arguments (named unit value)"

    val f0 = freshVar[Int ⇒ Int]

    the[Exception] thrownBy f0(123) should have message ".apply(i: Int) is defined only on conjunction types while this is <c>Int ⇒ <c>Int"
    the[Exception] thrownBy f0("abc") should have message ".apply(acc: String) is defined only on conjunction types while this is <c>Int ⇒ <c>Int"
    the[Exception] thrownBy f0.tExpr() should have message "Cannot call .apply() on type <c>Int ⇒ <c>Int"

    val ct = ConjunctT(Seq(e1.tExpr, u0.tExpr))
    val cte = ct(e1, u0)
    the[Exception] thrownBy cte(e1) should have message ".apply() must be called with 2 arguments on this type (Either[<c>String,<c>Int]{Left[<c>String,<c>Int] + Right[<c>String,<c>Int]}, Unit) but it was called with 1 arguments"

    var i = freshVar[Int]

    the[Exception] thrownBy i() should have message "t.apply(...) is not defined for this term t=i$28 of type <c>Int"
  }

}
