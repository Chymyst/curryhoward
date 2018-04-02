package io.chymyst.ch.unit

import io.chymyst.ch._
import io.chymyst.ch.data.{LawChecking => LC}
import org.scalatest.{Assertion, FlatSpec, Matchers}

class LambdaTermsSpec extends FlatSpec with Matchers {

  behavior of ".lambdaTerm API"

  it should "produce result terms" in {
    val terms1 = ofType[Int ⇒ Int]
    terms1.lambdaTerm shouldEqual CurriedE(List(VarE("a", BasicT("Int"))), VarE("a", BasicT("Int")))

    val terms2 = allOfType[Int ⇒ Int ⇒ Int]
    terms2.length shouldEqual 2
    terms2.map(_.lambdaTerm.prettyRenamePrint) shouldEqual Seq("a ⇒ b ⇒ b", "a ⇒ b ⇒ a")

    val terms3 = allOfType[Int ⇒ Int ⇒ (Int, Int)]
    terms3.length shouldEqual 2
    terms3.map(_.lambdaTerm.prettyRenamePrint) shouldEqual Seq("a ⇒ b ⇒ Tuple2(b, a)", "a ⇒ b ⇒ Tuple2(a, b)")

    val terms4 = allOfType[(Int, Int) ⇒ (Int, Int)]
    terms4.length shouldEqual 2
    terms4.map(_.lambdaTerm.prettyRenamePrint) shouldEqual Seq("a ⇒ Tuple2(a._1, a._2)", "a ⇒ Tuple2(a._2, a._1)")

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
    val Seq(f2a: Either[String ⇒ String, Unit], f2b: Either[String ⇒ String, Unit]) = f2[String]

    f2a match {
      case Left(x) ⇒ x("abc") shouldEqual "abc"
      //      case Right(y) ⇒ y shouldEqual (())
    }

    f2b match {
      case Right(y) ⇒ y shouldEqual (())
      //      case Left(x) ⇒ x("abc") shouldEqual "abc"
    }

    def f3[A] = allOfType[(A, A) ⇒ A]

    f3.length shouldEqual 2
    f3[Int].map(_.lambdaTerm.prettyRenamePrint) shouldEqual Seq("a ⇒ a._1", "a ⇒ a._2")
  }

  it should "produce result terms for functions of 2 and 3 arguments" in {
    val f2 = allOfType[(Int, Int) ⇒ Int]
    f2.length shouldEqual 2
    f2.flatMap(TermExpr.lambdaTerm).length shouldEqual 2
    f2.map(_.lambdaTerm).length shouldEqual 2

    val f3 = allOfType[(Int, Int, Int) ⇒ Int]
    f3.length shouldEqual 3
    f3.flatMap(TermExpr.lambdaTerm).length shouldEqual 3
    f3.map((x: Any) ⇒ x.lambdaTerm).length shouldEqual 3
  }

  it should "produce lambda-terms when `implement` is used" in {

    "val f0: () ⇒ Int = implement" shouldNot compile
    //    TermExpr.lambdaTerm(f0).nonEmpty shouldEqual true
    //    f0.lambdaTerm.prettyPrint shouldEqual ""

    val f1: Int ⇒ Int = implement
    TermExpr.lambdaTerm(f1).nonEmpty shouldEqual true
    f1.lambdaTerm.prettyPrint shouldEqual "a ⇒ a"

    the[Exception] thrownBy None.lambdaTerm should have message "Called `.lambdaTerm` on an expression None that has no attached lambda-term"

    val f2: (Int, String) ⇒ Int = implement
    TermExpr.lambdaTerm(f2).nonEmpty shouldEqual true
    f2.lambdaTerm.prettyPrint shouldEqual "a ⇒ a._1"

    // We do not support functions with many arguments.
    val f3: (Int, String, String, String, String, String, String, String, String, String) ⇒ Int = implement
    TermExpr.lambdaTerm(f3) shouldEqual None
    the[Exception] thrownBy f3.lambdaTerm should have message "Called `.lambdaTerm` on an expression <function10> that has no attached lambda-term"
  }

  it should "symbolically lambda-verify identity law for map on Reader monad" in {
    type R[X, A] = X ⇒ A

    def mapReader[X, A, B] = ofType[R[X, A] ⇒ (A ⇒ B) ⇒ R[X, B]]

    val mapReaderTerm = mapReader.lambdaTerm

    mapReaderTerm.prettyRenamePrint shouldEqual "a ⇒ b ⇒ c ⇒ b (a c)"

    def idFunc[A] = ofType[A ⇒ A]

    val idTermA = idFunc.lambdaTerm
    idTermA.prettyRenamePrint shouldEqual "a ⇒ a"

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

      fmapTerm.prettyRenamePrint shouldEqual "a ⇒ b ⇒ c ⇒ a (b c)"

      val readerTerm = freshVar[X ⇒ A]
      val aTerm = freshVar[A]
      val f1Term = freshVar[A ⇒ B]
      val f2Term = freshVar[B ⇒ C]

      // This syntax should compile.
      val aa = aTerm =>: aTerm =>: aTerm
      aa.isInstanceOf[TermExpr]

      // fmap f1 . fmap f2 = fmap (f1 . f2)
      val fmapF1 = fmapTerm(f1Term)
      val fmapAC = TermExpr.substTypeVar(TP("B"), TP("C"), fmapTerm)

      val fmapf1f2rxa = fmapAC(aTerm =>: f2Term(f1Term(aTerm)))(readerTerm)

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

      adds.map(_.lambdaTerm.prettyRenamePrint) shouldEqual Seq("a ⇒ b ⇒ c ⇒ a (b c)", "a ⇒ b ⇒ c ⇒ b (a c)")

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

  it should "return lambda terms together with the function when using `ofType` or `implement`" in {
    def f2[A] = anyOfType[Unit ⇒ Either[A ⇒ A, Unit]]().map(_.lambdaTerm)

    f2.map(_.prettyRenamePrint) shouldEqual Seq("a ⇒ (Left(b ⇒ b) + 0)", "a ⇒ (0 + Right(1))")

    def f2a[A]: (A ⇒ A) ⇒ Either[A ⇒ A, Unit] = implement

    TermExpr.lambdaTerm(f2a).map(_.prettyRenamePrint) shouldEqual Some("a ⇒ (Left(a) + 0)")
  }

  it should "verify identity law for Either[Int, T] as in tutorial" in {
    def fmap[A, B] = ofType[(A ⇒ B) ⇒ Either[Int, A] ⇒ Either[Int, B]]

    val fmapT = fmap.lambdaTerm

    def a[A] = freshVar[A]

    def b[B] = freshVar[B]

    val fmapAA = fmapT.substTypeVar(b, a)

    def optA[A] = freshVar[Either[Int, A]]

    fmapAA(a =>: a)(optA) equiv optA shouldEqual true
  }

  it should "verify identity law for Option[T]" in {
    def fmap[A, B] = ofType[(A ⇒ B) ⇒ Option[A] ⇒ Option[B]]

    val fmapT = fmap.lambdaTerm // No need to specify type parameters.
    def a[A] = freshVar[A]

    val idA = a =>: a

    def b[B] = freshVar[B]

    the[Exception] thrownBy idA(b) should have message "Internal error: Invalid head type in application (\\((a$12:A) ⇒ a$12) b$13): A ⇒ A must be a function with argument type B"

    val fmapAA = fmapT.substTypeVar(b, a)

    val f2 = fmapAA(idA)

    the[Exception] thrownBy fmapT.substTypeVar(idA, a) should have message "substTypeVar requires a type variable as type of expression \\((a$12:A) ⇒ a$12), but found type A ⇒ A"

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
    val case1 = n =>: ol(n())
    val sl = freshVar[Some[Long]]
    val case2 = su =>: ol.t(sl.t(su(0)("id")))
    val getId = ou =>: ou.cases(case1, case2)
    val dString = freshVar[String]
    val dLong = freshVar[Long]
    val u = freshVar[User]

    val uData = u(dString, dLong)
    uData("id").simplify shouldEqual dLong

    val data = ou(su(uData))
    val result1 = getId(data).simplify

    val expected = ol(sl(dLong))
    result1 shouldEqual expected

    val getIdAuto = ofType[Option[User] ⇒ Option[Long]]
    val getIdAutoTerm = getIdAuto.lambdaTerm
    getIdAutoTerm.prettyRenamePrint shouldEqual getId.simplify.prettyRenamePrint
    getIdAutoTerm equiv getId.prettyRename shouldEqual true
  }

  it should "generate errors when types do not match" in {
    val x = freshVar[None.type]

    x.t.typeParams shouldEqual Seq()
    x.t.conjunctSize shouldEqual 0
    x.t.caseObjectName shouldEqual Some("None")

    the[Exception] thrownBy x(1) should have message ".apply(1) is undefined since this conjunction type has only 0 parts"
    the[Exception] thrownBy x("a") should have message ".apply(a) is undefined since this conjunction type does not support this accessor (supported accessors: )"
    the[Exception] thrownBy x(x) should have message ".apply() must be called with 0 arguments on this type None.type but it was called with 1 arguments"

    val noneT1 = x.t()

    the[Exception] thrownBy x.t(x) should have message ".apply() must be called with 0 arguments on this type None.type but it was called with 1 arguments"

    the[Exception] thrownBy noneT1.cases() should have message ".cases() is not defined for the term <co>None() of type None.type"

    val u1 = freshVar[User]

    u1.t.typeParams shouldEqual Seq()
    u1.t.conjunctSize shouldEqual 2
    u1.t.caseObjectName shouldEqual None

    the[Exception] thrownBy u1(x, x) should have message "Some arguments of .apply() have unexpected types [None.type; None.type] that do not match the types in User"
    the[Exception] thrownBy u1() should have message ".apply() must be called with 2 arguments on this type User but it was called with 0 arguments"

    the[Exception] thrownBy u1.t(x, x) should have message "Some arguments of .apply() have unexpected types [None.type; None.type] that do not match the types in User"
    the[Exception] thrownBy u1.t() should have message ".apply() must be called with 2 arguments on this type User but it was called with 0 arguments"

    the[Exception] thrownBy u1(100) should have message ".apply(100) is undefined since this conjunction type has only 2 parts"

    the[Exception] thrownBy u1("abc") should have message ".apply(abc) is undefined since this conjunction type does not support this accessor (supported accessors: fullName, id)"

    val e1 = freshVar[Either[String, Int]]
    e1.t.conjunctSize shouldEqual 1
    e1.t.caseObjectName shouldEqual None

    e1.t.typeParams shouldEqual Seq(BasicT("String"), BasicT("Int"))

    the[Exception] thrownBy e1.accessor(1) should have message "Internal error: Cannot perform projection for term e1$25 : Either[<c>String,<c>Int] because its type is not a conjunction"

    the[Exception] thrownBy e1() should have message "Calling .apply() on type Either[<c>String,<c>Int] requires one argument (disjunction injection value)"

    the[Exception] thrownBy e1.t() should have message "Calling .apply() on type Either[<c>String,<c>Int] requires one argument (disjunction injection value)"

    the[Exception] thrownBy e1(x) should have message "Cannot inject into disjunction since the given disjunction type Either[<c>String,<c>Int] does not contain the type None.type of the given term x$23"

    the[Exception] thrownBy e1.cases() should have message "Case match on Either[<c>String,<c>Int] must use a sequence of 2 functions with matching types of arguments (Left[<c>String,<c>Int]; Right[<c>String,<c>Int]) and bodies, but have "

    val u0 = freshVar[Unit]
    val u00 = u0()
    val u000 = u0.t()
    u00 shouldEqual u000

    the[Exception] thrownBy u0(u0) should have message "Calling .apply() on type Unit requires zero arguments (named unit value)"

    val f0 = freshVar[Int ⇒ Int]

    the[Exception] thrownBy f0(123) should have message ".apply(index: Int) is defined only on conjunction types while this is <c>Int ⇒ <c>Int"
    the[Exception] thrownBy f0("abc") should have message ".apply(accessor: String) is defined only on conjunction types while this is <c>Int ⇒ <c>Int"
    the[Exception] thrownBy f0.t() should have message "Cannot call .apply() on type <c>Int ⇒ <c>Int"

    val ct = ConjunctT(Seq(e1.t, u0.t))
    val cte = ct(e1, u0)
    the[Exception] thrownBy cte(e1) should have message ".apply() must be called with 2 arguments on this type (Either[<c>String,<c>Int], Unit) but it was called with 1 arguments"

    val i = freshVar[Int]

    the[Exception] thrownBy i() should have message "t.apply(...) is not defined for the term i$28 of type <c>Int"

    (f0 andThen f0).t shouldEqual f0.t

    the[Exception] thrownBy (f0 andThen u0 =>: u0) should have message "Call to `.andThen` is invalid because the function types (<c>Int ⇒ <c>Int and Unit ⇒ Unit) do not match"

    the[Exception] thrownBy (f0 andThen u0) should have message "Call to `.andThen` is invalid because the type of one of the arguments (<c>Int ⇒ <c>Int and Unit) is not of a function type"
  }

  behavior of "automatic alpha-conversion"

  it should "substitute type variables and verify identity law for Reader monad" in {
    type R[X, A] = X ⇒ A

    def mapReader[X, A, B] = ofType[R[X, A] ⇒ (A ⇒ B) ⇒ R[X, B]]

    val mapReaderTerm = mapReader.lambdaTerm

    def idFunc[A] = ofType[A ⇒ A]

    val idTermA = idFunc.lambdaTerm

    def readerTerm[X, A] = freshVar[X ⇒ A]

    val mapApplied = mapReaderTerm :@ readerTerm

    mapApplied.toString shouldEqual "(\\((a:X ⇒ A) ⇒ (b:A ⇒ B) ⇒ (c:X) ⇒ (b (a c))) readerTerm$29)"
    (mapApplied.t.prettyPrint, idTermA.t.prettyPrint) shouldEqual (("(A ⇒ B) ⇒ X ⇒ B", "A ⇒ A"))

    // map(rxa)(id) = rxa
    mapReaderTerm :@ readerTerm :@ idTermA equiv readerTerm shouldEqual true
  }

  it should "substitute type variables and verify composition law for fmap on Reader monad" in {
    type R[X, A] = X ⇒ A

    def check[X, A, B, C](): Assertion = {
      val fmap = ofType[(A ⇒ B) ⇒ R[X, A] ⇒ R[X, B]]

      val fmapTerm = TermExpr.lambdaTerm(fmap).get

      fmapTerm.prettyRenamePrint shouldEqual "a ⇒ b ⇒ c ⇒ a (b c)"

      val reader = freshVar[X ⇒ A]

      val fAB = freshVar[A ⇒ B]
      val fBC = freshVar[B ⇒ C]

      // fmap fAB . fmap fBC = fmap (fAB . fBC) as functions on a `reader`
      (fmapTerm :@ fAB andThen (fmapTerm :@ fBC)) (reader) equiv (fmapTerm :@ (fAB andThen fBC)) (reader) shouldEqual true
    }

    check()
  }

  it should "compute correct substitutions for unification" in {
    def vA[A] = freshVar[A]

    val vInt = freshVar[Int]
    val vFloat = freshVar[Float]

    def vB[B] = freshVar[B]

    TypeExpr.leftUnify((vA =>: vB).t, (vInt =>: vFloat).t, (vA =>: vB).t) shouldEqual Right(Map(vA.t → vInt.t, vB.t → vFloat.t))

    def vC[C] = freshVar[C]

    TypeExpr.leftUnify((vA =>: vB).t, (vB =>: vC).t, (vA =>: vB).t) shouldEqual Right(Map(vA.t → vB.t, vB.t → vC.t))

    TypeExpr.leftUnify(ConjunctT(Seq(vA.t, vB.t)), ConjunctT(Seq(vInt.t, vFloat.t)), ConjunctT(Seq(vA.t, vB.t))) shouldEqual Right(Map(vA.t → vInt.t, vB.t → vFloat.t))

    def vX[A, B] = freshVar[(A, Option[(B, B)])]

    val vX1 = freshVar[(Int, Option[(Float, Float)])]
    TypeExpr.leftUnify(vX.t, vX1.t, vX.t) shouldEqual Right(Map(vA.t → vInt.t, vB.t → vFloat.t))

    val vX2 = freshVar[(Int, Option[(Float, Int)])]
    TypeExpr.leftUnify(vX.t, vX2.t, vX.t) shouldEqual Left("Cannot unify B with <c>Int because type parameter B requires incompatible substitutions <c>Float and <c>Int")

    def vX3[A, B] = freshVar[(A, Option[(A, B)])]

    TypeExpr.leftUnify(vX.t, vX3.t, vX.t) shouldEqual Left("Cannot unify B with B because type parameter B requires incompatible substitutions A and B")

    def vY[A, B] = freshVar[(A, Unit, Option[(B, B)])]

    val vY1 = freshVar[(Int, Unit, Option[(Float, Float)])]
    TypeExpr.leftUnify(vY.t, vY1.t, vY.t) shouldEqual Right(Map(vA.t → vInt.t, vB.t → vFloat.t))

    TypeExpr.leftUnify((vA =>: vB).t, (vB =>: vA).t, (vA =>: vB).t) shouldEqual Right(Map(vA.t → vB.t, vB.t → vA.t))
  }

  behavior of "other functionality"

  it should "produce error message when Java-style argument group is mismatched" in {
    val f = freshVar[(Int, String) ⇒ Int]
    val vInt = freshVar[Int]
    val vString = freshVar[String]

    f(vInt, vString).t shouldEqual vInt.t

    the[Exception] thrownBy f(vInt, vInt) should have message s"Internal error: Invalid head type in application (${f.name} (${vInt.name}, ${vInt.name})): (<c>Int, <c>String) ⇒ <c>Int must be a function with argument type (<c>Int, <c>Int)"
    the[Exception] thrownBy f(vInt, vInt, vInt) should have message s"Internal error: Invalid head type in application (${f.name} (${vInt.name}, ${vInt.name}, ${vInt.name})): (<c>Int, <c>String) ⇒ <c>Int must be a function with argument type (<c>Int, <c>Int, <c>Int)"
  }

  it should "produce error message when applying ConjunctE to wrong types" in {
    val vInt = freshVar[Int]
    val vString = freshVar[String]
    val x = ConjunctE(Seq(vInt, vString))

    x(vInt, vString).t shouldEqual x.t

    the[Exception] thrownBy x(vInt, vInt) should have message "Some arguments of .apply() have unexpected types [<c>Int] that do not match the types in (<c>Int, <c>String)"
    the[Exception] thrownBy x(vInt, vInt, vInt) should have message ".apply() must be called with 2 arguments on this type (<c>Int, <c>String) but it was called with 3 arguments"
  }

  it should "use unit types in functions" in {
    val vUnit = freshVar[Unit]

    def vF[A] = freshVar[Unit ⇒ A]

    def vA[A] = freshVar[A]

    vF(vUnit).t shouldEqual vA.t
    (vF :@ vUnit).t shouldEqual vA.t
  }

  it should "unify disjunction and conjunction types containing basic types" in {
    def vF[A, B] = freshVar[(A, Unit, Option[(B, B)]) ⇒ A]

    val vUnit = freshVar[Unit]

    def vA[A] = freshVar[A]

    val vInt = freshVar[Int]
    val vFloatFloat = freshVar[Option[(Float, Float)]]
    val vFloatInt = freshVar[Option[(Float, Int)]]

    (vF :@ (vInt, vUnit, vFloatFloat)).t shouldEqual vInt.t

    the[Exception] thrownBy (vF :@ (vInt, vUnit, vFloatInt)).t should have message "Cannot unify B with <c>Int because type parameter B requires incompatible substitutions <c>Float and <c>Int"
    the[Exception] thrownBy (vF :@ vFloatFloat).t should have message "Cannot unify (A, Unit, Option[Tuple2[B,B]]) with an incompatible type Option[Tuple2[<c>Float,<c>Float]]"
    (vF :@ (vA =>: vFloatInt, vUnit, vFloatFloat)).t.prettyPrintVerbose shouldEqual "A ⇒ Option[Tuple2[<c>Float,<c>Int]]{None.type + Some[Tuple2[<c>Float,<c>Int]]}"
  }

  it should "have correct types for Option[Option[Int]]" in {
    val fs = allOfType[Option[Option[Int]] ⇒ Option[Option[Int]]].map(_.lambdaTerm)
    fs.length shouldEqual 1

    val f1 = fs(0)
    val ooi = freshVar[Option[Option[Int]]]
    val oi = freshVar[Option[Int]]
    val n = freshVar[None.type]
    val si = freshVar[Some[Option[Int]]]


    val nonE = oi(n())
    val someNone = ooi(si(nonE))
    println(someNone.t)
    println(someNone.t.prettyPrint)

    f1(ooi(n())).simplify shouldEqual ooi(n())
    f1(someNone).simplify shouldEqual someNone
  }

  it should "produce a type expression with empty-arg case class" in {

    final case class Empty()

    val vE = freshVar[Empty]
    val vI = freshVar[Int]
    val vU = freshVar[Unit]

    val u = vU()

    val tExpr = (vE =>: vI =>: u).t

    tExpr.prettyPrint shouldEqual "Empty ⇒ <c>Int ⇒ Unit"

    val (proofs, _) = TheoremProver.findProofs(tExpr)

    proofs.length shouldEqual 1

    val f: Empty ⇒ Int ⇒ Unit = implement

    f(Empty())(123) shouldEqual (())
  }

  it should "simplify tuple application" in {
    val vT = freshVar[(Int, Int)]
    val tTuple = vT.t
    val f1 = vT =>: tTuple(vT(0), vT(1))
    f1.simplify.prettyRename.prettyPrint shouldEqual "a ⇒ a" // not "a ⇒ Tuple2(a._1, a._2)"
  }

  it should "verify naturality for pure / Either as in tutorial" in {
    def fmapT[A, B] = ofType[(A ⇒ B) ⇒ Either[Int, A] ⇒ Either[Int, B]].lambdaTerm

    def pure[A] = ofType[A ⇒ Either[Int, A]].lambdaTerm

    def f[A, B] = freshVar[A ⇒ B]

    val leftSide = f @@: pure
    leftSide.t.prettyPrint shouldEqual "A ⇒ Either[<c>Int,B]"

    val fmapF = fmapT :@ f
    fmapF.t.prettyPrint shouldEqual "Either[<c>Int,A] ⇒ Either[<c>Int,B]"

    val rightSide = pure :@@ (fmapT :@ f)
    leftSide equiv rightSide shouldEqual true
    leftSide.simplify.prettyRename.toString shouldEqual rightSide.simplify.prettyRename.toString
  }

  it should "auto-rename unused type variables in case of a name clash" in {
    def f[A, B] = freshVar[(A, B) ⇒ B]

    TypeExpr.allTypeParams(f.t) shouldEqual Set(TP("A"), TP("B"))

    def pure[A] = ofType[A ⇒ (A, A)].lambdaTerm

    val composition = f :@@ pure
    composition.t.prettyPrint should fullyMatch regex "\\(Z[0-9]+, A\\) ⇒ Tuple2\\[A,A\\]"
  }

  it should "perform alpha-conversions in match clauses" in {
    val t = ofType[Option[Option[Int]] ⇒ Option[Int]].lambdaTerm
    t.prettyPrint shouldEqual "a ⇒ a match { b ⇒ (None() + 0); c ⇒ c.value }"

    type P[A] = Either[A, Int ⇒ A]

    def ftn[A] = anyOfType[P[P[A]] ⇒ P[A]]().map(_.lambdaTerm)

    def px[X] = freshVar[P[X]]

    ftn.map(_.prettyPrint) shouldEqual Seq(
      "a ⇒ a match { b ⇒ b.value; c ⇒ (0 + Right(d ⇒ c.value d match { e ⇒ e.value; f ⇒ f.value d })) }",
      "a ⇒ a match { b ⇒ b.value match { c ⇒ (0 + Right(d ⇒ c.value)); e ⇒ (0 + e) }; f ⇒ (0 + Right(g ⇒ f.value g match { h ⇒ h.value; i ⇒ i.value g })) }"
    )

    def fmapTerm[A, B] = ofType[(A ⇒ B) ⇒ P[A] ⇒ P[B]].lambdaTerm

    def flmTerms[A, B] = anyOfType[(A ⇒ P[B]) ⇒ P[A] ⇒ P[B]]().map(_.lambdaTerm)

    val ftnTerms = flmTerms.map(flm ⇒ (flm :@ (px =>: px)).simplify)

    ftnTerms.map(_.prettyPrint) shouldEqual Seq(
      "b ⇒ b match { c ⇒ c.value; d ⇒ (0 + Right(e ⇒ d.value e match { f ⇒ f.value; g ⇒ g.value e })) }",
      "b ⇒ b match { c ⇒ (0 + Right(d ⇒ c.value match { e ⇒ e.value; f ⇒ f.value d })); g ⇒ (0 + Right(h ⇒ g.value h match { i ⇒ i.value; j ⇒ j.value h })) }",
      "b ⇒ b match { c ⇒ c.value match { d ⇒ (0 + Right(e ⇒ d.value)); f ⇒ (0 + f) }; g ⇒ (0 + Right(h ⇒ g.value h match { i ⇒ i.value; j ⇒ j.value h })) }"
    )

    val flatten = ftnTerms.head

    val lhs = (flatten :@@ flatten).simplify
    val expected = " match { c ⇒ c.value match { c ⇒ c.value; d ⇒ (0 + Right(e ⇒ d.value e match { f ⇒ f.value; g ⇒ g.value e })) }; d ⇒ (0 + Right(e ⇒ d.value e match { f ⇒ f.value match { f ⇒ f.value; g ⇒ g.value e }; g ⇒ g.value e match { f ⇒ f.value; g ⇒ g.value e } })) }"
    lhs.prettyPrint should endWith(expected)

    val fmapFlatten = (fmapTerm :@ flatten).simplify
    val rhs = (fmapFlatten :@@ flatten).simplify
    rhs.prettyPrint should endWith (" match { c ⇒ c.value match { c ⇒ c.value; d ⇒ (0 + Right(e ⇒ d.value e match { f ⇒ f.value; g ⇒ g.value e })) }; d ⇒ (0 + Right(e ⇒ d.value e match { c ⇒ c.value match { f ⇒ f.value; g ⇒ g.value e }; d ⇒ d.value e match { f ⇒ f.value; g ⇒ g.value e } })) }")
    // TODO: should be able to rename to `expected`!
  }

  behavior of "discovering monads"

  def semimonadsAndMonads(fmapTerm: TermExpr, pureVar: TermExpr, flmVar: TermExpr, debug: Boolean = false): (Seq[TermExpr], Seq[(TermExpr, TermExpr)]) = {
    val px = VarE("px", pureVar.t.asInstanceOf[#->].body)

    val initTime = System.currentTimeMillis()
    val flmTerms = TheoremProver.findProofs(flmVar.t)._2
    val elapsed = System.currentTimeMillis() - initTime

    // Compute flatten terms from flm terms
    val ftnTerms = flmTerms.map(flm ⇒ (flm :@ (px =>: px)).simplify)
    if (debug) println(s"flatten terms: ${ftnTerms.map(_.prettyPrint)}")

    val pureTerms = TheoremProver.findProofs(pureVar.t)._2
    if (debug) println(s"pure terms: ${pureTerms.map(_.prettyPrint)}")

    println(s"Computed ${flmTerms.size} flm terms in $elapsed ms, and ${pureTerms.size} pure terms")

    val goodSemimonads: Seq[TermExpr] = ftnTerms.filter(LC.checkFlattenAssociativity(fmapTerm, _))

    println(s"Good semimonads: flatten is one of ${goodSemimonads.map(_.prettyPrint)}")

    val goodMonads: Seq[(TermExpr, TermExpr)] = for {
      ftn ← goodSemimonads
      pure ← pureTerms
      if LC.checkPureFlattenLaws(fmapTerm, pure, ftn)
    } yield (pure, ftn)

    (goodSemimonads, goodMonads)
  }

  it should "verify monad laws for Either" in {
    type P[A] = Either[Int, A]

    def fmapTerm[A, B] = ofType[(A ⇒ B) ⇒ P[A] ⇒ P[B]].lambdaTerm

    def flm[A, B] = freshVar[(A ⇒ P[B]) ⇒ P[A] ⇒ P[B]]

    def pure[A] = freshVar[A ⇒ P[A]]

    val (goodSemimonads, goodMonads) = semimonadsAndMonads(fmapTerm, pure, flm)

    println(s"Good semimonads: flatten is one of ${goodSemimonads.map(_.prettyPrint)}")

    println("Good monads:")
    println(goodMonads.map { case (pure, ftn) ⇒ s"pure = ${pure.prettyPrint}, flatten = ${ftn.prettyPrint}" })

    goodSemimonads.size shouldEqual 1
    goodMonads.size shouldEqual 1
  }

  //Either[A, Int ⇒ A]//Option[(A, A)]// Either[Int, (A,A)] // Either[A, A] // Either[A, (A, A)]

  it should "check Id + Reader monad" in {
    type P[A] = Either[A, Int ⇒ A]

    def fmapTerm[A, B] = ofType[(A ⇒ B) ⇒ P[A] ⇒ P[B]].lambdaTerm

    def flm[A, B] = freshVar[(A ⇒ P[B]) ⇒ P[A] ⇒ P[B]]

    def pure[A] = freshVar[A ⇒ P[A]]

    val (goodSemimonads, goodMonads) = semimonadsAndMonads(fmapTerm, pure, flm)

    println(s"Good semimonads: flatten is one of ${goodSemimonads.map(_.prettyPrint)}")
    println("Good monads:")
    println(goodMonads.map { case (pure, ftn) ⇒ s"pure = ${pure.prettyPrint}, flatten = ${ftn.prettyPrint}" })

    goodSemimonads.size shouldEqual 2
    goodMonads.size shouldEqual 1
  }

  it should "check 1 + A x A monad" in {
    type P[A] = Option[(A, A)]

    def fmapTerm[A, B] = ofType[(A ⇒ B) ⇒ P[A] ⇒ P[B]].lambdaTerm

    def flm[A, B] = freshVar[(A ⇒ P[B]) ⇒ P[A] ⇒ P[B]]

    def pure[A] = freshVar[A ⇒ P[A]]

    val (goodSemimonads, goodMonads) = semimonadsAndMonads(fmapTerm, pure, flm)

    println(s"Good semimonads: flatten is one of ${goodSemimonads.map(_.prettyPrint)}")
    println("Good monads:")
    println(goodMonads.map { case (pure, ftn) ⇒ s"pure = ${pure.prettyPrint}, flatten = ${ftn.prettyPrint}" })

    goodSemimonads.size shouldEqual 5
    goodMonads.size shouldEqual 0
  }

  it should "check C + A x A monad" in {
    type P[A] = Either[Int, (A, A)]

    def fmapTerm[A, B] = ofType[(A ⇒ B) ⇒ P[A] ⇒ P[B]].lambdaTerm

    def flm[A, B] = freshVar[(A ⇒ P[B]) ⇒ P[A] ⇒ P[B]]

    def pure[A] = freshVar[A ⇒ P[A]]

    val (goodSemimonads, goodMonads) = semimonadsAndMonads(fmapTerm, pure, flm)

    println(s"Good semimonads: flatten is one of ${goodSemimonads.map(_.prettyPrint)}")
    println("Good monads:")
    println(goodMonads.map { case (pure, ftn) ⇒ s"pure = ${pure.prettyPrint}, flatten = ${ftn.prettyPrint}" })

    goodSemimonads.size shouldEqual 4
    goodMonads.size shouldEqual 0
  }

  it should "check Id + Id monad" in {
    type P[A] = Either[A, A]

    def fmapTerm[A, B] = ofType[(A ⇒ B) ⇒ P[A] ⇒ P[B]].lambdaTerm

    def flm[A, B] = freshVar[(A ⇒ P[B]) ⇒ P[A] ⇒ P[B]]

    def pure[A] = freshVar[A ⇒ P[A]]

    val (goodSemimonads, goodMonads) = semimonadsAndMonads(fmapTerm, pure, flm)

    println(s"Good semimonads: flatten is one of ${goodSemimonads.map(_.prettyPrint)}")
    println("Good monads:")
    println(goodMonads.map { case (pure, ftn) ⇒ s"pure = ${pure.prettyPrint}, flatten = ${ftn.prettyPrint}" })

    goodSemimonads.size shouldEqual 13
    goodMonads.size shouldEqual 6
  }

  it should "check Id + A x A monad" in {
    type P[A] = Either[A, (A, A)]

    def fmapTerm[A, B] = ofType[(A ⇒ B) ⇒ P[A] ⇒ P[B]].lambdaTerm

    def flm[A, B] = freshVar[(A ⇒ P[B]) ⇒ P[A] ⇒ P[B]]

    def pure[A] = freshVar[A ⇒ P[A]]

    val (goodSemimonads, goodMonads) = semimonadsAndMonads(fmapTerm, pure, flm)

    println(s"Good semimonads: flatten is one of ${goodSemimonads.map(_.prettyPrint)}")
    println("Good monads:")
    println(goodMonads.map { case (pure, ftn) ⇒ s"pure = ${pure.prettyPrint}, flatten = ${ftn.prettyPrint}" })

    goodSemimonads.size shouldEqual 12
    goodMonads.size shouldEqual 2
  }

  it should "check A + 1 ⇒ A monad" in {
    type P[A] = Either[A, Unit ⇒ A]

    def fmapTerm[A, B] = ofType[(A ⇒ B) ⇒ P[A] ⇒ P[B]].lambdaTerm

    def flm[A, B] = freshVar[(A ⇒ P[B]) ⇒ P[A] ⇒ P[B]]

    def pure[A] = freshVar[A ⇒ P[A]]

    val (goodSemimonads, goodMonads) = semimonadsAndMonads(fmapTerm, pure, flm, debug = true)

    println(s"Good semimonads: flatten is one of ${goodSemimonads.map(_.prettyPrint)}")
    println("Good monads:")
    println(goodMonads.map { case (pure, ftn) ⇒ s"pure = ${pure.prettyPrint}, flatten = ${ftn.prettyPrint}" })

    goodSemimonads.size shouldEqual 2
    goodMonads.size shouldEqual 1
  }
}
