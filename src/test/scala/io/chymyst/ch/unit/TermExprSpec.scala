package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class TermExprSpec extends FlatSpec with Matchers {

  val var12 = VarE("x1", TP("2"))
  val var23 = VarE("x2", TP("3"))
  val var32 = VarE("x3", TP("2"))
  val var41 = VarE("x4", TP("1"))
  val termExpr1 = CurriedE(List(var23, var32, var41), var32)
  val termExpr2 = CurriedE(List(var23, var32, var41), var12)
  val termExpr3 = CurriedE(List(var12), termExpr2)

  behavior of "TermExpr miscellaneous methods"

  it should "compute identity function" in {
    def idAB[A, B] = TermExpr.id(typeExpr[A ⇒ B])
    idAB.prettyPrint shouldEqual "x ⇒ x"
    idAB.toString shouldEqual "\\((x:A ⇒ B) ⇒ x)"
    idAB.t.prettyPrint shouldEqual "(A ⇒ B) ⇒ A ⇒ B"
  }

  it should "compute term size" in {
    TermExpr.size(termExpr1) shouldEqual 7 // x2 ⇒ x3 ⇒ x4 ⇒ x3
    TermExpr.size(termExpr3) shouldEqual 9 // x1 ⇒ x2 ⇒ x3 ⇒ x4 ⇒ x1
  }

  it should "compare function with non-function" in {
    termExpr1 equiv termExpr1 shouldEqual true
    termExpr1 equiv termExpr2 shouldEqual false
    termExpr1 equiv var23 shouldEqual false
  }

  it should "produce error when subst is done with non-matching type" in {
    val varZ = VarE("z", var23.t)
    TermExpr.subst(var23, varZ, var32 =>: var23).prettyPrint shouldEqual "x3 ⇒ z"

    TermExpr.subst(var23, termExpr1, var32 =>: var23).prettyPrint shouldEqual "x3 ⇒ x2 ⇒ x3 ⇒ x4 ⇒ x3"

    the[Exception] thrownBy {
      TermExpr.subst(var23, termExpr1, var32 =>: var23.copy(t = TP("1"))) shouldEqual (var32 =>: termExpr1)
    } should have message "In subst(x2, \\((x2:3) ⇒ (x3:2) ⇒ (x4:1) ⇒ x3), \\((x3:2) ⇒ x2)), found variable(s) (x2:1) with incorrect type(s), expected variable type 3"
  }

  it should "recover from incorrect substitution" in {
    val p = freshVar[(Int, Int)]
    val xV = freshVar[Int]
    val x = xV.copy(name = "x") // Rename this variable to avoid non-determinism in variable name during tests.
    val pair = p(x, x)

    TermExpr.subst(var23, pair, var23 =>: var12) shouldEqual var23 =>: var12

    the[Exception] thrownBy TermExpr.substMap(var23 =>: var12) {
      case VarE(_, _) ⇒ pair
    } should // The `subst` tries to replace `var23` in `var23 =>: var12` with `pair`, which is a NamedConjunctE.
      have message "Incorrect substitution of bound variable x2 by non-variable Tuple2(x, x) in substMap(x2 ⇒ x1){...}"
  }

  behavior of "TermExpr#renameVar"

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
    termExpr.prettyRenamePrint shouldEqual "a ⇒ b ⇒ a (c ⇒ c b)"
  }

  behavior of "TermExpr#freeVars"

  it should "detect free variables" in {
    termExpr1.freeVarNames shouldEqual Seq()
    termExpr2.freeVarNames shouldEqual Seq("x1")
    termExpr3.freeVarNames shouldEqual Seq()
  }

  behavior of "TermExpr#equiv"

  it should "detect equivalent terms" in {
    val termExpr1a = termExpr1.renameAllVars(Seq("x2", "x3", "x4"), Seq("y2", "y3", "y4"))
    TermExpr.equiv(termExpr1a, termExpr1) shouldEqual true
  }

  behavior of "information loss"

  it should "compute permutation score for conjunctions" in {

    def permutationScore(t: TermExpr) = t.informationLossScore._3

    val c = ConjunctE(Seq(VarE("a", TP("A")), VarE("b", TP("B"))))

    val t = ConjunctE(Seq(
      ProjectE(0, c),
      ProjectE(1, c)
    ))

    permutationScore(t) shouldEqual 0

    permutationScore(ConjunctE(Seq(
      ProjectE(1, c),
      ProjectE(1, c)
    ))) shouldEqual TermExpr.roundFactor(1)

    permutationScore(ConjunctE(Seq(
      ProjectE(1, c),
      ProjectE(0, c)
    ))) shouldEqual TermExpr.roundFactor(2)

    TermExpr.findAll(t) {
      case ProjectE(_, _) ⇒ "abc"
    } shouldEqual Seq("abc", "abc")

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

  behavior of "automatic alpha-conversions"

  type Pair[A] = (A, A)

  def fmap[A, B] = ofType[(A => B) => Pair[A] => Pair[B]].lambdaTerm

  def flatten[A] = anyOfType[Pair[Pair[A]] ⇒ Pair[A]]()

  it should "perform automatic type conversions when using :@" in {

    TypeExpr.allTypeParams(fmap.t) shouldEqual Set(TP("A"), TP("B"))

    def ftnAs[A] = allOfType[Pair[Pair[A]] ⇒ Pair[A]].map(_.lambdaTerm)

    val ftnA = flatten.head.lambdaTerm
    val head = fmap.asInstanceOf[CurriedE].heads.head
    TypeExpr.leftUnify(head.t, ftnA.t, head.t) match {
      case Right(m: Map[TP, TypeExpr]) ⇒ m.size shouldEqual 2
    }
    (fmap :@ ftnA).t.prettyPrint shouldEqual "Tuple2[Tuple2[Tuple2[A,A],Tuple2[A,A]],Tuple2[Tuple2[A,A],Tuple2[A,A]]] ⇒ Tuple2[Tuple2[A,A],Tuple2[A,A]]"
    (ftnA :@@ ftnA).t.prettyPrint shouldEqual "Tuple2[Tuple2[Tuple2[A,A],Tuple2[A,A]],Tuple2[Tuple2[A,A],Tuple2[A,A]]] ⇒ Tuple2[A,A]"
    (fmap @@: fmap).t.prettyPrint shouldEqual "(A ⇒ B) ⇒ Tuple2[Tuple2[A,A],Tuple2[A,A]] ⇒ Tuple2[Tuple2[B,B],Tuple2[B,B]]"
  }

  it should "automatically select the correct implementation for (A, A) monad" in {

    val terms = flatten

    terms.length shouldEqual 16

    // Select the implementations that satisfy rigorously the associativity law.
    // fmap ftn . ftn = ftn . ftn

    def associativeTerms[A] = flatten[A].filter { ftn ⇒
      val ftnA = ftn.lambdaTerm

      val ftnAftnA = ftnA :@@ ftnA

      // ftnLifted: Pair[Pair[Pair[C]]] ⇒ Pair[Pair[C]]
      val ftnLifted = fmap :@ ftnA

      // We could also use `ftnLifted andThen ftnA` here, since the function types already match.
      // But :@@ works just as well.
      val ftnLiftedftn = ftnLifted :@@ ftnA
      ftnLiftedftn equiv ftnAftnA
    }

    println("Semimonads:")
    associativeTerms[Int].map(_.lambdaTerm.prettyPrint).foreach(println)
    associativeTerms[Int].length shouldEqual 7 // One standard and six non-standard semimonads.
    /*
a ⇒ a._2 // Choose second outer tuple.
a ⇒ a._1 // Choose first outer tuple.
a ⇒ Tuple2(a._1._1, a._2._2) // The standard monad.
a ⇒ Tuple2(a._1._2, a._2._2) // Choose second inner tuple.
a ⇒ Tuple2(a._1._1, a._2._1) // Choose first inner tuple.
a ⇒ Tuple2(a._1._1, a._1._1) // Choose first element of first inner tuple.
a ⇒ Tuple2(a._2._2, a._2._2) // Choose second element of second inner tuple.
     */

    // Of these, select the implementations that satisfy rigorously the two identity laws.
    // pure . ftn = id
    // fmap pure . ftn = id

    def pure[A] = ofType[A ⇒ Pair[A]]

    val pureA = pure[Int].lambdaTerm
    val pureLA = fmap :@ pureA // fmap pure: Pair[A] ⇒ Pair[Pair[A]]

    def idAA[A] = ofType[Pair[A] ⇒ Pair[A]].lambdaTerm

    def monadTerms[A] = associativeTerms[A].filter { ftn ⇒
      val ftnA = ftn.lambdaTerm

      val law1 = pureA :@@ ftnA
      val law2 = pureLA :@@ ftnA
      (law1 equiv idAA) && (law2 equiv idAA)
    }

    println("Monads:")
    monadTerms[Int].map(_.lambdaTerm.prettyPrint).foreach(println)
    monadTerms[Int].length shouldEqual 1
  }

  it should "fail to apply @@: and :@@ to incorrect types" in {
    val a = VarE("a", BasicT("Int"))
    the[Exception] thrownBy (a @@: a) should have message "Call to `@@:` is invalid because the type of one of the arguments (<c>Int and <c>Int) is not of a function type"
    the[Exception] thrownBy (a :@@ a) should have message "Call to `:@@` is invalid because the type of one of the arguments (<c>Int and <c>Int) is not of a function type"
    the[Exception] thrownBy (fmap @@: (a =>: a)) should have message "Call to `:@@` is invalid because the function types (<c>Int ⇒ <c>Int and (A ⇒ B) ⇒ Tuple2[A,A] ⇒ Tuple2[B,B]) do not match: Cannot unify <c>Int with an incompatible type Tuple2[A,A] ⇒ Tuple2[B,B]"
    the[Exception] thrownBy (fmap :@@ (a =>: a)) should have message "Call to `:@@` is invalid because the function types ((A ⇒ B) ⇒ Tuple2[A,A] ⇒ Tuple2[B,B] and <c>Int ⇒ <c>Int) do not match: Cannot unify Tuple2[A,A] ⇒ Tuple2[B,B] with an incompatible type <c>Int"
    the[Exception] thrownBy (a :@ (a =>: a)) should have message "Call to `:@` is invalid because the head term a of type <c>Int is not a function"
    the[Exception] thrownBy (fmap :@ a) should have message "Cannot unify A ⇒ B with an incompatible type <c>Int"
  }

  it should "generate different implementations for tuples" in {
    type P[A] = Either[A, (A, A, A)]

    def fmap[A, B] = ofType[(A ⇒ B) ⇒ P[A] ⇒ P[B]].lambdaTerm

    def pure[A] = ofType[A ⇒ P[A]].lambdaTerm

    //    def flattens[A] = anyOfType[P[P[A]] ⇒ P[A]]()

    //    println(flattens.size)

    def f[A] = allOfType[Option[(A, A, A)] ⇒ Option[(A, A, A)]]()

    println(f.size)
    //    f[Int].map(_.lambdaTerm.prettyPrint).sorted.foreach(println)
    //    f.size shouldEqual factorial(4)
  }

  it should "generate match clauses" in {
    def f[A] = anyOfType[Option[Option[A]] ⇒ Option[Option[A]]]().map(_.lambdaTerm)

    println(f.size)
    f.map(_.prettyPrint).foreach(println)

  }

}
