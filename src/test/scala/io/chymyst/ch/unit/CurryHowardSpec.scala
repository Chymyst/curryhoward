package io.chymyst.ch.unit

import io.chymyst.ch.CurryHowardMacros._
import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class CurryHowardSpec extends FlatSpec with Matchers {
  /* Not used now.
    behavior of "syntax for untyped functions"

    it should "define syntax for untyped lambda calculus with products" in {
      // This is for testing only. We need to represent terms by a polynomial type, not by functions.
      sealed trait Term {
        def apply[T](x: T): Term = this // dummy implementation to simplify code
      }

      final case class \[T](v: Term ⇒ T) extends Term

      final case class \:[T](v: Any ⇒ T) extends Term

      "(x => x)" shouldNot compile

      val a1 = \(x ⇒ \(y ⇒ x))

      def f1[X, Y]: X => Y => X = x => y => x

      val a2 = \(x ⇒ \(y ⇒ x(y)))
      val a3 = \(x ⇒ \(y ⇒ y((x, x))))
      val a4 = \(x ⇒ \(y ⇒ \(z ⇒ x(y(z)))))
      val a5 = \(x ⇒ \(y ⇒ \(z ⇒ z(x(y)))))
      val a6 = \(x ⇒ (x, \(y ⇒ (x, y, x(y))))) // can use tuples
      val qqq = \ { x ⇒ x(()) } // can apply to unit
      val a7 = \: { case (x: Term, y: Term) ⇒ x(y(x)) } // use tuples as argument types, need annotations
    }
  */

  behavior of "reifyType"

  it should "produce correct type expressions for function types" in {
    val t = testReifyType[Int ⇒ Double ⇒ String]
    t shouldEqual BasicT("Int") ->: BasicT("Double") ->: BasicT("String")
  }

  it should "produce correct type expressions for case classes" in {
    case class AA[T](x: Int, t: T)
    val t = testReifyType[Int ⇒ Double ⇒ AA[Double]]
    t match {
      case BasicT(_) #-> (BasicT(_) #-> NamedConjunctT(constructor, tParams, accessors, wrapped)) ⇒
        constructor shouldEqual "AA"
        tParams shouldEqual List(BasicT("Double"))
        accessors shouldEqual List("x", "t")
        wrapped shouldEqual ConjunctT(List(BasicT("Int"), BasicT("Double")))
    }
    t shouldEqual BasicT("Int") ->: BasicT("Double") ->: NamedConjunctT("AA", List(BasicT("Double")), List("x", "t"), ConjunctT(List(BasicT("Int"), BasicT("Double"))))
  }

  it should "produce correct type expressions for sealed traits with generic types" in {
    sealed trait AA[U]
    case class AA1[U](x: U, d: Double) extends AA[U]
    case class AA2[U](y: U, b: Boolean) extends AA[U]
    def t[T] = testReifyType[T ⇒ Double ⇒ AA[T]]
    t[String] shouldEqual TP("T") ->: BasicT("Double") ->: DisjunctT("AA", List(TP("T")), Seq(
      NamedConjunctT("AA1", List(TP("T")), List("x", "d"), ConjunctT(List(TP("T"), BasicT("Double")))),
      NamedConjunctT("AA2", List(TP("T")), List("y", "b"), ConjunctT(List(TP("T"), BasicT("Boolean"))))
    ))
  }
/*
  it should "produce correct type expressions for sealed traits with generic types and nonstandard type variable names" in {
    sealed trait AA[U]
    case class AA1[V](x: V, d: Double) extends AA[V]
    case class AA2[W](y: W, b: Boolean) extends AA[W]
    def t[T] = testReifyType[T ⇒ Double ⇒ AA[T]]
    t[String] shouldEqual TP("T") ->: BasicT("Double") ->: DisjunctT("AA", List(TP("T")), Seq(
      NamedConjunctT("AA1", List(TP("T")), List("x", "d"), ConjunctT(List(TP("T"), BasicT("Double")))),
      NamedConjunctT("AA2", List(TP("T")), List("y", "b"), ConjunctT(List(TP("T"), BasicT("Boolean"))))
    ))
  }
*/
  it should "produce correct type expressions for sealed traits with concrete types" in {
    sealed trait AA[T]
    case class AA1[T](x: T, d: Double) extends AA[T]
    case class AA2[T](y: T, b: Boolean) extends AA[T]

    val tl = List(BasicT("Int"))
    val t = testReifyType[Int ⇒ Double ⇒ AA[Int]]
    t shouldEqual BasicT("Int") ->: BasicT("Double") ->: DisjunctT("AA", tl, Seq(
      NamedConjunctT("AA1", tl, List("x", "d"), ConjunctT(List(tl.head, BasicT("Double")))),
      NamedConjunctT("AA2", tl, List("y", "b"), ConjunctT(List(tl.head, BasicT("Boolean"))))
    ))
  }

  it should "produce correct type expressions for sealed traits with case objects and empty-argument case classes" in {

    sealed trait BB[T]
    case class BB1() extends BB[Int]
    case object BB2 extends BB[Boolean]

    val t = testReifyType[Int ⇒ Double ⇒ BB[Int]]
    t match {
      case BasicT("Int") #-> (BasicT("Double") #-> DisjunctT("BB", List(BasicT("Int")), Seq(c1, c2))) ⇒
        c1 shouldEqual NamedConjunctT("BB1", List(), List(), UnitT(""))
        c2 shouldEqual NamedConjunctT("BB2", List(), List(), NothingT("Nothing"))
    }
  }

  it should "produce correct type expressions for Left" in {
    val b = testReifyType[Left[Int, Double]]
    b shouldEqual NamedConjunctT("Left", List(BasicT("Int"), BasicT("Double")), List("value"), BasicT("Int"))
  }

  it should "produce correct type expressions for Either with concrete types" in {
    val t = testReifyType[Either[Int, Double]]
    val typeList = List(BasicT("Int"), BasicT("Double"))
    t shouldEqual DisjunctT("Either", typeList, List(NamedConjunctT("Left", typeList, List("value"), typeList(0)),
      NamedConjunctT("Right", typeList, List("value"), typeList(1))))
  }

  it should "produce correct type expressions for Either with function type" in {
    val t = testReifyType[Either[Int, Int ⇒ Double]]
    val typeList = List(BasicT("Int"), #->(BasicT("Int"), BasicT("Double")))
    t shouldEqual DisjunctT("Either", typeList, List(NamedConjunctT("Left", typeList, List("value"), typeList(0)),
      NamedConjunctT("Right", typeList, List("value"), typeList(1))))
  }

  it should "produce correct type expressions for Either as result type" in {
    def t[P, Q] = testReifyType[Option[P] ⇒ Either[P, Q]]
    val tl2 = List(TP("P"), TP("Q"))
    val tl1 = List(TP("P"))
    t[Int, String] shouldEqual DisjunctT("Option", tl1, List(NamedConjunctT("None", Nil, Nil, NothingT("Nothing")), NamedConjunctT("Some", tl1, List("value"), TP("P")))) ->: DisjunctT("Either", tl2, List(NamedConjunctT("Left", tl2, List("value"), TP("P")),
      NamedConjunctT("Right", tl2, List("value"), TP("Q"))))
  }

  it should "produce correct type expression for unknown type constructors" in {
    val t = testReifyType[IndexedSeq[Int]]
    t shouldEqual ConstructorT("IndexedSeq[Int]")
  }

  it should "produce correct type expression for unknown type constructors under Option" in {
    val t = testReifyType[Option[IndexedSeq[Int]]]
    val t1 = ConstructorT("IndexedSeq[Int]")
    val t1l = List(t1)
    t shouldEqual DisjunctT("Option", t1l, List(NamedConjunctT("None", List(), List(), NothingT("Nothing")), NamedConjunctT("Some", t1l, List("value"), t1)))
  }

  behavior of "syntax of `implement` and `typeOf`"

  it should "compile" in {
    def f1[X, Y]: X ⇒ Y ⇒ X = implement

    // This does not work because `implement` needs to access the type of the enclosing owner!
    // The compiler error is "recursive method f2 needs type".
    " def f2a[X, Y] = implement[X ⇒ Y ⇒ X] " shouldNot compile

    def f2[X, Y] = ofType[X ⇒ Y ⇒ X]

    // does not work because ofType[Nothing] is instantiated: the macro does not use the enclosing owner.
    " def f3[X, Y]: X ⇒ Y ⇒ X = ofType " shouldNot compile
  }

  it should "get the list of propositions" in {
    TermExpr.propositions(CurriedE(List(PropE("A", TP("A"))), AppE(PropE("B", TP("B") ->: TP("A")), PropE("B", TP("B"))))) shouldEqual Set(PropE("A", TP("A")), PropE("B", TP("B")), PropE("B", TP("B") ->: TP("A")))
  }

  it should "generate correct code for the identity function with `ofType[]` syntax" in {
    def f1[X] = ofType[X ⇒ X]

    f1(123) shouldEqual 123
    f1("abc") shouldEqual "abc"
    f1(true) shouldEqual true
  }

  it should "generate correct code for the const function with `ofType[]` syntax" in {
    def f2[X, Y] = ofType[X ⇒ Y ⇒ X]

    val cTrue = f2(true)
    cTrue(123) shouldEqual true
    cTrue("abc") shouldEqual true
    cTrue(true) shouldEqual true

    val c3 = f2(3)
    c3(123) shouldEqual 3
    c3("abc") shouldEqual 3
    c3(true) shouldEqual 3
  }

  it should "generate correct code for the permuted const function with `ofType[]` syntax" in {
    def f2[X, Y]: X ⇒ Y ⇒ Y = implement

    f2(123)("true") shouldEqual "true"
    f2(false)(1.0) shouldEqual 1.0
  }

  it should "generate correct code for the identity function with standard syntax" in {
    def f1[X]: X ⇒ X = implement

    f1(123) shouldEqual 123
    f1("abc") shouldEqual "abc"
    f1(true) shouldEqual true
  }

  it should "generate correct code for the const function with standard syntax" in {
    def f2[X, Y]: X ⇒ Y ⇒ X = implement

    val cTrue = f2(true)
    cTrue(123) shouldEqual true
    cTrue("abc") shouldEqual true
    cTrue(true) shouldEqual true

    val c3 = f2(3)
    c3(123) shouldEqual 3
    c3("abc") shouldEqual 3
    c3(true) shouldEqual 3
  }

  it should "fail to compile when two possible implementations are equally good" in {
    "def f1[X, A, B]: X ⇒ A ⇒ X ⇒ X = implement" shouldNot compile
  }

  it should "generate correct code for the const function with extra unused arguments" in {
    def f1[X, A, B]: X ⇒ A ⇒ B ⇒ X = implement

    f1(123)("q")(true) shouldEqual 123
    f1("abc")(Some((1, 1)))(Map()) shouldEqual "abc"
    f1(true)(123.0)('blah) shouldEqual true
  }

  it should "generate correct code for the identity function on a=>b" in {
    def f2[X, Y]: (X ⇒ Y) ⇒ X ⇒ Y = implement

    val printInt: Int ⇒ String = _.toString

    f2(printInt)(123) shouldEqual "123"
  }

  it should "generate correct code for the const function with more unused arguments of coincident type" in {
    def f1[X, A, B]: A ⇒ X ⇒ A ⇒ B ⇒ X = implement

    f1("b")(123)("q")(true) shouldEqual 123
    f1(Some((3, 4)))("abc")(Some((1, 1)))(Map()) shouldEqual "abc"
    f1(0.0)(true)(123.0)('blah) shouldEqual true
  }

  // TODO: make this work too!
  it should "generate correct code for the identity function with explicit arguments" in {
    "def f1[X](x: X): X = implement" shouldNot compile
    //
    //    f1(123) shouldEqual 123
    //    f1("abc") shouldEqual "abc"
    //    f1(true) shouldEqual true
  }
}
