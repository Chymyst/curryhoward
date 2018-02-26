package io.chymyst.ch.unit

import io.chymyst.ch.Macros._
import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class MacrosSpec extends FlatSpec with Matchers {
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

  behavior of "emitTypeCode"

  it should "produce correct type expressions for function types" in {
    val t = freshVar[Int ⇒ Double ⇒ String].t
    t shouldEqual BasicT("Int") ->: BasicT("Double") ->: BasicT("String")
  }

  it should "produce correct type expressions for case classes" in {
    case class AA[T](x: Int, t: T)
    val t = freshVar[Int ⇒ Double ⇒ AA[Double]].t
    t match {
      case BasicT(_) #-> (BasicT(_) #-> NamedConjunctT(constructor, tParams, accessors, wrapped)) ⇒
        constructor shouldEqual "AA"
        tParams shouldEqual List(BasicT("Double"))
        accessors shouldEqual List("x", "t")
        wrapped shouldEqual List(BasicT("Int"), BasicT("Double"))
    }
    t shouldEqual BasicT("Int") ->: BasicT("Double") ->: NamedConjunctT("AA", List(BasicT("Double")), List("x", "t"), List(BasicT("Int"), BasicT("Double")))
  }

  it should "produce correct type expressions for sealed traits with generic types" in {
    sealed trait AA[U]
    case class AA1[U](x: U, d: Double) extends AA[U]
    case class AA2[U](y: U, b: Boolean) extends AA[U]
    def t[T] = freshVar[T ⇒ Double ⇒ AA[T]].t
    t[String] shouldEqual TP("T") ->: BasicT("Double") ->: DisjunctT("AA", List(TP("T")), Seq(
      NamedConjunctT("AA1", List(TP("T")), List("x", "d"), List(TP("T"), BasicT("Double"))),
      NamedConjunctT("AA2", List(TP("T")), List("y", "b"), List(TP("T"), BasicT("Boolean")))
    ))
  }
  // TODO: make this work by introspecting the remapping of type variables between the trait and its child classes
  it should "produce correct type expressions for sealed traits with generic types and nonstandard type variable names" in {
    sealed trait AA[U]
    case class AA1[V](x: V, d: Double) extends AA[V]
    case class AA2[W](y: W, b: Boolean) extends AA[W]
    def t[T] = freshVar[T ⇒ Double ⇒ AA[T]].t
    t[String] shouldEqual TP("T") ->: BasicT("Double") ->: DisjunctT("AA", List(TP("T")), Seq(
      NamedConjunctT("AA1", List(TP("T")), List("x", "d"), List(TP("T"), BasicT("Double"))),
      NamedConjunctT("AA2", List(TP("T")), List("y", "b"), List(TP("T"), BasicT("Boolean")))
    ))
  }

  it should "produce correct type expressions for sealed traits with concrete types" in {
    sealed trait AA[T]
    case class AA1[T](x: T, d: Double) extends AA[T]
    case class AA2[T](y: T, b: Boolean) extends AA[T]

    val tl = List(BasicT("Int"))
    val t = freshVar[Int ⇒ Double ⇒ AA[Int]].t
    t shouldEqual BasicT("Int") ->: BasicT("Double") ->: DisjunctT("AA", tl, Seq(
      NamedConjunctT("AA1", tl, List("x", "d"), List(tl.head, BasicT("Double"))),
      NamedConjunctT("AA2", tl, List("y", "b"), List(tl.head, BasicT("Boolean")))
    ))
  }

  it should "produce correct type expressions for sealed traits with case objects and empty-argument case classes" in {

    sealed trait BB[T]
    case class BB1() extends BB[Int]
    case object BB2 extends BB[Boolean]

    val t = freshVar[Int ⇒ Double ⇒ BB[Int]].t
    t match {
      case BasicT("Int") #-> (BasicT("Double") #-> DisjunctT("BB", List(BasicT("Int")), Seq(c1, c2))) ⇒
        c1 shouldEqual NamedConjunctT("BB1", Nil, Nil, List(UnitT("BB1")))
        c2 shouldEqual NamedConjunctT("BB2", Nil, Nil, Nil)
    }
  }

  it should "produce correct type expressions for Left" in {
    val b = freshVar[Left[Int, Double]].t
    b shouldEqual NamedConjunctT("Left", List(BasicT("Int"), BasicT("Double")), List("value"), List(BasicT("Int")))
  }

  it should "produce correct type expressions for Either with concrete types" in {
    val t = freshVar[Either[Int, Double]].t
    val typeList = List(BasicT("Int"), BasicT("Double"))
    t shouldEqual DisjunctT("Either", typeList, List(NamedConjunctT("Left", typeList, List("value"), List(typeList(0))),
      NamedConjunctT("Right", typeList, List("value"), List(typeList(1)))))
  }

  it should "produce correct type expressions for Either with function type" in {
    val t = freshVar[Either[Int, Int ⇒ Double]].t
    val typeList = List(BasicT("Int"), #->(BasicT("Int"), BasicT("Double")))
    t shouldEqual DisjunctT("Either", typeList, List(NamedConjunctT("Left", typeList, List("value"), List(typeList(0))),
      NamedConjunctT("Right", typeList, List("value"), List(typeList(1)))))
  }

  it should "produce correct type expressions for Either as result type" in {
    def t[P, Q] = freshVar[Option[P] ⇒ Either[P, Q]].t
    val tl2 = List(TP("P"), TP("Q"))
    val tl1 = List(TP("P"))
    t[Int, String] shouldEqual DisjunctT("Option", tl1, List(NamedConjunctT("None", Nil, Nil, Nil), NamedConjunctT("Some", tl1, List("value"), tl1))) ->: DisjunctT("Either", tl2, List(NamedConjunctT("Left", tl2, List("value"), tl1),
      NamedConjunctT("Right", tl2, List("value"), List(TP("Q")))))
  }

  it should "produce correct type expression for unknown type constructors" in {
    val t = freshVar[IndexedSeq[Int]].t
    t shouldEqual ConstructorT("IndexedSeq", List(BasicT("Int")))
  }

  it should "produce correct type expression for unknown type constructors under Option" in {
    val t = freshVar[Option[IndexedSeq[Int]]].t
    val t1 = ConstructorT("IndexedSeq", List(BasicT("Int")))
    val t1l = List(t1)
    t shouldEqual DisjunctT("Option", t1l, List(NamedConjunctT("None", List(), List(), Nil), NamedConjunctT("Some", t1l, List("value"), t1l)))
  }
}
