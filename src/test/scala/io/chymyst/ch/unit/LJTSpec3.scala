package io.chymyst.ch.unit

import io.chymyst.ch.CurryHowardMacros.testReifyType
import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

case class Wrap1[A, B](x: Int, a: A, b: B)

sealed trait SimpleChoice[A]

case class SimpleChoice1[A](x: A) extends SimpleChoice[A]

case class SimpleChoice2[A](x: A, y: A) extends SimpleChoice[A]

sealed trait GadtChoice[A]

case class GadtChoice1[A, B](x: Int, a: A, b: B) extends GadtChoice[A]

case class GadtChoice2[B](name: String, bb: B) extends GadtChoice[Boolean]

sealed trait Wrap2

case class Wrap2a(x: Int, y: String) extends Wrap2

case class Wrap2b() extends Wrap2

case object Wrap2c extends Wrap2

case class Wrap2d[A]() extends Wrap2

case class Wrap2e[A](a: A) extends Wrap2

class LJTSpec3 extends FlatSpec with Matchers {

  behavior of "terms with case classes"
/*
  it should "generate code for case class" in {
    def f[A, B]: Wrap1[A, B] ⇒ B = implement

    f(Wrap1(123, "abc", true)) shouldEqual true
  }

  it should "generate code for case class that is part of a sealed trait" in {
    def f[A, B]: GadtChoice1[A, B] ⇒ B = implement

    f(GadtChoice1(123, "abc", true)) shouldEqual true
  }

  it should "generate code for GADT case class that is part of a sealed trait" in {
    def f[B]: GadtChoice2[B] ⇒ B = implement

    f(GadtChoice2("abc", true)) shouldEqual true
  }

  //  def f: Wrap2 ⇒ Wrap2c.type = implement

  it should "generate code for sealed trait" in {
    def f[A, B]: GadtChoice[A] ⇒ B = implement

    val r1 = f[String, Boolean](GadtChoice1(123, "abc", true))

    r1 shouldEqual true

    val r2 = f[Boolean, Int](GadtChoice2("abc", 123))

    r2 shouldEqual 123
  }
*/
  it should "produce correct type parameters for Either with concrete types" in {
    val t = testReifyType[Either[Int, Double]]
    val typeList = List(BasicT("Int"), BasicT("Double"))
    t shouldEqual DisjunctT("Either", typeList, List(NamedConjunctT("Left", typeList, List("value"), BasicT("Int")),
      NamedConjunctT("Right", typeList, List("value"), BasicT("Double"))))
  }

  it should "generate code for the weak law of _tertium non datur_" in {
    //    def f[A, B]: (Either[A, A ⇒ B] ⇒ B) ⇒ B = implement
  }

    it should "generate code using various disjunction rules" in {
  //    def f[A, B, C, D, E]: A ⇒ Either[B, C] ⇒ (Either[A, C] ⇒ B ⇒ Either[C, D]) ⇒ (C ⇒ E) ⇒ Either[D, E] = implement
    }

  behavior of "named types"
  // TODO: make this work
  // This does not work because we match Tuple3 but `args` show only one type parameter. So this
  // is incorrectly recognized as a tuple with a single element of type T.
  /*
  it should "generate code by reflection on named type" in {
    type MyType[T] = (Int, T, T)

    def f[T]: Int ⇒ T ⇒ T ⇒ MyType[T] = implement

    f(1)("abc") shouldEqual ((1, "abc", "abc"))
  }
*/
}
