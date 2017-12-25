package io.chymyst.ch.unit

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

  // TODO: make this work
  //    def f: Wrap2 ⇒ Wrap2c.type = implement

  it should "generate code for sealed trait" in {
    def f[A, B]: GadtChoice[A] ⇒ B = implement

    val r1 = f[String, Boolean](GadtChoice1(123, "abc", true))

    r1 shouldEqual true

    val r2 = f[Boolean, Int](GadtChoice2("abc", 123))

    r2 shouldEqual 123
  }
  it should "generate code for the weak law of _tertium non datur_" in {
    def f[A, B, C]: (Either[A, A ⇒ B] ⇒ B) ⇒ Either[B, C] = implement
  }

  it should "generate identity for Option[X]" in {
    def f[X] = ofType[Option[X] ⇒ Option[X]]

    def f2[X] = ofType[Option[X]]

    f2[Int] shouldEqual None

    // This code is generated for f:
    //    def f[X]: Option[X] ⇒ Option[X] = (c: Option[X]) => c match {
    //      case None => None
    //      case (a: Some[X]) => Some[X](a.value)
    //    }
    // Generating code such as `case (a : None.type) ⇒ None` does not work for some reason!

    f(Some(123)) shouldEqual Some(123)

    f(None) shouldEqual None
  }

  it should "generate code using various disjunction rules" in {
    def f1[A, B] = ofType[A ⇒ (Either[A, B] ⇒ B) ⇒ B]

    def f1a[A, B]: Either[A, B] ⇒ Either[B, A] = implement

    def f1b[A, B, C]: Either[A, Either[B, C]] ⇒ Either[Either[A, B], C] = implement

    def f2a[A, B, C, D, E] = allOfType[Either[(A, B), C] ⇒ (Either[A, C] ⇒ B ⇒ Either[C, D]) ⇒ Either[C, D]]

    f2a[Int, Int, Int, Int, Int].size shouldEqual 4

    def f2b[A, B, C, D, E]: Either[A, B] ⇒ (Either[A, B] ⇒ Either[C, D]) ⇒ Either[C, D] = implement

    def f2c[A, B, C, D, E]: (((A, E), B)) ⇒ Either[A, B] = implement

    def f2c1[A, B, C, D, E]: ((A, E)) ⇒ Either[A, B] = implement

    def f2d[A, B, C, D, E]: Either[(A, E), B] ⇒ Either[A, B] = implement

    def f2e[A, B, C, D, E]: Either[A, B] ⇒ Either[(A, A), B] = implement

    def f2f[A, B, C, D, E]: Either[A, B] ⇒ Either[A, B] = implement

    def f3[A, B, C, D, E] = allOfType[Either[(A, B), C] ⇒ (Either[A, C] ⇒ B ⇒ Either[C, D]) ⇒ (C ⇒ E) ⇒ Either[D, E]]

    f3[Int, Int, Int, Int, Int].size shouldEqual 2
  }

  it should "generate the example in the tutorial" in {
    case class User[N, I](name: N, id: I)
    def makeUser[N, I](userName: N, userIdGenerator: N ⇒ I): User[N, I] = implement

    makeUser(123, (x: Int) ⇒ x.toString) shouldEqual User(123, "123")
  }

  behavior of "named types"

  // TODO: make this work
  /*
  it should "generate code by reflection on named type that has no type parameters" in {
    type MyType = (Int, String)

    def f: MyType ⇒ Int = implement

    f((123, "abc")) shouldEqual 123
  }

  // TODO: make this work
  // This does not work because we match Tuple3 but `args` show only one type parameter. So this
  // is incorrectly recognized as a tuple with a single element of type T.

  it should "generate code by reflection on named type with type parameters" in {
    type MyType[T] = (Int, T, T)

    def f[T]: Int ⇒ T ⇒ T ⇒ MyType[T] = implement

    f(1)("abc") shouldEqual ((1, "abc", "abc"))
  }
*/
}
