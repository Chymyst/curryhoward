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

  System.setProperty("curryhoward.log", "prover,macros,terms")

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

  it should "generate code with case objects correctly" in {
    def f: Wrap2 ⇒ Wrap2c.type = implement
  }

  it should "generate code for a sealed trait" in {
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

    // Case match expressions should be simplified into identity functions, so we should get one expression here.
    def f2a[A, B, C, D, E] = allOfType[Either[(A, B), C] ⇒ (Either[A, C] ⇒ B ⇒ Either[C, D]) ⇒ Either[C, D]]

    f2a[Int, Int, Int, Int, Int].size shouldEqual 1

    def f2b[A, B, C, D, E]: Either[A, B] ⇒ (Either[A, B] ⇒ Either[C, D]) ⇒ Either[C, D] = implement

    def f2c[A, B, C, D, E]: (((A, E), B)) ⇒ Either[A, B] = implement

    def f2c1[A, B, C, D, E]: ((A, E)) ⇒ Either[A, B] = implement

    def f2d[A, B, C, D, E]: Either[(A, E), B] ⇒ Either[A, B] = implement

    def f2e[A, B, C, D, E]: Either[A, B] ⇒ Either[(A, A), B] = implement

    def f2f[A, B, C, D, E]: Either[A, B] ⇒ Either[A, B] = implement

    def f3[A, B, C, D, E] = allOfType[Either[(A, B), C] ⇒ (Either[A, C] ⇒ B ⇒ Either[C, D]) ⇒ (C ⇒ E) ⇒ Either[D, E]]

    f3[Int, Int, Int, Int, Int].size shouldEqual 1
  }

  it should "generate methods for Continuation monad with no ambiguity" in {
    case class Cont[X, R](c: (X ⇒ R) ⇒ R)

    def points[D, A] = allOfType[A ⇒ Cont[A, D]]().length

    points[Int, String] shouldEqual 1

    def maps[D, A, B] = allOfType[Cont[A, D] ⇒ (A ⇒ B) ⇒ Cont[B, D]]().length

    maps[Int, String, Boolean] shouldEqual 1

    def flatmaps[D, A, B] = allOfType[Cont[A, D] ⇒ (A ⇒ Cont[B, D]) ⇒ Cont[B, D]]().length

    flatmaps[Int, String, Boolean] shouldEqual 1
  }

  it should "generate contramap involving Option as argument" in {
    allOfType[Option[Int] ⇒ (Option[Int] ⇒ String) ⇒ String].length shouldEqual 1

    def contramaps[D, A, B] = allOfType[(Option[A] ⇒ D) ⇒ (B ⇒ A) ⇒ (Option[B] ⇒ D)].length

    contramaps[Int, String, Boolean] shouldEqual 1
  }

  it should "enumerate all implementations for the Reader-Option monad" in {
    def points[D, A] = allOfType[A ⇒ (D ⇒ Option[A])]().length

    def maps[D, A, B] = allOfType[(D ⇒ Option[A]) ⇒ (A ⇒ B) ⇒ (D ⇒ Option[B])]().length

    def flatmaps[D, A, B] = allOfType[(D ⇒ Option[A]) ⇒ (A ⇒ (D ⇒ Option[B])) ⇒ (D ⇒ Option[B])]().length

    points[Int, String] shouldEqual 1
    maps[Int, String, Boolean] shouldEqual 1
    flatmaps[Int, String, Boolean] shouldEqual 1
  }

  it should "fail to generate join or contrajoin involving Option as argument" in {
    def contrajoins1[D, A] = allOfType[(Option[A] ⇒ D) ⇒ (Option[Option[A] ⇒ D] ⇒ D)].length

    def contrajoins2[D, A] = allOfType[(Option[Option[A] ⇒ D] ⇒ D) ⇒ (Option[A] ⇒ D)].length

    contrajoins1[Int, String] shouldEqual 0
    contrajoins2[Int, String] shouldEqual 0
  }

  it should "generate methods for the Density-Option monad" in {
    def points[D, A] = allOfType[A ⇒ ((Option[A] ⇒ D) ⇒ Option[A])]()

    points[Int, String].length shouldEqual 1
    points[Int, String].head("abc")(_ ⇒ 123) shouldEqual Some("abc")

    def maps[D, A, B] = allOfType[((Option[A] ⇒ D) ⇒ Option[A]) ⇒ (A ⇒ B) ⇒ ((Option[B] ⇒ D) ⇒ Option[B])]()

    maps[Int, String, String].length shouldEqual 1
    // Should not be a trivial implementation that always returns `None`.
    val dString: (Option[String] ⇒ Int) ⇒ Option[String] = f ⇒ if (f(Some("abc")) > f(Some("ab"))) Some("abc") else None
    val f: String ⇒ String = identity
    val g: Option[String] ⇒ Int = os ⇒ if (os.contains("abc")) 10 else 0
    maps[Int, String, String].head(dString)(f)(g) shouldEqual Some("abc")

    // This takes a longer time.
    def flatmaps[D, A, B] = allOfType[((Option[A] ⇒ D) ⇒ Option[A]) ⇒ (A ⇒ ((Option[B] ⇒ D) ⇒ Option[B])) ⇒ ((Option[B] ⇒ D) ⇒ Option[B])]().length

    flatmaps[Int, String, Boolean] shouldEqual 4

    // We cannot select the "best" implementation automatically.
    "def flatmaps1[D, A, B] = ofType[((Option[A] ⇒ D) ⇒ Option[A]) ⇒ (A ⇒ ((Option[B] ⇒ D) ⇒ Option[B])) ⇒ ((Option[B] ⇒ D) ⇒ Option[B])]()" shouldNot compile
  }

  it should "generate the examples in the tutorial" in {
    case class User[N, I](name: N, id: I)
    def makeUser[N, I](userName: N, userIdGenerator: N ⇒ I): User[N, I] = implement

    makeUser(123, (x: Int) ⇒ x.toString) shouldEqual User(123, "123")

    ofType[User[Int, String]](123, (x: Int) ⇒ x.toString) shouldEqual User(123, "123")
  }

  behavior of "ofType"

  it should "detect type of all relevant variables" in {
    val yyy: Int = 123
    val zzz: String = "abc"
    val p = ofType[(Int, String)](yyy, zzz)
    p shouldEqual ((123, "abc"))
  }

  it should "use ofType without arguments" in {
    val p = ofType[Int ⇒ Int]()
    p(123) shouldEqual 123
  }

  it should "use ofType with constant arguments" in {
    val p = ofType[(Int, String)](123, "abc")
    p shouldEqual ((123, "abc"))
  }

  behavior of "named types"

  it should "generate code by reflection on named type that has no type parameters" in {
    type MyType = (Int, String)

    def f: MyType ⇒ Int = implement

    f((123, "abc")) shouldEqual 123
  }

  it should "generate code by reflection on named type that represents the Reader monad" in {
    type MyType[T] = Int ⇒ T

    def f[T]: MyType[T] ⇒ Int ⇒ T = implement
  }

  it should "generate code by reflection on named type with type parameters" in {
    type MyType[T] = (Int, T, T)

    def f[T]: Int ⇒ T ⇒ MyType[T] = implement

    f(1)("abc") shouldEqual ((1, "abc", "abc"))

    type A[T] = ((T, Int)) ⇒ T

    val x = ofType[A[String]]

    x(("abc", 123)) shouldEqual "abc"
  }

  it should "check miscellaneous logic identities" in {
    def f1[A, B, C] = ofType[(A ⇒ B) ⇒ (B ⇒ C) ⇒ (A ⇒ C)]

    def f2[A, B, C] = ofType[(A ⇒ B ⇒ C) ⇒ (B ⇒ A ⇒ C)]

    def f3[A, B] = allOfType[((A ⇒ B) ⇒ B) ⇒ (B ⇒ A) ⇒ A].length

    f3[String, Int] shouldEqual 0

    def f4[A, B] = ofType[((A ⇒ B) ⇒ B ⇒ A) ⇒ B ⇒ A]

    def f5[A, B] = ofType[Either[A, B] ⇒ (A ⇒ B) ⇒ B]

    def f6[A, B] = allOfType[A ⇒ B ⇒ Either[A, B]].length

    f6[Int, String] shouldEqual 2

    def f7[A, B, C] = allOfType[(((A ⇒ B, B ⇒ A)) ⇒ C) ⇒ C].length

    f7[Int, String, Boolean] shouldEqual 0

    def f8[A, B, C] = allOfType[(((A ⇒ B, B ⇒ A)) ⇒ A) ⇒ A].length

    f8[Int, String, Boolean] shouldEqual 0
  }

}
