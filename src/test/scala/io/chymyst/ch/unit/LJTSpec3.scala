package io.chymyst.ch.unit

import io.chymyst.ch.implement
import org.scalatest.{FlatSpec, Matchers}

class LJTSpec3 extends FlatSpec with Matchers {

  case class Wrap1[A, B](x: Int, a: A, b: B)

  sealed trait Choice[A]

  case class Choice1[A, B](x: Int, a: A, b: B) extends Choice[A]

  case class Choice2[B](name: String, bb: B) extends Choice[Boolean]

  behavior of "terms with case classes"

  it should "generate code for case class" in {
    def f[A, B]: Wrap1[A, B] ⇒ B = implement

    f(Wrap1(123, "abc", true)) shouldEqual true
  }

  it should "generate code for case class that is part of a sealed trait" in {
    def f[A, B]: Choice1[A, B] ⇒ B = implement

    f(Choice1(123, "abc", true)) shouldEqual true
  }

  it should "generate code for GADT case class that is part of a sealed trait" in {
    def f[B]: Choice2[B] ⇒ B = implement

    f(Choice2("abc", true)) shouldEqual true
  }

  /*
    it should "generate code for sealed trait" in {
      def f[A, B]: Choice[A] ⇒ B = implement

      val r1 = f[String, Boolean](Choice1(123, "abc", true))

      r1 shouldEqual true

      val r2 = f[Boolean, Int](Choice2("abc", 123))

      r2 shouldEqual 123
    }
  */
  // TODO: make this work
  /*
      it should "generate code for the weak law of excluded middle" in {
      def f[A, B]: (Either[A, A ⇒ B] ⇒ B) ⇒ B = implement
    }

        it should "generate code using various disjunction rules" in {
            def f[A, B, C, D, E]: A ⇒ Either[B, C] ⇒ (Either[A, C] ⇒ B ⇒ Either[C, D]) ⇒ (C ⇒ E) ⇒ Either[D, E] = implement
          }

        behavior of "named types"

        it should "generate code by reflection on named type" in {
          type MyType[T] = (Int, T, T)

          def f[T]: Int ⇒ T ⇒ T ⇒ MyType[T] = implement

          f(1)("abc") shouldEqual ((1, "abc", "abc"))
        }
  */
}
