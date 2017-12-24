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
