package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, FlatSpec, Matchers}

trait LawChecking extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  def fEqual[A: Arbitrary, B](f1: A ⇒ B, f2: A ⇒ B): Assertion = {
    forAll { (x: A) ⇒ f1(x) shouldEqual f2(x) }
  }

  private def checkFunctionEquality[A: Arbitrary, B](resultsEqual: (B, B) ⇒ Assertion)(f1: A ⇒ B, f2: A ⇒ B): Assertion = {
    forAll { (x: A) ⇒ resultsEqual(f1(x), f2(x)) }
  }

  // Check equality for higher-order functions of type A ⇒ B ⇒ C.
  def hofEqual[A: Arbitrary, B: Arbitrary, C: Arbitrary](f1: A ⇒ B ⇒ C, f2: A ⇒ B ⇒ C): Assertion =
    checkFunctionEquality[A, B ⇒ C]((x: B ⇒ C, y: B ⇒ C) ⇒ fEqual(x, y))(f1, f2)

}

class LawsSpec extends LawChecking {

  behavior of "generated type class methods"

  it should "check laws for Reader monad" in {
    def pure[E, A]: A ⇒ (E ⇒ A) = implement

    def map[E, A, B]: (E ⇒ A) ⇒ (A ⇒ B) ⇒ (E ⇒ B) = implement

    def fmap[E, A, B]: (A ⇒ B) ⇒ (E ⇒ A) ⇒ (E ⇒ B) = implement

    forAll { (reader: Int ⇒ String) ⇒
      // fmap id = id
      fEqual(map(reader)(identity[String]), reader)
    }

    // Same check, using the helper methods.
    hofEqual((reader: Int ⇒ String) ⇒ map(reader)(identity[String]), identity[Int ⇒ String])

    forAll { (reader: String ⇒ Int, f: Int ⇒ Int, g: Int ⇒ Int) ⇒
      // fmap f . fmap g = fmap (f . g)

      val x = fmap[String, Int, Int](f) compose fmap[String, Int, Int](g)
      val y = fmap[String, Int, Int](f compose g)
      fEqual(x(reader), y(reader))
    }

    forAll { (a: String, f: String ⇒ Int) ⇒
      // pure(a).map(f) = pure(f(a))
      val pure_dot_map = map(pure[Int, String](a))(f)
      val f_dot_pure = pure[Int, Int](f(a))
      fEqual(pure_dot_map, f_dot_pure)
    }

    def flatMap[E, A, B]: (E ⇒ A) ⇒ (A ⇒ E ⇒ B) ⇒ (E ⇒ B) = implement

    // r.flatMap(pure) = r
    forAll { (reader: Int ⇒ String) ⇒
      val flatMap_pure = flatMap(reader)(pure)
      fEqual(flatMap_pure, reader)
    }
  }

  it should "check laws for State monad" in {
    def pure[S, A]: A ⇒ (S ⇒ (A, S)) = implement

    def map[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ (S ⇒ (B, S)) = implement

    def flatMap[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ S ⇒ (B, S)) ⇒ (S ⇒ (B, S)) = implement
  }

}
