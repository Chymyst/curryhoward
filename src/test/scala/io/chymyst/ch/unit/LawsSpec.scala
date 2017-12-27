package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, FlatSpec, Matchers}

trait Fmap[F[_]] {
  def fmap[A, B]: (A ⇒ B) ⇒ F[A] ⇒ F[B]
}

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

  //
  //  def fmapLawIdentity[A: Arbitrary, F[_]](fmap: (A ⇒ A) ⇒ F[A] ⇒ F[A])(fResultsEqual: (F[A], F[A]) ⇒ Assertion): Assertion = {
  //    checkFunctionEquality[F[A], F[A]](fResultsEqual)(fmap(identity[A]), identity[F[A]])
  //  }

  def fmapLawIdentity[A: Arbitrary, F[_]](fmap: Fmap[F])(fResultsEqual: (F[A], F[A]) ⇒ Assertion)(implicit ev: Arbitrary[F[A]]): Assertion = {
    checkFunctionEquality[F[A], F[A]](fResultsEqual)(fmap.fmap(identity[A]), identity[F[A]])
  }

  def fmapLawComposition[A: Arbitrary, B: Arbitrary, C: Arbitrary, F[_]](fmap: Fmap[F])(fResultsEqual: (F[C], F[C]) ⇒ Assertion)(implicit evA: Arbitrary[F[A]], evB: Arbitrary[F[B]], evAB: Arbitrary[A ⇒ B], evBC: Arbitrary[B ⇒ C]): Assertion = {
    forAll { (f: A ⇒ B, g: B ⇒ C) ⇒
      checkFunctionEquality[F[A], F[C]](fResultsEqual)(fmap.fmap(f) andThen fmap.fmap(g), fmap.fmap(f andThen g))
    }
  }
}

class LawsSpec extends LawChecking {

  behavior of "generated type class methods"

  it should "check laws for Reader monad" in {
    def pointReader[E, A]: A ⇒ (E ⇒ A) = implement

    def mapReader[E, A, B]: (E ⇒ A) ⇒ (A ⇒ B) ⇒ (E ⇒ B) = implement

    def fmapReader[E, A, B]: (A ⇒ B) ⇒ (E ⇒ A) ⇒ (E ⇒ B) = implement

    // Check identity law with E = Int and A = String.
    forAll { (reader: Int ⇒ String) ⇒
      // fmap id = id
      fEqual(mapReader(reader)(identity[String]), reader)
    }

    type Reader[A] = Int ⇒ A

    def readersEqual[A](r1: Reader[A], r2: Reader[A]): Assertion = forAll { x: Int ⇒ r1(x) shouldEqual r2(x) }

    val fmapR = new Fmap[Reader] {
      override def fmap[A, B]: (A ⇒ B) ⇒ (Reader[A]) ⇒ Reader[B] = fmapReader[Int, A, B]
    }

    // Check identity law with E = Int and A = String.
    fmapLawIdentity[String, Reader](fmapR)(readersEqual)

    fmapLawComposition[Boolean, Long, String, Reader](fmapR)(readersEqual)

    // Same check, using the helper methods.
    hofEqual((reader: Int ⇒ String) ⇒ mapReader(reader)(identity[String]), identity[Int ⇒ String])

    forAll { (reader: String ⇒ Int, f: Int ⇒ Int, g: Int ⇒ Int) ⇒
      // fmap f . fmap g = fmap (f . g)

      val x = fmapReader[String, Int, Int](f) compose fmapReader[String, Int, Int](g)
      val y = fmapReader[String, Int, Int](f compose g)
      fEqual(x(reader), y(reader))
    }

    forAll { (a: String, f: String ⇒ Int) ⇒
      // pure(a).map(f) = pure(f(a))
      val pure_dot_map = mapReader(pointReader[Int, String](a))(f)
      val f_dot_pure = pointReader[Int, Int](f(a))
      fEqual(pure_dot_map, f_dot_pure)
    }

    def flatMapReader[E, A, B]: (E ⇒ A) ⇒ (A ⇒ E ⇒ B) ⇒ (E ⇒ B) = implement

    // r.flatMap(pure) = r
    forAll { (reader: Int ⇒ String) ⇒
      val flatMap_pure = flatMapReader(reader)(pointReader)
      fEqual(flatMap_pure, reader)
    }
  }

  it should "check laws for State monad" in {
    def pointState[S, A]: A ⇒ (S ⇒ (A, S)) = implement

    def mapState[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ B) ⇒ (S ⇒ (B, S)) = implement

    def flatMapState[S, A, B]: (S ⇒ (A, S)) ⇒ (A ⇒ S ⇒ (B, S)) ⇒ (S ⇒ (B, S)) = implement
  }

}
