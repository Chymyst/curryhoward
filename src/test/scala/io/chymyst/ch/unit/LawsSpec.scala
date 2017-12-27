package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, FlatSpec, Matchers}

trait Fmap[F[_]] {
  def fmap[A, B]: (A ⇒ B) ⇒ F[A] ⇒ F[B]
}

trait FPoint[F[_]] {
  def fpoint[A]: A ⇒ F[A]
}

trait FflatMap[F[_]] {
  def fflatMap[A, B]: (A ⇒ F[B]) ⇒ F[A] ⇒ F[B]
}

trait LawChecking extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  def fEqual[A: Arbitrary, B](f1: A ⇒ B, f2: A ⇒ B): Assertion = {
    forAll { (x: A) ⇒ f1(x) shouldEqual f2(x) }
  }

  private def checkFunctionEquality[A: Arbitrary, B](f1: A ⇒ B, f2: A ⇒ B)(implicit resultsEqual: (B, B) ⇒ Assertion): Assertion = {
    forAll { (x: A) ⇒ resultsEqual(f1(x), f2(x)) }
  }

  // Check equality for higher-order functions of type A ⇒ B ⇒ C.
  def hofEqual[A: Arbitrary, B: Arbitrary, C: Arbitrary](f1: A ⇒ B ⇒ C, f2: A ⇒ B ⇒ C): Assertion =
    checkFunctionEquality[A, B ⇒ C](f1, f2)(implicitly[Arbitrary[A]], (x: B ⇒ C, y: B ⇒ C) ⇒ fEqual(x, y))


  def fmapLawIdentity[A: Arbitrary, F[_]](fmap: Fmap[F])(implicit fResultsEqual: (F[A], F[A]) ⇒ Assertion, ev: Arbitrary[F[A]]): Assertion = {
    checkFunctionEquality[F[A], F[A]](fmap.fmap(identity[A]), identity[F[A]])
  }

  def fmapLawComposition[A: Arbitrary, B: Arbitrary, C: Arbitrary, F[_]](fmap: Fmap[F])(implicit fResultsEqual: (F[C], F[C]) ⇒ Assertion, evA: Arbitrary[F[A]], evB: Arbitrary[F[B]], evAB: Arbitrary[A ⇒ B], evBC: Arbitrary[B ⇒ C]): Assertion = {
    forAll { (f: A ⇒ B, g: B ⇒ C) ⇒
      checkFunctionEquality[F[A], F[C]](fmap.fmap(f) andThen fmap.fmap(g), fmap.fmap(f andThen g))
    }
  }

  def fmapPointLaw[A: Arbitrary, B: Arbitrary, F[_]](point: FPoint[F], fmap: Fmap[F])(implicit fResultsEqual: (F[B], F[B]) ⇒ Assertion, evAB: Arbitrary[A ⇒ B]): Assertion = forAll { (f: A ⇒ B) ⇒
    val point_dot_map = point.fpoint andThen fmap.fmap(f)
    val f_dot_point = f andThen point.fpoint
    checkFunctionEquality[A, F[B]](point_dot_map, f_dot_point)
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

    implicit def readersEqual[A](r1: Reader[A], r2: Reader[A]): Assertion = forAll { x: Int ⇒ r1(x) shouldEqual r2(x) }

    val fmapR = new Fmap[Reader] {
      override def fmap[A, B]: (A ⇒ B) ⇒ (Reader[A]) ⇒ Reader[B] = fmapReader[Int, A, B]
    }

    val pointR = new FPoint[Reader] {
      override def fpoint[A]: A => Reader[A] = pointReader[Int, A]
    }

    // Check identity law with E = Int and A = String.
    fmapLawIdentity[String, Reader](fmapR)

    fmapLawComposition[Boolean, Long, String, Reader](fmapR)

    fmapPointLaw[Long, String, Reader](pointR, fmapR)

    // Same check, using the helper methods.
    hofEqual((reader: Int ⇒ String) ⇒ mapReader(reader)(identity[String]), identity[Int ⇒ String])

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
