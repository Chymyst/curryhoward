package io.chymyst.ch.unit

import io.chymyst.ch.implement
import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait FMap[F[_]] {
  def f[A, B]: (A ⇒ B) ⇒ F[A] ⇒ F[B]
}

trait FPoint[F[_]] {
  def f[A]: A ⇒ F[A]
}

trait FFlatMap[F[_]] {
  def f[A, B]: (A ⇒ F[B]) ⇒ F[A] ⇒ F[B]
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


  def fmapLawIdentity[A: Arbitrary, F[_]](fmap: FMap[F])(implicit fResultsEqual: (F[A], F[A]) ⇒ Assertion, ev: Arbitrary[F[A]]): Assertion = {
    checkFunctionEquality[F[A], F[A]](fmap.f(identity[A]), identity[F[A]])
  }

  def fmapLawComposition[A: Arbitrary, B: Arbitrary, C: Arbitrary, F[_]](fmap: FMap[F])(implicit fResultsEqual: (F[C], F[C]) ⇒ Assertion, evA: Arbitrary[F[A]], evAB: Arbitrary[A ⇒ B], evBC: Arbitrary[B ⇒ C]): Assertion = {
    forAll { (f: A ⇒ B, g: B ⇒ C) ⇒
      checkFunctionEquality[F[A], F[C]](fmap.f(f) andThen fmap.f(g), fmap.f(f andThen g))
    }
  }

  def fmapPointLaw[A: Arbitrary, B: Arbitrary, F[_]](point: FPoint[F], fmap: FMap[F])(implicit fResultsEqual: (F[B], F[B]) ⇒ Assertion, evAB: Arbitrary[A ⇒ B]): Assertion = forAll { (f: A ⇒ B) ⇒
    val point_dot_map = point.f andThen fmap.f(f)
    val f_dot_point = f andThen point.f
    checkFunctionEquality[A, F[B]](point_dot_map, f_dot_point)
  }

  def flatmapPointLaw[A: Arbitrary, B: Arbitrary, F[_]](point: FPoint[F], flatmap: FFlatMap[F])(implicit fResultsEqual: (F[B], F[B]) ⇒ Assertion, evAB: Arbitrary[A ⇒ F[B]], evFB: Arbitrary[F[B]]): Assertion = forAll { (f: A ⇒ F[B]) ⇒
    checkFunctionEquality[F[B], F[B]](flatmap.f(point.f), identity)
    checkFunctionEquality(point.f andThen flatmap.f(f), f)
  }

  def flatmapAssocLaw[A: Arbitrary, B: Arbitrary, C: Arbitrary, F[_]](fflatMap: FFlatMap[F])(implicit fResultsEqual: (F[C], F[C]) ⇒ Assertion, evFA: Arbitrary[F[A]], evAB: Arbitrary[A ⇒ F[B]], evBC: Arbitrary[B ⇒ F[C]]): Assertion = forAll { (f: A ⇒ F[B], g: B ⇒ F[C]) ⇒
    val x = fflatMap.f(f) andThen fflatMap.f(g)
    val y = fflatMap.f((x: A) ⇒ fflatMap.f(g)(f(x)))
    checkFunctionEquality[F[A], F[C]](x, y)
  }

  def flip[A, B, C]: (A ⇒ B ⇒ C) ⇒ (B ⇒ A ⇒ C) = implement
}
