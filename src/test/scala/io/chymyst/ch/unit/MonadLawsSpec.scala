package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, FlatSpec, Matchers}

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

class LawsSpec extends LawChecking {

  behavior of "generated type class methods"

  it should "check laws for Reader monad" in {
    def pointReader[E, A]: A ⇒ (E ⇒ A) = implement

    def mapReader[E, A, B]: (E ⇒ A) ⇒ (A ⇒ B) ⇒ (E ⇒ B) = implement

    def fmapReader[E, A, B]: (A ⇒ B) ⇒ (E ⇒ A) ⇒ (E ⇒ B) = flip(mapReader)

    // Check identity law with E = Int and A = String.
    forAll { (reader: Int ⇒ String) ⇒
      // fmap id = id
      fEqual(mapReader(reader)(identity[String]), reader)
    }

    type Reader[A] = Int ⇒ A

    implicit def readersEqual[A](r1: Reader[A], r2: Reader[A]): Assertion = forAll { x: Int ⇒ r1(x) shouldEqual r2(x) }

    // Same check, using the helper methods.
    hofEqual((reader: Int ⇒ String) ⇒ mapReader(reader)(identity[String]), identity[Int ⇒ String])

    def flatMapReader[E, A, B]: (E ⇒ A) ⇒ (A ⇒ E ⇒ B) ⇒ (E ⇒ B) = implement

    // r.flatMap(pure) = r
    forAll { (reader: Int ⇒ String) ⇒
      val flatMap_pure = flatMapReader(reader)(pointReader)
      fEqual(flatMap_pure, reader)
    }

    // Same checks using the universal helper method.

    val fmapR = new FMap[Reader] {
      override def f[A, B]: (A ⇒ B) ⇒ (Reader[A]) ⇒ Reader[B] = fmapReader[Int, A, B]
    }

    val pointR = new FPoint[Reader] {
      override def f[A]: A => Reader[A] = pointReader[Int, A]
    }

    val flatmapR = new FFlatMap[Reader] {
      override def f[A, B]: (A => Reader[B]) => (Reader[A]) => Reader[B] = flip(flatMapReader[Int, A, B])
    }

    checkMonadLaws[Int, Long, String, Reader](pointR, fmapR, flatmapR)
  }

  def checkMonadLaws[A: Arbitrary, B: Arbitrary, C: Arbitrary, F[_]](pointS: FPoint[F], fmapS: FMap[F], flatmapS: FFlatMap[F])(implicit frec: (F[C], F[C]) ⇒ Assertion, fab: Arbitrary[A ⇒ B], fac: Arbitrary[A ⇒ C], fa: Arbitrary[F[A]], fc: Arbitrary[F[C]], evBC: Arbitrary[B ⇒ C], evAfC: Arbitrary[A ⇒ F[C]], evAfB: Arbitrary[A ⇒ F[B]], evBfC: Arbitrary[B ⇒ F[C]]): Assertion = {

    fmapLawIdentity[C, F](fmapS)

    fmapLawComposition[A, B, C, F](fmapS)

    fmapPointLaw[A, C, F](pointS, fmapS)

    flatmapPointLaw[A, C, F](pointS, flatmapS)

    flatmapAssocLaw[A, B, C, F](flatmapS)
  }

  it should "check laws for State monad" in {
    case class State[S, A](st: S ⇒ (A, S))

    case class IntState[A](st: State[Int, A])

    implicit def intStateEqual[A](s1: IntState[A], s2: IntState[A]): Assertion = fEqual[Int, (A, Int)](s1.st.st, s2.st.st)

    implicit def genCaseClass[A: Arbitrary]: Arbitrary[IntState[A]] = Arbitrary {
      for {
        n <- arbitrary[Int ⇒ (A, Int)]
      } yield IntState(State(n))
    }


    val pointS = new FPoint[IntState] {
      override def f[A]: A => IntState[A] = implement
    }

    val fmapS = new FMap[IntState] {
      override def f[A, B]: (A => B) => IntState[A] => IntState[B] = implement
    }

    val flatmapS = new FFlatMap[IntState] {
      override def f[A, B]: (A => IntState[B]) => IntState[A] => IntState[B] = implement
    }

    checkMonadLaws[Int, Long, String, IntState](pointS, fmapS, flatmapS)
  }

  it should "check laws for Option monad" in {
    implicit def optionEqual[A](s1: Option[A], s2: Option[A]): Assertion = s1 shouldEqual s2

    val pointS = new FPoint[Option] {
      override def f[A]: A => Option[A] = implement
    }

    val fmapS = new FMap[Option] {
      override def f[A, B]: (A => B) => Option[A] => Option[B] = implement
    }

    val flatmapS = new FFlatMap[Option] {
      override def f[A, B]: (A => Option[B]) => Option[A] => Option[B] = implement
    }

    checkMonadLaws[Int, Long, String, Option](pointS, fmapS, flatmapS)

  }

  it should "check laws for Continuation monad" in {
    case class Cont[X](c: (X ⇒ Int) ⇒ Int)

    implicit def contEqual[A: Arbitrary](s1: Cont[A], s2: Cont[A])(implicit ai: Arbitrary[A ⇒ Int]): Assertion = fEqual(s1.c, s2.c)

    implicit def genCaseClass[A: Arbitrary]: Arbitrary[Cont[A]] = Arbitrary {
      for {
        n <- arbitrary[(A ⇒ Int) ⇒ Int]
      } yield Cont(n)
    }

    val pointS = new FPoint[Cont] {
      override def f[A]: A => Cont[A] = implement
    }

    val fmapS = new FMap[Cont] {
      override def f[A, B]: (A => B) => Cont[A] => Cont[B] = implement
    }

    val flatmapS = new FFlatMap[Cont] {
      override def f[A, B]: (A => Cont[B]) => Cont[A] => Cont[B] = implement
    }

    checkMonadLaws[Int, Long, String, Cont](pointS, fmapS, flatmapS)
  }

  it should "check laws for Density monad" in {
    case class Dens[X](c: (X ⇒ Int) ⇒ X)

    implicit def contEqual[A: Arbitrary](s1: Dens[A], s2: Dens[A])(implicit ai: Arbitrary[A ⇒ Int]): Assertion = fEqual(s1.c, s2.c)

    implicit def genCaseClass[A: Arbitrary]: Arbitrary[Dens[A]] = Arbitrary {
      for {
        n <- arbitrary[(A ⇒ Int) ⇒ A]
      } yield Dens(n)
    }

    val pointS = new FPoint[Dens] {
      override def f[A]: A => Dens[A] = implement
    }

    val fmapS = new FMap[Dens] {
      override def f[A, B]: (A => B) => Dens[A] => Dens[B] = implement
    }

    val flatmapS = new FFlatMap[Dens] {
      override def f[A, B]: (A => Dens[B]) => Dens[A] => Dens[B] = implement
    }

    checkMonadLaws[Int, Long, String, Dens](pointS, fmapS, flatmapS)
  }

  it should "fail to implement Option[Option] monad due to ambiguities" in {
    case class OOption[A](x: Option[Option[A]])

    val pointS = new FPoint[OOption] {
      override def f[A]: A => OOption[A] = implement // This works.
    }

    // Here we have 4 implementations, and there is no good heuristic to use so far that can choose the correct functor instance.
    def maps[A, B] = allOfType[(A => B) => OOption[A] => OOption[B]]

    // All implementations should transform a non-empty option correctly.
    val mapsIntString = maps[Int, String]
    mapsIntString.length shouldEqual 1
    mapsIntString.foreach { m ⇒
      m(_.toString + "abc")(OOption(Some(Some(123)))) shouldEqual OOption(Some(Some("123abc")))
    }

    def flatmaps[A, B] = allOfType[(A => OOption[B]) => OOption[A] => OOption[B]]

    val flatmapsIntString = flatmaps[Int, String]
    flatmapsIntString.length shouldEqual 12


    /*    val fmapS = new FMap[OOption] {
          override def f[A, B]: (A => B) => OOption[A] => OOption[B] = implement
        }

        val flatmapS = new FFlatMap[OOption] {
          override def f[A, B]: (A => OOption[B]) => OOption[A] => OOption[B] = implement
        }

        checkMonadLaws[Int, Long, String, OOption](pointS, fmapS, flatmapS)
    */
  }

  it should "check functor laws for the worked example 1.3 from chapter 4, part 1" in {
    // Data[A] ≡ 1 + A × (Int × String + A)
    final case class Data[A](d: Option[(A, Data2[A])]) //  1 + A × Data2[A]

    sealed trait Data2[+A]
    final case class Message[A](code: Int, message: String) extends Data2[A]
    final case class Value[A](x: A) extends Data2[A]

    val fmap: FMap[Data] = new FMap[Data] {
      override def f[A, B]: (A => B) => Data[A] => Data[B] = implement
    }

    def fmaps[A, B] = allOfType[(A ⇒ B) ⇒ Data[A] ⇒ Data[B]]

    fmaps.length shouldEqual 1
  }
}
