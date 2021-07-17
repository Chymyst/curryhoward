package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatest.Assertion
import org.scalacheck.ScalacheckShapeless._

class LawsSpec extends LawChecking {

  behavior of "generated type class methods"

  it should "check laws for Reader monad" in {
    def pointReader[E, A]: A => (E => A) = implement

    def mapReader[E, A, B]: (E => A) => (A => B) => (E => B) = implement

    def fmapReader[E, A, B]: (A => B) => (E => A) => (E => B) = flip(mapReader)

    // Check identity law with E = Int and A = String.
    forAll { (reader: Int => String) =>
      // fmap id = id
      fEqual(mapReader(reader)(identity[String]), reader)
    }

    type Reader[A] = Int => A

    implicit def readersEqual[A](r1: Reader[A], r2: Reader[A]): Assertion = forAll { x: Int => r1(x) shouldEqual r2(x) }

    // Same check, using the helper methods.
    hofEqual((reader: Int => String) => mapReader(reader)(identity[String]), identity[Int => String])

    def flatMapReader[E, A, B]: (E => A) => (A => E => B) => (E => B) = implement

    // r.flatMap(pure) = r
    forAll { (reader: Int => String) =>
      val flatMap_pure = flatMapReader(reader)(pointReader)
      fEqual(flatMap_pure, reader)
    }

    // Same checks using the universal helper method.

    val fmapR = new FMap[Reader] {
      override def f[A, B]: (A => B) => (Reader[A]) => Reader[B] = fmapReader[Int, A, B]
    }

    val pointR = new FPoint[Reader] {
      override def f[A]: A => Reader[A] = pointReader[Int, A]
    }

    val flatmapR = new FFlatMap[Reader] {
      override def f[A, B]: (A => Reader[B]) => (Reader[A]) => Reader[B] = flip(flatMapReader[Int, A, B])
    }

    checkMonadLaws[Int, Long, String, Reader](pointR, fmapR, flatmapR)
  }

  def checkMonadLaws[A: Arbitrary, B: Arbitrary, C: Arbitrary, F[_]](pointS: FPoint[F], fmapS: FMap[F], flatmapS: FFlatMap[F])(implicit frec: (F[C], F[C]) => Assertion, fab: Arbitrary[A => B], fac: Arbitrary[A => C], fa: Arbitrary[F[A]], fc: Arbitrary[F[C]], evBC: Arbitrary[B => C], evAfC: Arbitrary[A => F[C]], evAfB: Arbitrary[A => F[B]], evBfC: Arbitrary[B => F[C]]): Assertion = {

    fmapLawIdentity[C, F](fmapS)

    fmapLawComposition[A, B, C, F](fmapS)

    fmapPointLaw[A, C, F](pointS, fmapS)

    flatmapPointLaw[A, C, F](pointS, flatmapS)

    flatmapAssocLaw[A, B, C, F](flatmapS)
  }

  it should "check laws for State monad" in {
    case class State[S, A](st: S => (A, S))

    case class IntState[A](st: State[Int, A])

    implicit def intStateEqual[A](s1: IntState[A], s2: IntState[A]): Assertion = fEqual[Int, (A, Int)](s1.st.st, s2.st.st)

    implicit def genCaseClass[A: Arbitrary]: Arbitrary[IntState[A]] = Arbitrary {
      for {
        n <- arbitrary[Int => (A, Int)]
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
    case class Cont[X](c: (X => Int) => Int)

    implicit def contEqual[A: Arbitrary](s1: Cont[A], s2: Cont[A])(implicit ai: Arbitrary[A => Int]): Assertion = fEqual(s1.c, s2.c)

    implicit def genCaseClass[A: Arbitrary]: Arbitrary[Cont[A]] = Arbitrary {
      for {
        n <- arbitrary[(A => Int) => Int]
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

  it should "check laws for Center-of-mass monad" in {
    case class CenterOfMass[X](c: (X => Int) => X)

    implicit def contEqual[A: Arbitrary](s1: CenterOfMass[A], s2: CenterOfMass[A])(implicit ai: Arbitrary[A => Int]): Assertion = fEqual(s1.c, s2.c)

    implicit def genCaseClass[A: Arbitrary]: Arbitrary[CenterOfMass[A]] = Arbitrary {
      for {
        n <- arbitrary[(A => Int) => A]
      } yield CenterOfMass(n)
    }

    val pointS = new FPoint[CenterOfMass] {
      override def f[A]: A => CenterOfMass[A] = implement
    }

    val fmapS = new FMap[CenterOfMass] {
      override def f[A, B]: (A => B) => CenterOfMass[A] => CenterOfMass[B] = implement
    }

    val flatmapS = new FFlatMap[CenterOfMass] {
      override def f[A, B]: (A => CenterOfMass[B]) => CenterOfMass[A] => CenterOfMass[B] = implement
    }

    checkMonadLaws[Int, Long, String, CenterOfMass](pointS, fmapS, flatmapS)
  }

  it should "implement Option[Option] monad and check laws" in {
    case class OOption[A](x: Option[Option[A]])

    val pointS = new FPoint[OOption] {
      override def f[A]: A => OOption[A] = implement // This works.
    }

    // Here we have 4 implementations, and there is no good heuristic to use so far that can choose the correct functor instance.
    def maps[A, B] = allOfType[(A => B) => OOption[A] => OOption[B]]

    // All implementations should transform a non-empty option correctly.
    maps.length shouldEqual 1
    val mapsIntString = maps[Int, String]
    mapsIntString.foreach { m =>
      m(_.toString + "abc")(OOption(Some(Some(123)))) shouldEqual OOption(Some(Some("123abc")))
    }

    val fmapS = new FMap[OOption] {
      override def f[A, B]: (A => B) => OOption[A] => OOption[B] = implement
    }

    def flatmaps[A, B] = allOfType[(A => OOption[B]) => OOption[A] => OOption[B]]

    flatmaps.length shouldEqual 1


    val flatmapS = new FFlatMap[OOption] {
      override def f[A, B]: (A => OOption[B]) => OOption[A] => OOption[B] = implement
    }

    implicit def ooptEqual[T](x: OOption[T], y: OOption[T]): Assertion = x shouldEqual y

    checkMonadLaws[Int, Long, String, OOption](pointS, fmapS, flatmapS)
  }

  it should "fail to check functor laws for the worked example 1.3 from chapter 4, part 1, due to ambiguities" in {
    // Data[A] ≡ 1 + A × (Int × String + A)
    final case class Data[A](d: Option[(A, Data2[A])]) //  1 + A × Data2[A]

    sealed trait Data2[+A]
    final case class Message[A](code: Int, message: String) extends Data2[A]
    final case class Value[A](x: A) extends Data2[A]
/* This does not work due to ambiguity of d.get._1 and Value.x
    val fmap: FMap[Data] = new FMap[Data] {
      override def f[A, B]: (A => B) => Data[A] => Data[B] = implement
    }
    implicit def dataEqual[T](x: Data[T], y: Data[T]): Assertion = x shouldEqual y

    fmapLawIdentity[Int, Data](fmap)
    fmapLawComposition[Int, String, Boolean, Data](fmap)

//    fmap.f[Int, Int](x => x + 1)(Data(Some((123, Value(456))))) shouldEqual Data(Some((124, Value(457))))
//    fmap.f[Int, Int](x => x + 1)(Data(Some((123, Message(10, "20"))))) shouldEqual Data(Some((124, Message(10, "20"))))
*/
    def fmaps[A, B] = allOfType[(A => B) => Data[A] => Data[B]]

    fmaps.length shouldEqual 2
  }

}
