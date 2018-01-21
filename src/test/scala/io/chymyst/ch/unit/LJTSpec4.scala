package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class LJTSpec4 extends FlatSpec with Matchers {

  // Playground for easier debugging.
  behavior of "misc. tests"

  it should "work" in {
    final case class Data2[X, A](g: X => A, x: A)

    final case class P[T](t: Option[T])

    final case class Data[A](d: Either[Data2[String, A], Data2[Boolean, A]])

    //    def t1[A, B] = Macros.testReifyType[(A ⇒ B) ⇒  Data[A] ⇒ Data[B]]
    //    println(t1[Int, String].toString)

    //    val x = allOfType[(Int => Double) => Data[Int] => Data[Double]]

    //    def fmap1[A, B]: (A ⇒ B) ⇒  Data[A] ⇒ Data[B] = implement

    // This does not work correctly. Is the problem in the theorem prover? It does not implement a correct functor instance for Reader.
    //    def fmap2[A, B]: (A ⇒ B) ⇒  Option[Data2[Int, A]] ⇒ Option[Data2[Int, B]] = implement

    //    def fmap2[A, B]: (A ⇒ B) ⇒  P[(Int => A, A)] ⇒ P[(Int => B, B)] = implement

    //    def fmap3[A, B]: (Int => A) ⇒ A => (A => B) ⇒  Option[Int => B] = implement

    def fmap4[A, B]: A => (A => B) ⇒ Option[A ⇒ B] = implement

    //    val res: Data[String] = fmap[Int, String](_.toString)(Data(Left(Data2(_ => 100, 200))))
    //
    //    res match {
    //      case Data(Left(Data2(f, g))) ⇒ (f(""), g) shouldEqual (("100", "200"))
    //    }
  }
}
