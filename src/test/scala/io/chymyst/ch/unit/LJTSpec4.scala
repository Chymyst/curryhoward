package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{Assertion, FlatSpec, Matchers}

class LJTSpec4 extends FlatSpec with Matchers {

  // Playground for easier debugging.
  behavior of "misc. tests"

  it should "work" in {
    type P[T] = (Option[T], Option[T])

//        def fmaps[A, B](f: A ⇒ B) = anyOfType[P[A] ⇒ P[B]](f)
//    println(s"got ${fmaps(identity[Int]).size} implementations")


    def flattens[A] = anyOfType[P[Option[A]] ⇒ P[A]]()

    // Is any of these implementations good?
    println(s"got ${flattens.size} implementations")

    flattens[Int].filter { f ⇒
      f((Some(Some(1)), Some(Some(2)))) == ((Some(1), Some(2))) &&
        f((Some(None), Some(Some(2)))) == ((None, Some(2))) &&
        f((Some(Some(1)), None)) == ((Some(1), None))
    } shouldEqual Seq(true)

    // TODO: make this work
    //    type P[T] = (Option[T], Option[T])
    //    def flatten[A]: P[Option[Int]] ⇒ P[Int] = implement
    //    flatten((Some(Some(1)), Some(Some(2)))) shouldEqual ((Some(1), Some(2)))
    //    flatten((Some(None), Some(Some(2)))) shouldEqual ((None, Some(2)))
    //    flatten((Some(Some(1)), None)) shouldEqual ((Some(1), None))
  }
}
