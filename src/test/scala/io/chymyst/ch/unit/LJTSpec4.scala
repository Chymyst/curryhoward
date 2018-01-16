package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class LJTSpec4 extends FlatSpec with Matchers {

  // Playground for easier debugging.
  behavior of "misc. tests"

  it should "generate a pointed instance for infinite sum despite different type parameter names" in {
    sealed trait InfiniteSum[T]
    final case class Element[U](t: U) extends InfiniteSum[U]
    final case class RecursiveSum[V](s: InfiniteSum[V]) extends InfiniteSum[V]


    Macros.testReifyType[InfiniteSum[Int]]
/*
        val is = ofType[Int ⇒ InfiniteSum[Int]]

        is(123) should matchPattern { case Element(_) => }

        def isT[A] = ofType[A ⇒ InfiniteSum[A]]

        isT("abc") shouldEqual Element("abc")
*/
  }
  it should "work" in {

  }
}
