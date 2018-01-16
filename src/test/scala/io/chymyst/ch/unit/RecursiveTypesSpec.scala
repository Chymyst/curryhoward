package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class RecursiveTypesSpec extends FlatSpec with Matchers {

  behavior of "recursive types"

  it should "generate identity function for List" in {
    def f[A]: List[A] ⇒ List[A] = implement

    f(List(1, 2, 3)) shouldEqual List(1, 2, 3)
  }

  it should "generate a pointed instance for infinite sum using equal type parameter names" in {
    sealed trait InfiniteSum[T]
    final case class Element[T](t: T) extends InfiniteSum[T]
    final case class RecursiveSum[V](s: InfiniteSum[V]) extends InfiniteSum[V]

    val is = ofType[Int ⇒ InfiniteSum[Int]]

    is(123) should matchPattern { case Element(_) => }

    def isT[A] = ofType[A ⇒ InfiniteSum[A]]

    isT("abc") shouldEqual Element("abc")
  }

  it should "generate a pointed instance for infinite sum despite different type parameter names" in {
    sealed trait InfiniteSum[T]
    final case class Element[U](t: U) extends InfiniteSum[U]
    final case class RecursiveSum[V](s: InfiniteSum[V]) extends InfiniteSum[V]
    /*
        val is = ofType[Int ⇒ InfiniteSum[Int]]

        is(123) should matchPattern { case Element(_) => }

        def isT[A] = ofType[A ⇒ InfiniteSum[A]]

        isT("abc") shouldEqual Element("abc")
    */
  }

  it should "generate an empty List" in {
        val is = ofType[Unit ⇒ List[Int]]

        is(()) shouldEqual Nil

        def isT[A] = ofType[Unit ⇒ List[A]]

        isT(()) shouldEqual Nil
  }

}
