package io.chymyst.ch.unit

import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch._

class OrderingSpec extends FlatSpec with Matchers {

  behavior of "ordering heuristics"

  it should "generate correct code for conjunctions with coincident types" in {
    def f1[A](x: (A, A, A, A)): (A, A, A, A) = implement

    case class P[T](x: T, y: T, z: T)

    case class Q[T](x: T, y: T)

    allOfType[P[Int] ⇒ (Int ⇒ Double) ⇒ Q[Double]].length shouldEqual 9

    def f2: P[Int] ⇒ (Int ⇒ Double) ⇒ P[Double] = implement

    def f3(p: P[Int], f: Int ⇒ Double): P[Double] = implement // bug: should work too!
  }

  it should "generate correct code for disjunctions with coincident types" in {
    def f1[A](x: Either[A, A]): Either[A, A] = implement

    case class P[T](x: T, y: T, z: T)

    case class Q[T](x: T, y: T)

    "def f2a: Either[P[Int], Q[Int]] ⇒ Either[Q[Int], P[Int]] = implement" shouldNot compile

    def f2b[T] = allOfType[Either[P[T], Q[T]] ⇒ Either[Q[T], P[T]]]

    f2b[Int].length shouldEqual 6
  }

  it should "generate desired code for Option[Option[T]]" in {
    def f1(x: Option[Option[Int]]): Option[Option[Int]] = implement

    def f2[T](x: Option[Option[T]]): Option[Option[T]] = implement
  }
}
