package io.chymyst.ch.unit

import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch._

class OrderingSpec extends FlatSpec with Matchers {

  behavior of "ordering heuristics"

  it should "generate correct code for functions with coincident types" in {
    def f1[A](x: (A, A, A, A)): (A, A, A, A) = implement

    case class P[T](x: T, y: T, z: T)

    case class Q[T](x: T, y: T)
    def f2: P[Int] ⇒ (Int ⇒ Double) ⇒ Q[Double] = implement
//    def f2(p: P[Int], f: Int ⇒ Double): Q[Double] = implement
  }
}
