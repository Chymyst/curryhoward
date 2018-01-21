package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class LJTSpec4 extends FlatSpec with Matchers {

  // Playground for easier debugging.
  behavior of "misc. tests"

  it should "work" in {
    final case class P(x: Int, y: Int)
    val exprs = allOfType[((Int, Int)) ⇒ (Int, Int)]
    val exprs2 = allOfType[P ⇒ P]

    def exprs3[A] = ofType[((A, A)) ⇒ (A, A)]
  }
}
