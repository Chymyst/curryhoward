package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class LJTSpec4 extends FlatSpec with Matchers {

  // Playground for easier debugging.
  behavior of "misc. tests"

  it should "work" in {

    def f[A, B]: (A => Option[B]) => Option[A] => Option[B] = implement

  }
}
