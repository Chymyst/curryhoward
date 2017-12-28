package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class LJTSpec4 extends FlatSpec with Matchers {

  // Playground for easier debugging.
  behavior of "misc. tests"

  it should "compile" in {
    def flatmap[D, A, B] = ofType[((Option[A] ⇒ D) ⇒ Option[A]) ⇒ (A ⇒ ((Option[B] ⇒ D) ⇒ Option[B])) ⇒ ((Option[B] ⇒ D) ⇒ Option[B])]()
  }
}
