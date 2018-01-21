package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class LJTSpec4 extends FlatSpec with Matchers {

  // Playground for easier debugging.
  behavior of "misc. tests"

  it should "work" in {
    final case class Data[A, B](ab: Either[A, B], d: (A ⇒ Int) ⇒ B)

    // TODO: fix
    "def fmapB[Z, B, C](f: B ⇒ C): Data[Z, B] ⇒ Data[Z, C] = implement" shouldNot compile

    "def fmap[X, Y, B](f: X ⇒ Y): Data[X, B] ⇒ Data[Y, B] = implement" shouldNot compile
  }
}
