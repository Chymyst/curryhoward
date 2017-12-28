package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class LJTSpec4 extends FlatSpec with Matchers {

  // Playground for easier debugging.
  behavior of "misc. tests"

  it should "work" in {

    case class OOption[A](x: Option[Option[A]])

    val A = "A"
    val B = "B"
    val value = "value"
    val x = "x"

    def result2[B] = Macros.testReifyType[OOption[B]]

    result2 shouldEqual NamedConjunctT("OOption", List(TP(B)), List(x), List(DisjunctT("Option", List(DisjunctT("Option", List(TP(B)), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(TP(B)), List(value), List(TP(B)))))), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(DisjunctT("Option", List(TP(B)), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(TP(B)), List(value), List(TP(B)))))), List(value), List(DisjunctT("Option", List(TP(B)), List(NamedConjunctT("None", List(), List(), List()), NamedConjunctT("Some", List(TP(B)), List(value), List(TP(B)))))))))))


  }
}
