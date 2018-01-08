package io.chymyst.ch.unit

import io.chymyst.ch.implement
import org.scalatest.{FlatSpec, Matchers}

class RecursiveTypesSpec extends FlatSpec with Matchers {

  behavior of "recursive types"

  it should "generate identity function for List" in {
    def f[A]: List[A] ⇒ List[A] = implement

    f(List(1, 2, 3)) shouldEqual List(1, 2, 3)
  }
}
