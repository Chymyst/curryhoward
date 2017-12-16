package io.chymyst.ch.unit

import io.chymyst.ch.CurryHowardMacros.testType
import org.scalatest.{FlatSpec, Matchers}

class MatchTypeSpec2 extends FlatSpec with Matchers {

  behavior of "other syntax"

  it should "get type of conventional syntax for function" in {
    def result[A, B, C](x: A, y: B)(z: (C, C))(implicit t: Int): (String, String) = testType[A ⇒ C]

    val r = result(0, 0)((0, 0))(0)
    r._1 shouldEqual "(A) → C"
    r._2 shouldEqual ""
  }
}
