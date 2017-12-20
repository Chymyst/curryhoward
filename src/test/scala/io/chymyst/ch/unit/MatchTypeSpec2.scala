package io.chymyst.ch.unit

import io.chymyst.ch.CurryHowardMacros.testType
import org.scalatest.{FlatSpec, Matchers}

class MatchTypeSpec2 extends FlatSpec with Matchers {

  behavior of "tuples"

  it should "get type with argument tuples" in {
    def result[A, B]: (String, String) = testType[A ⇒ ((A, B)) ⇒ A]

    var r = result

    r._2 shouldEqual "(<c>String, <c>String)"
    r._1 shouldEqual "(A) → ((A, B)) → A"
  }

  it should "get type with nested argument tuples" in {
    def result[A, B]: (String, String) = testType[(((A, B)) ⇒ A) ⇒ A]

    var r = result

    r._2 shouldEqual "(<c>String, <c>String)"
    r._1 shouldEqual "(((A, B)) → A) → A"
  }

  it should "get a complicated type with argument tuples" in {
    def result[S, A, B]: (String, String) = testType[(S ⇒ (A, S)) ⇒ (((A, S)) ⇒ (B, S)) ⇒ (S ⇒ (B, S))]

    var r = result

    r._2 shouldEqual "(<c>String, <c>String)"
    r._1 shouldEqual "((S) → (A, S)) → (((A, S)) → (B, S)) → (S) → (B, S)"
  }

  behavior of "other syntax"

  it should "get type of conventional syntax for function" in {
    def result[A, B, C](x: A, y: B)(z: (C, C))(implicit t: Int): (String, String) = testType[A ⇒ C]

    val r = result(0, 0)((0, 0))(0)
    r._1 shouldEqual "(A) → C"
    r._2 shouldEqual "<tc>[A, B, C](x: A, y: B)(z: (C, C))(implicit t: Int)(String, String)"
  }
}
