package io.chymyst.ch.unit

import io.chymyst.ch._

class ApiSpec extends LawChecking {

  behavior of "left-hand-side type introspection"

  it should "detect the type of function on the LHS" in {
    def f1[X](x: X): X = implement

    def f2[X]: X ⇒ X = implement

    fEqual(f1[Int], f2[Int])
  }

  behavior of "syntax of `implement` and `ofType`"

  it should "compile" in {
    def f1[X, Y]: X ⇒ Y ⇒ X = implement

    // This does not work because `implement` needs to access the type of the enclosing owner!
    // The compiler error is "recursive method f2 needs type".
    " def f2a[X, Y] = implement[X ⇒ Y ⇒ X] " shouldNot compile

    def f2[X, Y] = ofType[X ⇒ Y ⇒ X]

    // does not work because ofType[Nothing] is instantiated: the macro does not use the enclosing owner.
    " def f3[X, Y]: X ⇒ Y ⇒ X = ofType " shouldNot compile
  }

  it should "generate correct code for the identity function with `ofType[]` syntax" in {
    def f1[X] = ofType[X ⇒ X]

    f1(123) shouldEqual 123
    f1("abc") shouldEqual "abc"
    f1(true) shouldEqual true
  }

  it should "generate correct code for the const function with `ofType[]` syntax" in {
    def f2[X, Y] = ofType[X ⇒ Y ⇒ X]

    val cTrue = f2(true)
    cTrue(123) shouldEqual true
    cTrue("abc") shouldEqual true
    cTrue(true) shouldEqual true

    val c3 = f2(3)
    c3(123) shouldEqual 3
    c3("abc") shouldEqual 3
    c3(true) shouldEqual 3
  }

  it should "generate correct code for the permuted const function with `ofType[]` syntax" in {
    def f2[X, Y]: X ⇒ Y ⇒ Y = implement

    f2(123)("true") shouldEqual "true"
    f2(false)(1.0) shouldEqual 1.0
  }

  it should "generate correct code for the identity function with standard syntax" in {
    def f1[X]: X ⇒ X = implement

    f1(123) shouldEqual 123
    f1("abc") shouldEqual "abc"
    f1(true) shouldEqual true
  }

  it should "generate correct code for the const function with standard syntax" in {
    def f2[X, Y]: X ⇒ Y ⇒ X = implement

    val cTrue = f2(true)
    cTrue(123) shouldEqual true
    cTrue("abc") shouldEqual true
    cTrue(true) shouldEqual true

    val c3 = f2(3)
    c3(123) shouldEqual 3
    c3("abc") shouldEqual 3
    c3(true) shouldEqual 3
  }

  it should "fail to compile when two possible implementations are equally good" in {
    "def f1[X, A, B]: X ⇒ A ⇒ X ⇒ X = implement" shouldNot compile

    def f1[X, A, B] = allOfType[X ⇒ A ⇒ X ⇒ X]

    f1[Int, String, Boolean].length shouldEqual 2
  }

  it should "generate correct code for the const function with extra unused arguments" in {
    def f1[X, A, B]: X ⇒ A ⇒ B ⇒ X = implement

    f1(123)("q")(true) shouldEqual 123
    f1("abc")(Some((1, 1)))(Map()) shouldEqual "abc"
    f1(true)(123.0)('blah) shouldEqual true
  }

  it should "generate correct code for the identity function on a=>b" in {
    def f2[X, Y]: (X ⇒ Y) ⇒ X ⇒ Y = implement

    val printInt: Int ⇒ String = _.toString

    f2(printInt)(123) shouldEqual "123"
  }

  it should "generate correct code for the const function with more unused arguments of coincident type" in {
    def f1[X, A, B]: A ⇒ X ⇒ A ⇒ B ⇒ X = implement

    f1("b")(123)("q")(true) shouldEqual 123
    f1(Some((3, 4)))("abc")(Some((1, 1)))(Map()) shouldEqual "abc"
    f1(0.0)(true)(123.0)('blah) shouldEqual true
  }

  it should "generate correct code for the identity function with explicit arguments" in {
    def f1[X](x: X): X = implement

    f1(123) shouldEqual 123
    f1("abc") shouldEqual "abc"
    f1(true) shouldEqual true
  }

  behavior of "ofType and allOfType"

  it should "find implementation with given arguments" in {
    val x = ofType[(Int, String)](123, (x: Int) ⇒ x.toString + "abc")
    x shouldEqual ((123, "123abc"))

    ofType[(Int, Int, String)](123, (x: Int) ⇒ (b: Boolean) ⇒ x.toString + b.toString, false) shouldEqual ((123, 123, "123false"))
  }

  it should "find all implementations with given arguments when there is one implementation" in {
    val x = allOfType[(Int, String)](123, (x: Int) ⇒ x.toString + "abc")
    x.length shouldEqual 1
  }

  it should "find all implementations with given arguments when there is more than one implementation" in {
    allOfType[(Int, String)](123, 456, (x: Int) ⇒ x.toString + "abc").length shouldEqual 2
  }

  it should "run the example in the tutorial" in {
    ofType[Option[Int] ⇒ Option[Option[Int]]] // should print alternative terms

    val fs = allOfType[Option[Int] ⇒ Option[Option[Int]]]
    fs.map(f ⇒ f(None)) shouldEqual List(None)
    fs.map(f ⇒ f(Some(123))) shouldEqual List(Some(Some(123)))

  }

  it should "find implementation of tuple with identity functions" in {
    val f = ofType[(Int ⇒ Int, String ⇒ String)]

    f._1(123) shouldEqual 123
    f._2("abc") shouldEqual "abc"
  }

  behavior of "java-style functions"

  it should "handle function with zero arguments" in {
    // TODO: fix this
    "val f = ofType[Int ⇒ () ⇒ Int]" shouldNot compile
//    f(123)() shouldEqual 123
  }
}
