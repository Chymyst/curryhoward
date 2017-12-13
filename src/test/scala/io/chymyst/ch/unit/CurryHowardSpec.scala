package io.chymyst.ch.unit

import io.chymyst.ch.LJT._
import io.chymyst.ch.CurryHowardMacros._
import io.chymyst.ch._
import io.chymyst.ch.unit.Subformulas._
import org.scalatest.{FlatSpec, Matchers}

// This is now only for testing. Subformula property may not hold in a further iteration of the theorem prover,
// and it is not bringing an obvious advantage to the implementation.
object Subformulas {
  private val freshSubformulas = new FreshIdents(prefix = "f")

  def subformulas[T](typeStructure: TypeExpr[T]): Set[TypeExpr[T]] = Set(typeStructure) ++ (typeStructure match {
    case DisjunctT(terms) ⇒ terms.flatMap(subformulas)
    case ConjunctT(terms) ⇒ terms.flatMap(subformulas)
    case head :-> body ⇒ subformulas(head) ++ subformulas(body) ++ (head match {
      case DisjunctT(terms) ⇒ terms.flatMap(t ⇒ subformulas(:->(t, body)))
      case ConjunctT(terms) ⇒ subformulas(terms.foldRight(body) { case (t, prev) ⇒ t :-> prev })
      case _ :-> bd ⇒ subformulas(bd :-> body) // Special subformula case for implication of the form (hd ⇒ bd) ⇒ body
      case _ ⇒ Seq() // `head` is an atomic type
    })
    case _ ⇒ Seq() // `typeStructure` is an atomic type
  }).toSet

}

class CurryHowardSpec extends FlatSpec with Matchers {

  behavior of "syntax for untyped functions"

  it should "define syntax for untyped lambda calculus with products" in {
    // This is for testing only. We need to represent terms by a polynomial type, not by functions.
    sealed trait Term {
      def apply[T](x: T): Term = this // dummy implementation to simplify code
    }

    final case class \[T](v: Term ⇒ T) extends Term

    final case class \:[T](v: Any ⇒ T) extends Term

    "(x => x)" shouldNot compile

    val a1 = \(x ⇒ \(y ⇒ x))

    def f1[X, Y]: X => Y => X = x => y => x

    val a2 = \(x ⇒ \(y ⇒ x(y)))
    val a3 = \(x ⇒ \(y ⇒ y((x, x))))
    val a4 = \(x ⇒ \(y ⇒ \(z ⇒ x(y(z)))))
    val a5 = \(x ⇒ \(y ⇒ \(z ⇒ z(x(y)))))
    val a6 = \(x ⇒ (x, \(y ⇒ (x, y, x(y))))) // can use tuples
    val qqq = \ { x ⇒ x() } // can apply to unit
    val a7 = \: { case (x: Term, y: Term) ⇒ x(y(x)) } // use tuples as argument types, need annotations
  }

  behavior of "type parameter introspection"

  it should "get printable representation of fixed types with _" in {
    def result[A, B, C]: (String, String) = testType[_ ⇒ B]

    val res = result._1
    res shouldEqual "(<other>_) ..=>.. <tparam>B"
  }

  it should "get printable representation of enclosing owner's type" in {
    def result[A, B, C]: (String, String) = testType[Int]

    result._2 shouldEqual "(<basic>String, <basic>String)"
  }

  it should "get printable representation of enclosing owner's type with function syntax" in {
    def result[A, B, C](x: A, y: B)(z: C): (String, String) = testType
    // TODO: implement detection of types on the LHS
    result[Int, Boolean, Double](123, true)(1.0)._2 shouldEqual "<constructor>[A, B, C](x: A, y: B)(z: C)(String, String)"
  }

  it should "get printable representation of basic types" in {
    def result[A, B, C]: (String, String) = testType[Int]

    result._1 shouldEqual "<basic>Int"
  }

  it should "get printable representation of parametric type" in {
    def result[A, B, C]: (String, String) = testType[A]

    result._1 shouldEqual "<tparam>A"
  }

  it should "get printable representation of function types" in {
    def result[A, B, C]: (String, String) = testType[A ⇒ B]

    result._1 shouldEqual "(<tparam>A) ..=>.. <tparam>B"
  }

  it should "get printable representation of fixed types with type constructors" in {
    def result[A, B, C]: (String, String) = testType[Option[Seq[Int]] ⇒ Option[List[Set[A]]] ⇒ B]

    result._1 shouldEqual "(1 + <constructor>Seq[Int]) ..=>.. (1 + <constructor>List[Set[A]]) ..=>.. <tparam>B"
  }

  it should "get printable representation of fixed types with type constructors with [_]" in {
    def result[A, B, C]: (String, String) = testType[Option[_] ⇒ B]

    val res = result._1
    res shouldEqual "(1 + <other>_) ..=>.. <tparam>B"
  }

  it should "get printable representation of Option types" in {
    def result[A, B, C]: (String, String) = testType[Option[A] ⇒ Either[A, B]]

    result._1 shouldEqual "(1 + <tparam>A) ..=>.. <tparam>A + <tparam>B"
  }

  it should "get printable representation of Any, Unit, and Nothing types" in {
    def result[A, B, C]: (String, String) = testType[Any ⇒ Nothing ⇒ Unit]

    result._1 shouldEqual "(<other>_) ..=>.. (0) ..=>.. 1"
  }

  it should "not confuse a type parameter with a type inheriting from Any" in {
    class Q

    def result[A, B, C]: (String, String) = testType[A ⇒ Q]

    result._1 shouldEqual "(<tparam>A) ..=>.. <other>Q"
  }

  it should "get printable representation of tuple types" in {
    def result[A, B, C]: (String, String) = testType[(Any, Nothing, Unit, A, B, C)]

    result._1 shouldEqual "(<other>_, 0, 1, <tparam>A, <tparam>B, <tparam>C)"
  }

  it should "get printable representation of tuple as function argument" in {
    def result[A, B, C]: (String, String) = testType[((A, B)) ⇒ C]

    result._1 shouldEqual "((<tparam>A, <tparam>B)) ..=>.. <tparam>C"
  }

  it should "get printable representation of tuple of basic types" in {
    def result[A, B, C]: (String, String) = testType[(Int, String, Boolean, Float, Double, Long, Symbol, Char)]

    result._1 shouldEqual "(" + CurryHowardMacros.basicTypes.map("<basic>" + _).mkString(", ") + ")"
  }

  it should "get printable representation of case class" in {
    sealed trait Test1[T]
    case class A[T](x: Int, y: T) extends Test1[T]
    case class B(y: String) extends Test1[String]

    sealed abstract class Test2
    case class C(x: Int)(y: Double) extends Test2
    case class D(y: String) extends Test2

    def result[T]: (String, String) = testType[Test1[T] ⇒ Test2]

    result._1 shouldEqual "(<constructor>Test1[T]) ..=>.. <other>Test2"
  }

  behavior of "syntax of `inhabit`"

  it should "compile" in {
    def f1[X, Y]: X ⇒ Y ⇒ X = implement

    // This does not work because `inhabit` needs to access the type of the enclosing owner!
    // The compiler error is "recursive method f2 needs type".
    " def f2a[X, Y] = inhabit[X ⇒ Y ⇒ X] " shouldNot compile

    def f2[X, Y] = ofType[X ⇒ Y ⇒ X]

    // does not work because ofType[Nothing] is instantiated: the macro does not use the enclosing owner.
    " def f3[X, Y]: X ⇒ Y ⇒ X = ofType " shouldNot compile
  }

  it should "get the list of propositions" in {
    TermExpr.propositions(CurriedE(List(PropE("A", TP("A"))), AppE(PropE("A", TP("A")), PropE("B", TP("B"))))) shouldEqual Set(PropE("A", TP("A")), PropE("B", TP("B")))
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
  }

  it should "generate correct code for the const function with extra unused arguments" in {
    def f1[X, A, B]: X ⇒ A ⇒ B ⇒ X = implement

    f1(123)("q")(true) shouldEqual 123
    f1("abc")(Some((1, 1)))(Map()) shouldEqual "abc"
    f1(true)(123.0)('blah) shouldEqual true
  }

  // TODO: make this work
    it should "generate correct code for the identity function on a=>b" in {
      "def f2[X, Y]: (X ⇒ Y) ⇒ X ⇒ Y = implement" shouldNot compile
  //
  //    val printInt: Int ⇒ String = _.toString
  //
  //    f2(printInt)(123) shouldEqual "123"
    }

  it should "generate correct code for the const function with more unused arguments of coincident type" in {
    def f1[X, A, B]: A ⇒ X ⇒ A ⇒ B ⇒ X = implement

    f1("b")(123)("q")(true) shouldEqual 123
    f1(Some((3, 4)))("abc")(Some((1, 1)))(Map()) shouldEqual "abc"
    f1(0.0)(true)(123.0)('blah) shouldEqual true
  }

  // TODO: make this work too!
  it should "generate correct code for the identity function with explicit arguments" in {
    "def f1[X](x: X): X = implement" shouldNot compile
    //
    //    f1(123) shouldEqual 123
    //    f1("abc") shouldEqual "abc"
    //    f1(true) shouldEqual true
  }

  behavior of "proof search - internal details"

  it should "correctly explode sequences of integers" in {
    ITP.explode[Int](Seq(Seq(1, 2))) shouldEqual Seq(Seq(1), Seq(2))
    ITP.explode[Int](Seq(Seq(1, 2), Seq())) shouldEqual Seq()
    ITP.explode[Int](Seq(Seq())) shouldEqual Seq()
    ITP.explode[Int](Seq()) shouldEqual Seq(Seq())
    ITP.explode[Int](Seq(Seq(1, 2), Seq(10, 20, 30))) shouldEqual Seq(Seq(1, 10), Seq(1, 20), Seq(1, 30), Seq(2, 10), Seq(2, 20), Seq(2, 30))
  }

  private val freshVar = ITP.freshVar

  it should "correctly produce proofs from the Id axiom" in {
    followsFromAxioms(Sequent[Int](List(TP(3), TP(2), TP(1)), TP(0), freshVar)) shouldEqual Seq()

    followsFromAxioms(Sequent[Int](List(TP(3), TP(2), TP(1)), TP(1), freshVar)) shouldEqual Seq(
      CurriedE(List(PropE("x4", TP(3)), PropE("x5", TP(2)), PropE("x6", TP(1))), PropE("x6", TP(1)))
    )
  }
  it should "produce several proofs from the Id axiom" in {
    followsFromAxioms(Sequent[Int](List(TP(1), TP(2), TP(1)), TP(1), freshVar)) shouldEqual Seq(
      CurriedE(List(PropE("x7", TP(1)), PropE("x8", TP(2)), PropE("x9", TP(1))), PropE("x7", TP(1))),
      CurriedE(List(PropE("x7", TP(1)), PropE("x8", TP(2)), PropE("x9", TP(1))), PropE("x9", TP(1)))
    )
  }

  it should "correctly compute LJT subformulas" in {
    subformulas[Int](TP(1) :-> TP(1)) shouldEqual Set(TP(1) :-> TP(1), TP(1))
    subformulas[Int](TP(1) :-> TP(2)) shouldEqual Set(TP(1) :-> TP(2), TP(1), TP(2))
    subformulas[Int](TP(1) :-> (TP(2) :-> TP(3))) shouldEqual Set(TP(1) :-> (TP(2) :-> TP(3)), TP(2) :-> TP(3), TP(1), TP(2), TP(3))

    // Example from the paper.
    subformulas[String](((TP("A") :-> TP("A")) :-> TP("C")) :-> TP("C")) shouldEqual Set(
      ((TP("A") :-> TP("A")) :-> TP("C")) :-> TP("C"),
      (TP("A") :-> TP("A")) :-> TP("C"),
      TP("A") :-> TP("A"),
      TP("A") :-> TP("C"),
      TP("C") :-> TP("C"),
      TP("A"), TP("C")
    )

    // Disjunctions.
    subformulas[Int](DisjunctT(Seq(TP(1), TP(2))) :-> TP(3)) shouldEqual Set(
      DisjunctT(Seq(TP(1), TP(2))) :-> TP(3),
      DisjunctT(Seq(TP(1), TP(2))),
      TP(1) :-> TP(3),
      TP(2) :-> TP(3),
      TP(1), TP(2), TP(3)
    )

    // Conjunctions.
    subformulas[Int](ConjunctT(Seq(TP(1), TP(2), TP(3))) :-> TP(4)) shouldEqual Set(
      ConjunctT(Seq(TP(1), TP(2), TP(3))) :-> TP(4),
      ConjunctT(Seq(TP(1), TP(2), TP(3))),
      TP(1) :-> (TP(2) :-> (TP(3) :-> TP(4))),
      TP(2) :-> (TP(3) :-> TP(4)),
      TP(3) :-> TP(4),
      TP(1), TP(2), TP(3), TP(4)
    )
  }

  it should "find proof term for given sequent with premises" in {
    val sequent = Sequent(List(TP(1)), TP(1), freshVar)
    LJT.findProofTerms(sequent) shouldEqual Seq(CurriedE(List(PropE("x10", TP(1))), PropE("x10", TP(1))))
    val sequent2 = Sequent(List(TP(3), TP(2), TP(1)), TP(2), freshVar)
    LJT.findProofTerms(sequent2) shouldEqual Seq(
      CurriedE(List(PropE("x11", TP(3)), PropE("x12", TP(2)), PropE("x13", TP(1))), PropE("x12", TP(2)))
    )
  }

  behavior of "proof search - high-level API, rule ->R"

  it should "find proof term for the I combinator using rule ->R" in {
    val typeExpr = TP(1) :-> TP(1)
    val proofs = ITP.findProofs(typeExpr)
    proofs shouldEqual Seq(CurriedE(List(PropE("x14", TP(1))), PropE("x14", TP(1))))
  }

  it should "find proof term for the K combinator using rule ->R" in {
    val typeExpr = TP(1) :-> (TP(2) :-> TP(1))
    val proofs = ITP.findProofs(typeExpr)
    proofs shouldEqual Seq(
      CurriedE(List(PropE("x16", TP(2)), PropE("x17", TP(1))), PropE("x17", TP(1)))
    )
  }

  behavior of "proof search - high-level API, rule +Rn"

  it should "find proof term for simple instance of +Rn" in {
    val typeExpr = TP(1) :-> DisjunctT(Seq(TP(1), TP(2)))
    val proofs = ITP.findProofs(typeExpr)
    proofs shouldEqual Seq(CurriedE(List(PropE("x19", TP(1))), DisjunctE(0, 2, PropE("x19", TP(1)), DisjunctT(Seq(TP(1), TP(2))))))
  }

  it should "find proof term for simple instance of +Rn with several disjuncts" in {
    val typeExpr = TP(2) :-> DisjunctT(Seq(TP(1), TP(2), TP(3)))
    val proofs = ITP.findProofs(typeExpr)
    proofs shouldEqual Seq(CurriedE(List(PropE("x23", TP(2))), DisjunctE(1, 3, PropE("x23", TP(2)), DisjunctT(Seq(TP(1), TP(2), TP(3))))))
  }

  it should "inhabit type using +Rn" in {
    "def f[X]: X ⇒ Option[X] = implement" shouldNot compile

    //    f(1) shouldEqual Some(1)

    //    def f[X, Y]: X ⇒ Either[X, Y] = implement
  }
}
