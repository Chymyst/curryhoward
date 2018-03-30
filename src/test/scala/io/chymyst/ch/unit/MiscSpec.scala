package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Future

class MiscSpec extends FlatSpec with Matchers {

  behavior of "miscellaneous examples"

  it should "support filter syntax" in {

    final case class C[A](d: Option[(A, A)]) {
      // greedy filter on the product
      def map[B](f: A ⇒ B): C[B] = ofType[C[B]](d, f)

      def withFilter(p: A ⇒ Boolean): C[A] = C(d filter {
        case (x, y) ⇒ p(x) && p(y)
      })
    }

    val c = C(Some((123, 456)))
    val d: Int ⇒ C[Int] = limit ⇒ for {
      x ← c
      y = x * 2
      if y > limit
    } yield y

    c.map(x ⇒ x * 2) shouldEqual C(Some((246, 912)))
    d(500) shouldEqual C(None)
    d(200) shouldEqual C(Some((246, 912)))
  }

  it should "correctly work with tuples of options" in {

    type P[T] = (Option[T], Option[T])

    // TODO: make this work and return the single correct fmap
    def fmap[A, B] = allOfType[(A ⇒ B) ⇒ P[A] ⇒ P[B]].map(_.lambdaTerm)

    fmap.length shouldEqual 2
    // def flattens[A] = anyOfType[P[Option[Int]] ⇒ P[Int]]()
    // flattens.length shouldEqual 128
    //    flatten((Some(Some(1)), Some(Some(2)))) shouldEqual ((Some(1), None)) // This is incorrect!
    //     flatten((Some(Some(1)), Some(Some(2)))) shouldEqual ((Some(1), Some(2)))
    //     flatten((Some(None), Some(Some(2)))) shouldEqual ((None, Some(2)))
    //     flatten((Some(Some(1)), None)) shouldEqual ((Some(1), None))

    // TODO: optimize the performance here!
    /* This takes 25 seconds. Why is it so slow?
    System.setProperty("curryhoward.log", "prover")

    def flattenType[A] = freshVar[P[P[A]] ⇒ P[A]].t

    val initTime = System.currentTimeMillis()
    val proofs = TheoremProver.findProofs(flattenType)
    val elapsed = System.currentTimeMillis() - initTime
    println(s"Computing proofs for flatten on (1+T)x(1+T) took $elapsed ms")

    proofs._1.length shouldEqual 16
    proofs._2.length shouldEqual 128

    System.clearProperty("curryhoward.log")
*/
  }

  it should "correctly work with nested options" in {
    type Q[T] = Option[Option[T]]

    def flattens[A] = anyOfType[Q[Option[Int]] ⇒ Q[Int]]()

    val terms = flattens.map(_.lambdaTerm)
    terms.length shouldEqual 26
  }

  it should "support foreign type constructors" in {
    val x = freshVar[Set[Seq[Int]]]
    x.t shouldEqual ConstructorT("Set", List(ConstructorT("Seq", List(BasicT("Int")))))
  }

  it should "support library types" in {
    type P[T] = Set[Future[Seq[T]]]

    def f[T] = ofType[P[T] ⇒ P[T]]

    f.lambdaTerm.prettyPrint shouldEqual "a ⇒ a"
  }

  it should "generate all implementations of `map` for the continuation monad with an extra function" in {
    type R = Int ⇒ String
    type C[T] = (T ⇒ R) ⇒ R

    // TODO: this is slow! Need to optimize performance here.
    def fmapc[A, B] = anyOfType[C[A] ⇒ (A ⇒ B) ⇒ C[B]]()

    fmapc.length shouldEqual 24
  }
}
