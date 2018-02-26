package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Future

class MiscSpec extends FlatSpec with Matchers {

  behavior of "miscellaneous examples"
  /*
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

      // TODO: make this work
      //  def fmap[A, B](f: A ⇒ B):P[A] ⇒ P[B] = implement
      //  def flatten[A]: P[Option[Int]] ⇒ P[Int] = implement
      //    flatten((Some(Some(1)), Some(Some(2)))) shouldEqual ((Some(1), None)) // This is incorrect!
      //     flatten((Some(Some(1)), Some(Some(2)))) shouldEqual ((Some(1), Some(2)))
      //     flatten((Some(None), Some(Some(2)))) shouldEqual ((None, Some(2)))
      //     flatten((Some(Some(1)), None)) shouldEqual ((Some(1), None))

      def flattens[A] = anyOfType[P[Option[Int]] ⇒ P[Int]]()

      flattens.length shouldEqual 128

    }
*/
  it should "support foreign type constructors" in {
    
  }

    it should "support library types" in {
      type P[T] = Set[Future[Seq[T]]]

      // TODO: make this work
      "def f[T] = ofType[P[T] ⇒ P[T]]" shouldNot compile

      //    f.lambdaTerm.prettyPrint shouldEqual ""
    }
 /*
  it should "generate 26 versions of `map` for the continuation monad" in {
    type R = Int ⇒ String
    type C[T] = (T ⇒ R) ⇒ R

    def fmapc[A, B] = anyOfType[C[A] ⇒ (A ⇒ B) ⇒ C[B]]()

    fmapc.length shouldEqual 26
  } */
}
