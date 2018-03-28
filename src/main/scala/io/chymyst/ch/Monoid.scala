package io.chymyst.ch

trait Monoid[T] {
  def empty: T

  def combine(x: T, y: T): T
}

object Monoid {
  def empty[T](implicit ev: Monoid[T]): T = ev.empty

  implicit class MonoidSyntax[T](t: T)(implicit ev: Monoid[T]) {

    def combine(y: T): T = ev.combine(t, y)
  }

}

object Helper {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class AnyOpsEquals[A](val self: A) extends AnyVal {
    @inline def ===(other: A): Boolean = self == other
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class AnyOpsNotEquals[A](val self: A) extends AnyVal {
    // removing `[@specialized A]` to allow `extends AnyVal`
    @inline def !==(other: A): Boolean = self != other
  }
}
