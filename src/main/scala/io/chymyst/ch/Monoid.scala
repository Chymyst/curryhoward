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
