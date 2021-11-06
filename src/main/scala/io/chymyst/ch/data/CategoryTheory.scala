package io.chymyst.ch.data

// Declarations of standard type classes, to be used in macros.

trait Semigroup[T] {
  def combine(x: T, y: T): T
}

trait Monoid[T] extends Semigroup[T] {
  def empty: T
}

object Monoid {
  def empty[T](implicit ev: Monoid[T]): T = ev.empty

  implicit class MonoidSyntax[T](t: T)(implicit ev: Monoid[T]) {

    def combine(y: T): T = ev.combine(t, y)
  }

}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A ⇒ B): F[B]
}

trait ContraFunctor[F[_]] {
  def map[A, B](fa: F[A])(f: B ⇒ A): F[B]
}

trait Filterable[F[_]] extends Functor[F] {
  def deflate[A](fa: F[Option[A]]): F[A]
}

trait ContraFilterable[F[_]] extends ContraFunctor[F] {
  def inflate[A](fa: F[A]): F[Option[A]]
}

trait Semimonad[F[_]] extends Functor[F] {
  def join[A](ffa: F[F[A]]): F[A]
}

trait Pointed[F[_]] extends Functor[F] {
  def point[A]: F[A]
}

trait Zippable[F[_]] extends Functor[F] {
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

trait Foldable[F[_]] extends Functor[F] {
  def foldMap[A, B: Monoid](fa: F[A])(f: A ⇒ B)
}

trait Traversable[F[_]] extends Functor[F] {
  def sequence[Z[_] : Zippable, A](fga: F[Z[A]]): Z[F[A]]
}

trait Monad[F[_]] extends Pointed[F] with Semimonad[F]

trait Applicative[F[_]] extends Pointed[F] with Zippable[F]

trait Cosemimonad[F[_]] extends Functor[F] {
  def cojoin[A](fa: F[A]): F[F[A]]
}

trait Copointed[F[_]] extends Functor[F] {
  def extract[A](fa: F[A]): A
}

trait Comonad[F[_]] extends Copointed[F] with Cosemimonad[F]

trait Cozippable[F[_]] extends Functor[F] {
  def decide[A, B](fab: F[Either[A, B]]): Either[F[A], F[B]]
}
