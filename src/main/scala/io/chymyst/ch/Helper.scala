package io.chymyst.ch

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
