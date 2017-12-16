package io.chymyst.ch

sealed trait TypeExpr[+T] {
  override lazy val toString: String = this match {
    case DisjunctT(terms) ⇒ terms.map(_.toString).mkString(" + ")
    case ConjunctT(terms) ⇒ "(" + terms.map(_.toString).mkString(", ") + ")"
    case head #-> body ⇒ s"($head) ..=>.. $body"
    case BasicT(name) ⇒ s"<basic>$name"
    case ConstructorT(fullExpr) ⇒ s"<constructor>$fullExpr"
    case TP(name) ⇒ s"<tparam>$name"
    case OtherT(name) ⇒ s"<other>$name"
    case NothingT(_) ⇒ "0"
    case UnitT(_) ⇒ "1"
  }

  def isAtomic: Boolean

  def map[U](f: T ⇒ U): TypeExpr[U]
}

sealed trait NonAtomicTypeExpr {
  def isAtomic: Boolean = false
}

sealed trait AtomicTypeExpr[T] {
  def isAtomic: Boolean = true

  def name: T
}

object TypeExpr {

  private def makeImplication[T](tpe1: TypeExpr[T], tpe2: TypeExpr[T]): TypeExpr[T] = #->(tpe1, tpe2)

  implicit class WithImplication[T](tpe1: TypeExpr[T]) {
    def #->(tpe2: TypeExpr[T]): TypeExpr[T] = makeImplication(tpe1, tpe2)
  }

}

final case class DisjunctT[T](terms: Seq[TypeExpr[T]]) extends TypeExpr[T] with NonAtomicTypeExpr {
  override def map[U](f: T ⇒ U): TypeExpr[U] = DisjunctT(terms.map(_.map(f)))
}

final case class ConjunctT[T](terms: Seq[TypeExpr[T]]) extends TypeExpr[T] with NonAtomicTypeExpr {
  override def map[U](f: T ⇒ U): TypeExpr[U] = ConjunctT(terms.map(_.map(f)))
}

final case class #->[T](head: TypeExpr[T], body: TypeExpr[T]) extends TypeExpr[T] with NonAtomicTypeExpr {
  override def map[U](f: T ⇒ U): TypeExpr[U] = #->(head.map(f), body.map(f))
}

final case class NothingT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
  override def map[U](f: T ⇒ U): TypeExpr[U] = NothingT(f(name))
}

final case class UnitT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
  override def map[U](f: T ⇒ U): TypeExpr[U] = UnitT(f(name))
}

// Type parameter. Use a short name for convenience.
case class TP[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
  override def map[U](f: T ⇒ U): TypeExpr[U] = TP(f(name))
}

case class OtherT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
  override def map[U](f: T ⇒ U): TypeExpr[U] = OtherT(f(name))
}

final case class BasicT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
  override def map[U](f: T ⇒ U): TypeExpr[U] = BasicT(f(name))
}

final case class ConstructorT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
  override def map[U](f: T ⇒ U): TypeExpr[U] = ConstructorT(f(name))
}
