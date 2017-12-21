package io.chymyst.ch

sealed trait TypeExpr[+T] {
  override lazy val toString: String = prettyPrint(0)

  private def prettyPrint(level: Int): String = this match {
    case DisjunctT(terms) ⇒ terms.map(_.prettyPrint(1)).mkString(" + ")
    case ConjunctT(terms) ⇒ "(" + terms.map(_.prettyPrint(0)).mkString(", ") + ")"
    case head #-> body ⇒
      val r = s"${head.prettyPrint(1)} → ${body.prettyPrint(0)}"
      if (level == 1) s"($r)" else r
    case BasicT(name) ⇒ s"<c>$name" // well-known constant type such as Int
    case ConstructorT(fullExpr) ⇒ s"<tc>$fullExpr" // type constructor with arguments, such as Seq[Int]
    case TP(name) ⇒ s"$name"
    case OtherT(name) ⇒ s"<oc>$name" // other constant type
    case NothingT(_) ⇒ "0"
    case UnitT(name) ⇒ name.toString
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
    def ->:(tpe2: TypeExpr[T]): TypeExpr[T] = makeImplication(tpe2, tpe1) // right-associative operators are desugared in the opposite order: a ->: b is b.->:(a)
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

// Since we do not know how to work with arbitrary type constructors, we treat them as atomic types.
// The only derivation rule for atomic types is the identity axiom.
final case class ConstructorT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
  override def map[U](f: T ⇒ U): TypeExpr[U] = ConstructorT(f(name))
}
