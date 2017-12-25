package io.chymyst.ch

sealed trait TypeExpr[+T] {
  lazy val prettyPrint: String = prettyPrintWithParentheses(0)

  private def prettyPrintWithParentheses(level: Int): String = this match {
    case DisjunctT(constructor, tParams, terms) ⇒ s"$constructor${TypeExpr.tParamString(tParams)}{${terms.map(_.prettyPrintWithParentheses(1)).mkString(" + ")}}"
    case ConjunctT(terms) ⇒ s"(${terms.map(_.prettyPrintWithParentheses(0)).mkString(", ")})"
    case head #-> body ⇒
      val r = s"${head.prettyPrintWithParentheses(1)} ⇒ ${body.prettyPrintWithParentheses(0)}"
      if (level == 1) s"($r)" else r
    case BasicT(name) ⇒ s"<c>$name" // well-known constant type such as Int
    case ConstructorT(fullExpr) ⇒ s"<tc>$fullExpr" // type constructor with arguments, such as Seq[Int]
    case TP(name) ⇒ s"$name"
    case NamedConjunctT(constructor, tParams, accessors, wrapped) ⇒
      val typeSuffix = if (caseObjectName.isDefined) ".type" else ""
      s"$constructor${TypeExpr.tParamString(tParams)}$typeSuffix"
    case OtherT(name) ⇒ s"<oc>$name" // other constant type
    case NothingT(_) ⇒ "0"
    case UnitT(name) ⇒ s"$name"
  }

  def conjunctSize: Int = this match {
    case ConjunctT(terms) ⇒ terms.length
    case NamedConjunctT(_, _, _, wrapped) ⇒ wrapped.size
    case _ ⇒ 1
  }

  def caseObjectName: Option[T] = None

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
  private[ch] def tParamString[T](tParams: Seq[TypeExpr[T]]): String =
    if (tParams.isEmpty)
      ""
    else
      s"[${tParams.map(_.prettyPrintWithParentheses(0)).mkString(",")}]"

  private def makeImplication[T](tpe1: TypeExpr[T], tpe2: TypeExpr[T]): TypeExpr[T] = #->(tpe1, tpe2)

  implicit class WithImplication[T](tpe1: TypeExpr[T]) {
    def ->:(tpe2: TypeExpr[T]): TypeExpr[T] = makeImplication(tpe2, tpe1) // right-associative operators are desugared in the opposite order: a ->: b is b.->:(a)
  }

}

final case class DisjunctT[T](constructor: T, tParams: Seq[TypeExpr[T]], terms: Seq[TypeExpr[T]]) extends TypeExpr[T] with NonAtomicTypeExpr {
  override def map[U](f: T ⇒ U): TypeExpr[U] = DisjunctT(f(constructor), tParams map (_ map f), terms map (_ map f))
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
final case class TP[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
  override def map[U](f: T ⇒ U): TypeExpr[U] = TP(f(name))
}

final case class OtherT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
  override def map[U](f: T ⇒ U): TypeExpr[U] = OtherT(f(name))
}

final case class BasicT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
  override def map[U](f: T ⇒ U): TypeExpr[U] = BasicT(f(name))
}

/** This type expression represents a case class, treated as a named conjunction.
  * The `wrapped` is a type expression for the entire contents of the named conjunction. This can be a Unit, a single type, or a ConjunctT.
  * If `accessors` is empty and `wrapped` is Seq(), this is a case object. If `accessors` is empty and `wrapped` is Seq(UnitT), this is a case class with zero arguments.
  * If `accessors` in not empty, `wrapped` could be a ConjunctT with more than one part, or another type (e.g. Int or another NamedConjunctT or whatever else).
  */
final case class NamedConjunctT[+T](constructor: T, tParams: List[TypeExpr[T]], accessors: List[T], wrapped: List[TypeExpr[T]]) extends TypeExpr[T] with NonAtomicTypeExpr {
  override def map[U](f: T ⇒ U): NamedConjunctT[U] = NamedConjunctT(f(constructor), tParams map (_ map f), accessors map f, wrapped map (_ map f))

  override def caseObjectName: Option[T] = if (tParams.isEmpty && accessors.isEmpty && wrapped.isEmpty) Some(constructor) else None
}

// Since we do not know how to work with arbitrary type constructors, we treat them as atomic types.
// The only derivation rule for atomic types is the identity axiom.
final case class ConstructorT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
  override def map[U](f: T ⇒ U): TypeExpr[U] = ConstructorT(f(name))
}
