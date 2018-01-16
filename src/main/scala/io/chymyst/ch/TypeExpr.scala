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
    case NamedConjunctT(constructor, tParams, _, wrapped) ⇒
      //      val termString = "(" + wrapped.map(_.prettyPrint).mkString(",") + ")" // Too verbose.
      val typeSuffix = if (caseObjectName.isDefined) ".type" else ""
      s"$constructor${TypeExpr.tParamString(tParams)}$typeSuffix"
    case RecurseT(name, tParams) ⇒ s"<rec>$name${TypeExpr.tParamString(tParams)}" // other constant type
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

  def typeParams: Seq[TypeExpr[T]] = Seq()
}

sealed trait NonAtomicTypeExpr {
  def isAtomic: Boolean = false
}

sealed trait AtomicTypeExpr[T] {
  def isAtomic: Boolean = true

  def name: T
}

object TypeExpr {
  def substNames[T](typeExpr: TypeExpr[T], typeMap: Map[T, TypeExpr[T]]): TypeExpr[T] = {

    def subst(typeExpr: TypeExpr[T]): TypeExpr[T] = substNames(typeExpr, typeMap)

    typeExpr match {
      case DisjunctT(constructor, tParams, terms) ⇒ DisjunctT(constructor, tParams.map(subst), terms.map(subst))
      case ConjunctT(terms) ⇒ ConjunctT(terms.map(subst))
      case #->(head, body) ⇒ #->(subst(head), subst(body))
      case NamedConjunctT(constructor, tParams, accessors, wrapped) ⇒
        NamedConjunctT(constructor, tParams.map(subst), accessors, wrapped.map(subst))
      case TP(name) ⇒ typeMap.getOrElse(name, typeExpr)
      case RecurseT(name, tParams) ⇒ RecurseT(name, tParams.map(subst))
      case _ ⇒ typeExpr
    }
  }

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
  override def typeParams: Seq[TypeExpr[T]] = tParams
}

final case class ConjunctT[T](terms: Seq[TypeExpr[T]]) extends TypeExpr[T] with NonAtomicTypeExpr

final case class #->[T](head: TypeExpr[T], body: TypeExpr[T]) extends TypeExpr[T] with NonAtomicTypeExpr

final case class NothingT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T]

final case class UnitT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T]

// Type parameter. Use a short name for convenience.
final case class TP[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T]

final case class RecurseT[T](name: T, tParams: Seq[TypeExpr[T]]) extends TypeExpr[T] with AtomicTypeExpr[T] {
  override def typeParams: Seq[TypeExpr[T]] = tParams
}

final case class BasicT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T]

/** This type expression represents a case class, treated as a named conjunction.
  * The `wrapped` is a type expression for the entire contents of the named conjunction. This can be a Unit, a single type, or a ConjunctT.
  * If `accessors` is empty and `wrapped` is Seq(), this is a case object. If `accessors` is empty and `wrapped` is Seq(UnitT), this is a case class with zero arguments.
  * If `accessors` in not empty, `wrapped` could be a ConjunctT with more than one part, or another type (e.g. Int or another NamedConjunctT or whatever else).
  */
final case class NamedConjunctT[+T](constructor: T, tParams: List[TypeExpr[T]], accessors: List[T], wrapped: List[TypeExpr[T]]) extends TypeExpr[T] {
  override def caseObjectName: Option[T] = if (isAtomic && wrapped.isEmpty) Some(constructor) else None

  override def isAtomic: Boolean = tParams.isEmpty && accessors.isEmpty

  override def typeParams: Seq[TypeExpr[T]] = tParams
}

// Since we do not know how to work with arbitrary type constructors, we treat them as atomic types.
// The only derivation rule for atomic types is the identity axiom.
final case class ConstructorT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T]
