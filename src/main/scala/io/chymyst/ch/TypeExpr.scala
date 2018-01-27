package io.chymyst.ch

sealed trait TypeExpr {
  def apply(args: TermExpr*): TermExpr = this match {
    case ConjunctT(terms) ⇒
      val invalidTypeArgs = args.zipWithIndex.filter { case (arg, i) ⇒ !terms.lift(i).contains(arg.tExpr) }
      if (terms.length != args.length)
        throw new Exception(s".apply() must be called with ${terms.length} arguments on this type $prettyPrint but it was called with ${args.length} arguments")
      else if (invalidTypeArgs.isEmpty)
        ConjunctE(args)
      else throw new Exception(s"Some arguments have unexpected types [${invalidTypeArgs.map(_._1.tExpr.prettyPrint).mkString("; ")}] that do not match the types in $prettyPrint")

    case nct: NamedConjunctT ⇒
      val invalidTypeArgs = args.zipWithIndex.filter { case (arg, i) ⇒ !nct.wrapped.lift(i).contains(arg.tExpr) }
      if (nct.accessors.length != args.length)
        throw new Exception(s".apply() must be called with ${nct.accessors.length} arguments on this type $prettyPrint but it was called with ${args.length} arguments")
      else if (invalidTypeArgs.isEmpty)
        NamedConjunctE(args, nct)
      else throw new Exception(s"Some arguments have unexpected types [${invalidTypeArgs.map(_._1.tExpr.prettyPrint).mkString("; ")}] that do not match the types in $prettyPrint")

    case DisjunctT(_, _, terms) ⇒ args.headOption match {
      case Some(arg) if args.length == 1 ⇒
        val index = terms.indexOf(arg.tExpr)
        if (index >= 0)
          DisjunctE(index, terms.length, arg, this)
        else throw new Exception(s"Cannot inject into disjunction since the given disjunction type $prettyPrint does not contain the type ${arg.tExpr.prettyPrint} of the given term $arg")
      case _ ⇒ throw new Exception(s"Calling .apply() on type $prettyPrint requires one argument (disjunction injection value)")
    }

    case _: UnitT ⇒ if (args.isEmpty)
      UnitE(this)
    else throw new Exception(s"Calling .apply() on type $prettyPrint requires zero arguments (named unit value)")

    case _ ⇒ throw new Exception(s"Cannot call .apply() on type $prettyPrint")
  }

  def substTypeVar(typeVar: TP, replaceBy: TypeExpr): TypeExpr = this match {
    case DisjunctT(constructor, tParams, terms) ⇒ DisjunctT(constructor, tParams.map(_.substTypeVar(typeVar, replaceBy)), terms.map(_.substTypeVar(typeVar, replaceBy)))
    case ConjunctT(terms) ⇒ ConjunctT(terms.map(_.substTypeVar(typeVar, replaceBy)))
    case #->(head, body) ⇒ #->(head.substTypeVar(typeVar, replaceBy), body.substTypeVar(typeVar, replaceBy))
    case NothingT(_) ⇒ this
    case UnitT(_) ⇒ this
    case TP(_) ⇒ if (this == typeVar) replaceBy else this
    case RecurseT(name, tParams) ⇒ RecurseT(name, tParams.map(_.substTypeVar(typeVar, replaceBy)))
    case BasicT(_) ⇒ this
    case NamedConjunctT(constructor, tParams, accessors, wrapped) ⇒ NamedConjunctT(constructor, tParams.map(_.substTypeVar(typeVar, replaceBy)), accessors, wrapped.map(_.substTypeVar(typeVar, replaceBy)))
    case ConstructorT(_) ⇒ this
  }

  lazy val prettyPrint: String = prettyPrintWithParentheses(0)

  private def prettyPrintWithParentheses(level: Int): String = this match {
    case DisjunctT(constructor, tParams, terms) ⇒ s"$constructor${TypeExpr.tParamString(tParams)}{${terms.map(_.prettyPrintWithParentheses(1)).mkString(" + ")}}"
    case ConjunctT(terms) ⇒ s"(${terms.map(_.prettyPrintWithParentheses(0)).mkString(", ")})"
    case head #-> body ⇒
      val r = s"${head.prettyPrintWithParentheses(1)} ⇒ ${body.prettyPrintWithParentheses(0)}"
      if (level == 1) s"($r)" else r
    case BasicT(name) ⇒ s"<c>$name" // a basic, non-parameter type such as Int
    case ConstructorT(fullExpr) ⇒ s"<tc>$fullExpr" // type constructor with arguments, such as Seq[Int]
    case TP(name) ⇒ s"$name"
    case NamedConjunctT(constructor, tParams, _, wrapped@_) ⇒
      //      val termString = "(" + wrapped.map(_.prettyPrint).mkString(",") + ")" // Too verbose.
      val typeSuffix = if (caseObjectName.isDefined) ".type" else ""
      s"$constructor${TypeExpr.tParamString(tParams)}$typeSuffix"
    case RecurseT(name, tParams) ⇒ s"<rec>$name${TypeExpr.tParamString(tParams)}" // recursive instance of type
    case NothingT(_) ⇒ "0"
    case UnitT(name) ⇒ s"$name"
  }

  def conjunctSize: Int = this match {
    case ConjunctT(terms) ⇒ terms.length
    case NamedConjunctT(_, _, _, wrapped) ⇒ wrapped.size
    case _ ⇒ 1
  }

  def caseObjectName: Option[String] = None

  def isAtomic: Boolean

  def typeParams: Seq[TypeExpr] = Seq()
}

sealed trait NonAtomicTypeExpr {
  def isAtomic: Boolean = false
}

sealed trait AtomicTypeExpr {
  def isAtomic: Boolean = true

  def name: String
}

object TypeExpr {
  def substNames(typeExpr: TypeExpr, typeMap: Map[String, TypeExpr]): TypeExpr = {

    def subst(typeExpr: TypeExpr): TypeExpr = substNames(typeExpr, typeMap)

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

  private[ch] def tParamString(tParams: Seq[TypeExpr]): String =
    if (tParams.isEmpty)
      ""
    else
      s"[${tParams.map(_.prettyPrintWithParentheses(0)).mkString(",")}]"

  private def makeImplication(tpe1: TypeExpr, tpe2: TypeExpr): TypeExpr = #->(tpe1, tpe2)

  implicit class WithImplication(tpe1: TypeExpr) {
    def ->:(tpe2: TypeExpr): TypeExpr = makeImplication(tpe2, tpe1) // right-associative operators are desugared in the opposite order: a ->: b is b.->:(a)
  }

}

final case class DisjunctT(constructor: String, tParams: Seq[TypeExpr], terms: Seq[TypeExpr]) extends TypeExpr with NonAtomicTypeExpr {
  override def typeParams: Seq[TypeExpr] = tParams
}

final case class ConjunctT(terms: Seq[TypeExpr]) extends TypeExpr with NonAtomicTypeExpr

final case class #->(head: TypeExpr, body: TypeExpr) extends TypeExpr with NonAtomicTypeExpr

final case class NothingT(name: String) extends TypeExpr with AtomicTypeExpr

final case class UnitT(name: String) extends TypeExpr with AtomicTypeExpr

// Type parameter. Use a short name for convenience.
final case class TP(name: String) extends TypeExpr with AtomicTypeExpr

final case class RecurseT(name: String, tParams: Seq[TypeExpr]) extends TypeExpr with AtomicTypeExpr {
  override def typeParams: Seq[TypeExpr] = tParams
}

final case class BasicT(name: String) extends TypeExpr with AtomicTypeExpr

/** This type expression represents a case class, treated as a named conjunction.
  * The `wrapped` is a type expression for the entire contents of the named conjunction. This can be a Unit, a single type, or a ConjunctT.
  * If `accessors` is empty and `wrapped` is Seq(), this is a case object. If `accessors` is empty and `wrapped` is Seq(UnitT), this is a case class with zero arguments.
  * If `accessors` in not empty, `wrapped` could be a ConjunctT with more than one part, or another type (e.g. Int or another NamedConjunctT or whatever else).
  */
final case class NamedConjunctT(constructor: String, tParams: List[TypeExpr], accessors: List[String], wrapped: List[TypeExpr]) extends TypeExpr {
  override def caseObjectName: Option[String] = if (isAtomic && wrapped.isEmpty) Some(constructor) else None

  override def isAtomic: Boolean = tParams.isEmpty && accessors.isEmpty

  override def typeParams: Seq[TypeExpr] = tParams
}

// Since we do not know how to work with arbitrary type constructors, we treat them as atomic types.
// The only derivation rule for atomic types is the identity axiom.
final case class ConstructorT(name: String) extends TypeExpr with AtomicTypeExpr
