package io.chymyst.ch

import io.chymyst.ch.Helper._

sealed trait TypeExpr {
  def apply(args: TermExpr*): TermExpr = this match {
    case ConjunctT(terms) ⇒
      val invalidTypeArgs = args.zipWithIndex.filter { case (arg, i) ⇒ !terms.lift(i).contains(arg.t) }
      if (terms.length !== args.length)
        throw new Exception(s".apply() must be called with ${terms.length} arguments on this type $prettyPrint but it was called with ${args.length} arguments")
      else if (invalidTypeArgs.isEmpty)
        ConjunctE(args)
      else throw new Exception(s"Some arguments of .apply() have unexpected types [${invalidTypeArgs.map(_._1.t.prettyPrint).mkString("; ")}] that do not match the types in $prettyPrint")

    case nct: NamedConjunctT ⇒
      val invalidTypeArgs = args.zipWithIndex.filter { case (arg, i) ⇒ !nct.wrapped.lift(i).contains(arg.t) }
      if (nct.accessors.length !== args.length)
        throw new Exception(s".apply() must be called with ${nct.accessors.length} arguments on this type $prettyPrint but it was called with ${args.length} arguments")
      else if (invalidTypeArgs.isEmpty)
        NamedConjunctE(args, nct)
      else throw new Exception(s"Some arguments of .apply() have unexpected types [${invalidTypeArgs.map(_._1.t.prettyPrint).mkString("; ")}] that do not match the types in $prettyPrint")

    case DisjunctT(_, _, terms) ⇒ args.headOption match {
      case Some(arg) if args.length === 1 ⇒
        val index = terms.indexOf(arg.t)
        if (index >= 0)
          DisjunctE(index, terms.length, arg, this)
        else throw new Exception(s"Cannot inject into disjunction since the given disjunction type $prettyPrint does not contain the type ${arg.t.prettyPrint} of the given term $arg")
      case _ ⇒ throw new Exception(s"Calling .apply() on type $prettyPrint requires one argument (disjunction injection value)")
    }

    case _: UnitT ⇒ if (args.isEmpty)
      UnitE(this)
    else throw new Exception(s"Calling .apply() on type $prettyPrint requires zero arguments (named unit value)")

    case _ ⇒ throw new Exception(s"Cannot call .apply() on type $prettyPrint")
  }

  def substTypeVar(typeVar: TP, replaceBy: TypeExpr): TypeExpr = this match {
    case DisjunctT(constructor, tParams, terms) ⇒
      DisjunctT(constructor, tParams.map(_.substTypeVar(typeVar, replaceBy)),
        terms.map(_.substTypeVar(typeVar, replaceBy)))
    case ConjunctT(terms) ⇒ ConjunctT(terms.map(_.substTypeVar(typeVar, replaceBy)))
    case #->(head, body) ⇒ #->(head.substTypeVar(typeVar, replaceBy), body.substTypeVar(typeVar, replaceBy))
    case NothingT(_) ⇒ this
    case UnitT(_) ⇒ this
    case TP(_) ⇒ if (this === typeVar) replaceBy else this
    case RecurseT(name, tParams) ⇒ RecurseT(name, tParams.map(_.substTypeVar(typeVar, replaceBy)))
    case BasicT(_) ⇒ this
    case NamedConjunctT(constructor, tParams, accessors, wrapped) ⇒ NamedConjunctT(constructor, tParams.map(_.substTypeVar(typeVar, replaceBy)), accessors, wrapped.map(_.substTypeVar(typeVar, replaceBy)))
    case ConstructorT(name, tParams) ⇒ ConstructorT(name, tParams.map(_.substTypeVar(typeVar, replaceBy)))
  }

  def substTypeVars(substitutions: Map[TP, TypeExpr]): TypeExpr = this match {
    case DisjunctT(constructor, tParams, terms) ⇒
      DisjunctT(constructor, tParams.map(_.substTypeVars(substitutions)),
        terms.map(_.substTypeVars(substitutions)))
    case ConjunctT(terms) ⇒ ConjunctT(terms.map(_.substTypeVars(substitutions)))
    case #->(head, body) ⇒ #->(head.substTypeVars(substitutions), body.substTypeVars(substitutions))
    case NothingT(_) ⇒ this
    case UnitT(_) ⇒ this
    case tp@TP(_) ⇒ substitutions.get(tp) match {
      case Some(replaceBy) ⇒ replaceBy
      case None ⇒ this
    }
    case RecurseT(name, tParams) ⇒ RecurseT(name, tParams.map(_.substTypeVars(substitutions)))
    case BasicT(_) ⇒ this
    case NamedConjunctT(constructor, tParams, accessors, wrapped) ⇒ NamedConjunctT(constructor, tParams.map(_.substTypeVars(substitutions)), accessors, wrapped.map(_.substTypeVars(substitutions)))
    case ConstructorT(name, tParams) ⇒ ConstructorT(name, tParams.map(_.substTypeVars(substitutions)))
  }

  lazy val prettyPrint: String = prettyPrintWithParentheses(0, showDisjunctionParts = false)

  lazy val prettyPrintVerbose: String = prettyPrintWithParentheses(0, showDisjunctionParts = true)

  private[ch] def prettyPrintWithParentheses(level: Int, showDisjunctionParts: Boolean): String = this match {
    case DisjunctT(constructor, tParams, terms) ⇒
      val constructorString = s"$constructor${TypeExpr.tParamString(tParams, showDisjunctionParts)}"
      val partsString = s"{${terms.map(_.prettyPrintWithParentheses(1, showDisjunctionParts)).mkString(" + ")}}"
      if (showDisjunctionParts) constructorString + partsString else constructorString
    case ConjunctT(terms) ⇒ s"(${terms.map(_.prettyPrintWithParentheses(0, showDisjunctionParts)).mkString(", ")})"
    case head #-> body ⇒
      val r = s"${head.prettyPrintWithParentheses(1, showDisjunctionParts)} ⇒ ${body.prettyPrintWithParentheses(0, showDisjunctionParts)}"
      if (level === 1) s"($r)" else r
    case BasicT(name) ⇒ s"<c>$name" // a basic, non-parameter type such as Int
    case ConstructorT(fullExpr, tParams) ⇒ s"<tc>$fullExpr${TypeExpr.tParamString(tParams, showDisjunctionParts)}" // A type constructor (e.g. `Seq[Int]`), possibly with type arguments.
    case TP(name) ⇒ s"$name"
    case NamedConjunctT(constructor, tParams, _, wrapped@_) ⇒
      //      val termString = "(" + wrapped.map(_.prettyPrint).mkString(",") + ")" // Too verbose.
      val typeSuffix = if (caseObjectName.isDefined) ".type" else ""
      s"$constructor${TypeExpr.tParamString(tParams, showDisjunctionParts)}$typeSuffix"
    case RecurseT(name, tParams) ⇒ s"<rec>$name${TypeExpr.tParamString(tParams, showDisjunctionParts)}" // recursive instance of type
    case NothingT(_) ⇒ "0"
    case UnitT(name) ⇒ s"$name"
  }

  lazy val conjunctSize: Int = this match {
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

  def findFirst[R](inExpr: TypeExpr)(pred: PartialFunction[TypeExpr, R]): Option[R] = {
    def ffirst(expr: TypeExpr): Option[R] = findFirst(expr)(pred)

    Some(inExpr).collect(pred).orElse {
      inExpr match {
        case DisjunctT(constructor, typeParams, terms) ⇒ terms.find(t ⇒ ffirst(t).nonEmpty).flatMap(ffirst)
        case ConjunctT(terms) ⇒ terms.find(t ⇒ ffirst(t).nonEmpty).flatMap(ffirst)
        case #->(head, body) ⇒ ffirst(head).orElse(ffirst(body))
        case NothingT(name) ⇒ None
        case UnitT(name) ⇒ None
        case TP(name) ⇒ None
        case RecurseT(name, typeParams) ⇒ None
        case BasicT(name) ⇒ None
        case NamedConjunctT(constructor, typeParams, accessors, wrapped) ⇒ wrapped.find(t ⇒ ffirst(t).nonEmpty).flatMap(ffirst)
        case ConstructorT(name, typeParams) ⇒ None
      }
    }
  }

  def allTypeParams(typeExpr: TypeExpr): Set[TP] = typeExpr match {
    case DisjunctT(_, tParams, terms) ⇒ (tParams ++ terms).flatMap(allTypeParams).toSet
    case ConjunctT(terms) ⇒ terms.flatMap(allTypeParams).toSet
    case #->(head, body) ⇒ Set(head, body).flatMap(allTypeParams)
    case NothingT(_) ⇒ Set()
    case UnitT(_) ⇒ Set()
    case tp@TP(_) ⇒ Set(tp)
    case RecurseT(_, tParams) ⇒ tParams.flatMap(allTypeParams).toSet
    case BasicT(_) ⇒ Set()
    case NamedConjunctT(_, tParams, _, wrapped) ⇒ (tParams ++ wrapped).flatMap(allTypeParams).toSet
    case ConstructorT(_, tParams) ⇒ tParams.flatMap(allTypeParams).toSet
  }

  def substNames(typeExpr: TypeExpr, typeMap: Map[String, TypeExpr]): TypeExpr = {

    def subst(typeExpr: TypeExpr): TypeExpr = substNames(typeExpr, typeMap)

    typeExpr match {
      case DisjunctT(constructor, tParams, terms) ⇒
        DisjunctT(constructor, tParams.map(subst), terms.map(subst))
      case ConjunctT(terms) ⇒ ConjunctT(terms.map(subst))
      case #->(head, body) ⇒ #->(subst(head), subst(body))
      case NamedConjunctT(constructor, tParams, accessors, wrapped) ⇒
        NamedConjunctT(constructor, tParams.map(subst), accessors, wrapped.map(subst))
      case TP(name) ⇒ typeMap.getOrElse(name, typeExpr)
      case RecurseT(name, tParams) ⇒ RecurseT(name, tParams.map(subst))
      case ConstructorT(name, tParams) ⇒ ConstructorT(name, tParams.map(subst))
      case _ ⇒ typeExpr
    }
  }

  private def containsConstructor(typeExpr: TypeExpr, constructorName: String): Boolean = typeExpr match {
    case NamedConjunctT(c, _, _, _) ⇒ c === constructorName
    case RecurseT(c, _) ⇒ c === constructorName
    case _ ⇒ false
  }

  private[ch] def isDisjunctionPart(disj: TypeExpr, part: TypeExpr): Boolean = (disj, part) match {
    case (DisjunctT(_, _, terms), NamedConjunctT(ncName, _, _, _)) if terms.exists(containsConstructor(_, ncName)) ⇒ true
    case _ ⇒ false
  }

  private[ch] type UnifyResult = Either[String, Map[TP, TypeExpr]]

  private val freshTypeVarIdents = new FreshIdents("Z")

  // Unification is performed between src and dst; the entire initial term is fullSrc.
  private[ch] def leftUnify(src: TypeExpr, dst: TypeExpr, fullSrc: TypeExpr): UnifyResult = {
    // Check whether some type variables are not mapped but used in other variables' maps.
    // In this case, we need to auto-rename them.
    leftUnifyRec(src, dst, Map()).right.map { substitutions ⇒
      val usedVars = substitutions.values.map(allTypeParams).foldLeft(Set[TP]())(_ ++ _)
      val unmappedVars = (usedVars intersect allTypeParams(fullSrc)) -- substitutions.keySet
      val alphaConversions: Map[TP, TypeExpr] = unmappedVars.toSeq.map {
        _ → TP(freshTypeVarIdents())
      }.to(Map)
      substitutions ++ alphaConversions
    }
  }

  /** Obtain type variable substitutions via unification of two type expressions `src` and `dst`.
    * Left-Unification consists of finding the values for type variables in `src` such that the two type expressions match.
    *
    * @param src           The first type expression. Type variables in this expression may be substituted in search of unification.
    * @param dst           The second type expression.
    * @param substitutions Previously available substitutions, if any.
    * @return An updated substitution map, or an error message if unification cannot succeed.
    */
  private def leftUnifyRec(src: TypeExpr, dst: TypeExpr, substitutions: Map[TP, TypeExpr]): UnifyResult = {
//    import MonadEither._ // This is necessary to support Scala 2.11. Do not remove this import.
    def wrapResult(tuples: Seq[(TypeExpr, TypeExpr)]): UnifyResult = tuples.foldLeft[UnifyResult](Right(substitutions)) { case (prev, (t, t2)) ⇒
      prev.flatMap(p ⇒ leftUnifyRec(t, t2, p))
    }

    val allDone: UnifyResult = Right(substitutions)

    val error: UnifyResult = Left(s"Cannot unify ${src.prettyPrint} with an incompatible type ${dst.prettyPrint}")

    def unifyTP(tp: TP, other: TypeExpr): UnifyResult = {
      //      if (TypeExpr.allTypeParams(dst) contains tp)
      //        Left(s"Cannot unify ${src.prettyPrint} with ${dst.prettyPrint} because type variable ${tp.prettyPrint} is used in the destination type")
      //      else {
      // Check that the new substitution does not contradict earlier substitutions for this variable.
      substitutions.get(tp) match {
        case Some(oldSubstitution) if other !== oldSubstitution ⇒ Left(s"Cannot unify ${src.prettyPrint} with ${dst.prettyPrint} because type parameter ${tp.prettyPrint} requires incompatible substitutions ${oldSubstitution.prettyPrint} and ${other.prettyPrint}")
        case _ ⇒ Right(Map(tp → other) ++ substitutions)
      }

      //      }
    }

    val result: UnifyResult = (src, dst) match {
      //      case (TP(name1), TP(name2)) if name1 === name2 ⇒ allDone // Let's not do this. Substitutions A -> A are useful to keep in the list, because it will prevent inconsistencies.
      // A type parameter can unify with anything, as long as it is not free there.
      case (tp@TP(_), _) ⇒ unifyTP(tp, dst)
      //      case (_, tp@TP(_)) ⇒ unifyTP(tp, src)

      case (BasicT(name1), BasicT(name2)) if name1 === name2 ⇒ allDone
      case (UnitT(name1), UnitT(name2)) if name1 === name2 ⇒ allDone
      case (NothingT(name1), NothingT(name2)) if name1 === name2 ⇒ allDone
      case (ConstructorT(name1, tParams), ConstructorT(name2, tParams2)) if name1 === name2 && tParams.length === tParams2.length ⇒ wrapResult(tParams zip tParams2)

      // Can unify DisjunctT with the same DisjunctT or with one of the terms.
      case (DisjunctT(constructor, tParams, _), DisjunctT(constructor2, tParams2, _))
        if constructor === constructor2 && tParams.length === tParams2.length ⇒ wrapResult(tParams zip tParams2)

      case (d@DisjunctT(_, _, _), n@NamedConjunctT(_, _, _, _)) if isDisjunctionPart(d, n) ⇒ wrapResult(d.typeParams zip n.typeParams)
      //      case (n@NamedConjunctT(ncName, tParams2, _, _), d@DisjunctT(_, tParams, terms)) if terms.map(_.constructor) contains ncName ⇒ wrapResult(tParams zip tParams2)

      case (ConjunctT(terms), ConjunctT(terms2)) if terms.length === terms2.length ⇒ wrapResult(terms zip terms2)

      case (#->(head, body), #->(head2, body2)) ⇒ wrapResult(Seq((head, head2), (body, body2)))

      case (RecurseT(name, tParams), RecurseT(name2, tParams2)) if name === name2 ⇒ wrapResult(tParams zip tParams2)

      case (NamedConjunctT(constructor, tParams, _, _), NamedConjunctT(constructor2, tParams2, _, _))
        if constructor === constructor2 && tParams.length === tParams2.length ⇒ wrapResult(tParams zip tParams2)

      case _ ⇒ error
    }
    result
  }

  private[ch] def tParamString(tParams: Seq[TypeExpr], showDisjunctionParts: Boolean): String =
    if (tParams.isEmpty)
      ""
    else
      s"[${tParams.map(_.prettyPrintWithParentheses(0, showDisjunctionParts)).mkString(",")}]"

  private def makeImplication(tpe1: TypeExpr, tpe2: TypeExpr): TypeExpr = #->(tpe1, tpe2)

  implicit class WithImplication(tpe1: TypeExpr) {
    def ->:(tpe2: TypeExpr): TypeExpr = makeImplication(tpe2, tpe1) // right-associative operators are desugared in the opposite order: a ->: b is b.->:(a)
  }

}

// `terms` is the list of the defined parts of the disjunction. They may be of types `NamedConjunctT` or `RecurseT`.
final case class DisjunctT(constructor: String, override val typeParams: Seq[TypeExpr], terms: Seq[TypeExpr]) extends TypeExpr with NonAtomicTypeExpr

final case class ConjunctT(terms: Seq[TypeExpr]) extends TypeExpr with NonAtomicTypeExpr

final case class #->(head: TypeExpr, body: TypeExpr) extends TypeExpr with NonAtomicTypeExpr

final case class NothingT(name: String) extends TypeExpr with AtomicTypeExpr

final case class UnitT(name: String) extends TypeExpr with AtomicTypeExpr

// Type parameter. Use a short name for convenience.
final case class TP(name: String) extends TypeExpr with AtomicTypeExpr

final case class RecurseT(name: String, override val typeParams: Seq[TypeExpr]) extends TypeExpr with AtomicTypeExpr

final case class BasicT(name: String) extends TypeExpr with AtomicTypeExpr

/** This type expression represents a case class, treated as a named conjunction.
  * The `wrapped` is a type expression for the entire contents of the named conjunction. This can be a Unit, a single type, or a ConjunctT.
  * If `accessors` is empty and `wrapped` is Seq(), this is a case object. If `accessors` is empty and `wrapped` is Seq(UnitT), this is a case class with zero arguments.
  * If `accessors` in not empty, `wrapped` could be a ConjunctT with more than one part, or another type (e.g. Int or another NamedConjunctT or whatever else).
  */
final case class NamedConjunctT(constructor: String, override val typeParams: List[TypeExpr], accessors: List[String], wrapped: List[TypeExpr]) extends TypeExpr {
  override def caseObjectName: Option[String] = if (isAtomic && wrapped.isEmpty) Some(constructor) else None

  override def isAtomic: Boolean = typeParams.isEmpty && accessors.isEmpty
}

// Since we do not know how to work with arbitrary type constructors, we treat them as atomic types.
// The only derivation rule for atomic types is the identity axiom.
final case class ConstructorT(name: String, override val typeParams: List[TypeExpr]) extends TypeExpr with AtomicTypeExpr
