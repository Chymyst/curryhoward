package io.chymyst.ch

import io.chymyst.ch.Helper._
import io.chymyst.ch.data.Monoid
import io.chymyst.ch.data.Monoid.MonoidSyntax

import scala.annotation.tailrec

object TermExpr {

  /** Convenience method to produce the identity function term of a given type.
    *
    * Example usage:
    *
    * {{{
    *   def idAB[A, B] = TermExpr.id(typeExpr[A ⇒ B])
    *   idAB.prettyPrint == "x ⇒ x"
    *   idAB.t.prettyPrint == "(A ⇒ B) ⇒ A ⇒ B"
    * }}}
    *
    * @param t Type for the argument of the identity function.
    * @return A term for the identity function.
    */
  def id(t: TypeExpr): TermExpr = {
    val v = VarE("x", t)
    v =>: v
  }

  @tailrec
  private[ch] def simplifyWithEtaUntilStable(t: TermExpr): TermExpr = {
    val simplified = t.simplifyOnce(withEta = true)
    if (t === simplified) t else simplifyWithEtaUntilStable(simplified)
  }

  def foldMap[R: Monoid](termExpr: TermExpr)(p: PartialFunction[TermExpr, R]): R = {
    if (p isDefinedAt termExpr)
      p(termExpr)
    else {
      lazy val empty = Monoid.empty[R]

      def foldmap(termExpr: TermExpr): R = foldMap(termExpr)(p)

      termExpr match {
        case VarE(_, _) ⇒ empty
        case AppE(head, arg) ⇒ foldmap(head) combine foldmap(arg)
        case CurriedE(heads, body) ⇒ heads.map(foldmap).foldLeft(empty)(_ combine _) combine foldmap(body)
        case UnitE(_) ⇒ empty
        case NamedConjunctE(terms, _) ⇒ terms.map(foldmap).foldLeft(empty)(_ combine _)
        case ConjunctE(terms) ⇒ terms.map(foldmap).foldLeft(empty)(_ combine _)
        case ProjectE(_, term) ⇒ foldmap(term)
        case MatchE(term, cases) ⇒ foldmap(term) combine cases.map(foldmap).foldLeft(empty)(_ combine _)
        case DisjunctE(_, _, term, _) ⇒ foldmap(term)
      }
    }
  }

  /** Approximate size of the term.
    * The computation adds 1 every time any subterms are combined in any way.
    * For example, the size of the term `x ⇒ y ⇒ x` is 5.
    *
    * @param termExpr A term.
    * @return Number of elements in the term.
    */
  def size(termExpr: TermExpr): Int = {
    implicit val monoidIntSum: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = -1

      override def combine(x: Int, y: Int): Int = x + y + 1
    }
    foldMap(termExpr) {
      case VarE(_, _) | UnitE(_) ⇒ 1
    }
  }

  // Lambda-term syntax helper.
  def lambdaTerm(f: Any): Option[TermExpr] = // Try(WithLambdaTerm(f).lambdaTerm).toOption fails to compile
    f match {
      case g: Function0Lambda[_] ⇒ Some(g.lambdaTerm)
      case g: Function1Lambda[_, _] ⇒ Some(g.lambdaTerm)
      case g: Function2Lambda[_, _, _] ⇒ Some(g.lambdaTerm)
      case g: Function3Lambda[_, _, _, _] ⇒ Some(g.lambdaTerm)
      case _ ⇒ None
    }

  private[ch] def propositions(termExpr: TermExpr): Seq[VarE] = foldMap(termExpr) {
    case p@VarE(_, _) ⇒ Seq(p)
  }.distinct

  private val freshIdents = new FreshIdents("z")

  private val makeFreshNames: Iterator[String] = Iterator.iterate(freshIdents())(_ ⇒ freshIdents())

  private def allFreshNames(names1: Seq[String], names2: Seq[String], namesToExclude: Seq[String]): Seq[String] = {
    val requiredNumber = names1.length
    val allExcluded = (names1 ++ names2 ++ namesToExclude).toSet
    makeFreshNames.filterNot(allExcluded.contains).take(requiredNumber).toSeq
  }

  // Apply this curried function to a number of arguments at once.
  def applyCurried(curriedFunction: TermExpr, args: Seq[TermExpr]): TermExpr = {
    args.foldLeft[TermExpr](curriedFunction) { case (prev, arg) ⇒ AppE(prev, arg) }
  }

  // Compare terms up to renaming. Note: this is not alpha-conversion yet.
  def equiv(e1: TermExpr, e2: TermExpr): Boolean = e1 match {
    case CurriedE(heads1, body1) ⇒ e2 match {
      case CurriedE(heads2, body2) if heads1.length === heads2.length ⇒
        // rename all vars to new names
        val vars1 = heads1.map(_.name)
        val vars2 = heads2.map(_.name)
        val freshNames = allFreshNames(vars1, vars2, body1.freeVarNames ++ body2.freeVarNames)
        heads1.map(_.renameAllVars(vars1, freshNames)) === heads2.map(_.renameAllVars(vars2, freshNames)) &&
          equiv(body1.renameAllVars(vars1, freshNames), body2.renameAllVars(vars2, freshNames))
      case _ ⇒ false
    }
    case _ ⇒ e1 === e2
  }

  def substMap(termExpr: TermExpr)(p: PartialFunction[TermExpr, TermExpr]): TermExpr =
    if (p isDefinedAt termExpr)
      p(termExpr)
    else {
      def subst(termExpr: TermExpr): TermExpr = substMap(termExpr)(p)

      def substForCurriedHeads(varExpr: VarE): VarE = substMap(varExpr)(p) match {
        // Result must be a VarE, otherwise it's an error.
        case v: VarE ⇒ v
        case other ⇒ throw new Exception(s"Incorrect substitution of bound variable $varExpr by non-variable ${other.prettyPrint} in substMap(${termExpr.prettyPrint}){...}")
      }

      termExpr match {
        case AppE(head, arg) ⇒ AppE(subst(head), subst(arg))
        case CurriedE(heads, body) ⇒ CurriedE(heads.map(substForCurriedHeads), subst(body))
        case ConjunctE(terms) ⇒ ConjunctE(terms.map(subst))
        case NamedConjunctE(terms, tExpr) ⇒ NamedConjunctE(terms.map(subst), tExpr)
        case ProjectE(index, term) ⇒ ProjectE(index, subst(term))
        case MatchE(term, cases) ⇒ MatchE(subst(term), cases.map(subst))
        case DisjunctE(index, total, term, tExpr) ⇒ DisjunctE(index, total, subst(term), tExpr)
        case VarE(_, _) ⇒ termExpr
        case UnitE(_) ⇒ termExpr
      }
    }

  private def varMatchesType(thisVar: VarE, otherVar: VarE): Boolean = {
    thisVar.name === otherVar.name && (thisVar.t === otherVar.t || TypeExpr.isDisjunctionPart(thisVar.t, otherVar.t))
  }

  /** Replace all free occurrences of variable `replaceVar` by expression `byExpr` in `origExpr`.
    *
    * @param replaceVar A variable that may occur freely in `origExpr`.
    * @param byExpr     A new expression to replace all free occurrences of that variable.
    * @param origExpr   The original expression.
    * @return A new expression where the variable has been substituted.
    */
  def subst(replaceVar: VarE, byExpr: TermExpr, origExpr: TermExpr): TermExpr = {
    // Check that all instances of replaceVar in origExpr have the correct type.
    val badVars = origExpr.freeVars.filter(_.name === replaceVar.name).filterNot(varMatchesType(_, replaceVar))
    if (badVars.nonEmpty) {
      throw new Exception(s"In subst($replaceVar:${replaceVar.t.prettyPrint}, $byExpr, $origExpr), found variable(s) ${badVars.map(v ⇒ s"(${v.name}:${v.t.prettyPrint})").mkString(", ")} with incorrect type(s), expected variable type ${replaceVar.t.prettyPrint}")
    }
    // Do we need an alpha-conversion? Better be safe than sorry.
    val (convertedReplaceVar, convertedOrigExpr) = if (byExpr.usedVarNames contains replaceVar.name) {
      val convertedVar = VarE(freshIdents(), replaceVar.t)
      val convertedExpr = subst(replaceVar, convertedVar, origExpr)
      (convertedVar, convertedExpr)
    } else (replaceVar, origExpr)

    substMap(convertedOrigExpr) {
      case c@CurriedE(heads, _) if heads.exists(_.name === convertedReplaceVar.name) ⇒ c
      // If a variable from `heads` collides with `convertedReplaceVar`, we do not replace anything in the body because the variable occurs as non-free.

      case v@VarE(_, _) if varMatchesType(v, convertedReplaceVar) ⇒ byExpr
    }
  }

  def substTypeVar(replaceTypeVar: TP, byTypeExpr: TypeExpr, inExpr: TermExpr): TermExpr = substMap(inExpr) {
    case VarE(name, tExpr) ⇒ VarE(name, tExpr.substTypeVar(replaceTypeVar, byTypeExpr))
    case NamedConjunctE(terms, tExpr) ⇒ NamedConjunctE(terms.map(substTypeVar(replaceTypeVar, byTypeExpr, _)), tExpr.substTypeVar(replaceTypeVar, byTypeExpr).asInstanceOf[NamedConjunctT])
    case DisjunctE(index, total, term, tExpr) ⇒ DisjunctE(index, total, substTypeVar(replaceTypeVar, byTypeExpr, term), tExpr.substTypeVar(replaceTypeVar, byTypeExpr))
  }

  def substTypeVars(inExpr: TermExpr, substitutions: Map[TP, TypeExpr]): TermExpr = substMap(inExpr) {
    case VarE(name, tExpr) ⇒ VarE(name, tExpr.substTypeVars(substitutions))
    case NamedConjunctE(terms, tExpr) ⇒ NamedConjunctE(terms.map(substTypeVars(_, substitutions)), tExpr.substTypeVars(substitutions).asInstanceOf[NamedConjunctT])
    case DisjunctE(index, total, term, tExpr) ⇒ DisjunctE(index, total, substTypeVars(term, substitutions), tExpr.substTypeVars(substitutions))
  }

  def findAll[R](inExpr: TermExpr)(pred: PartialFunction[TermExpr, R]): Seq[R] = {
    Seq(inExpr).collect(pred) ++ (inExpr match {
      case VarE(_, _) ⇒ Seq()
      case AppE(head, arg) ⇒ findAll(head)(pred) ++ findAll(arg)(pred)
      case CurriedE(_, body) ⇒ findAll(body)(pred)
      case UnitE(_) ⇒ Seq()
      case NamedConjunctE(terms, _) ⇒ terms.filter(t ⇒ findAll(t)(pred).nonEmpty).flatMap(t ⇒ findAll(t)(pred))
      case ConjunctE(terms) ⇒ terms.filter(t ⇒ findAll(t)(pred).nonEmpty).flatMap(t ⇒ findAll(t)(pred))
      case ProjectE(_, term) ⇒ findAll(term)(pred)
      case MatchE(term, cases) ⇒ findAll(term)(pred) ++ cases.filter(t ⇒ findAll(t)(pred).nonEmpty).flatMap(t ⇒ findAll(t)(pred))
      case DisjunctE(_, _, term, _) ⇒ findAll(term)(pred)
    })
  }

  def findFirst[R](inExpr: TermExpr)(pred: PartialFunction[TermExpr, R]): Option[R] = {
    Some(inExpr).collect(pred).orElse {
      inExpr match {
        case VarE(_, _) ⇒ None
        case AppE(head, arg) ⇒ findFirst(head)(pred).orElse(findFirst(arg)(pred))
        case CurriedE(_, body) ⇒ findFirst(body)(pred)
        case UnitE(_) ⇒ None
        case NamedConjunctE(terms, _) ⇒ terms.find(t ⇒ findFirst(t)(pred).isDefined).flatMap(t ⇒ findFirst(t)(pred))
        case ConjunctE(terms) ⇒ terms.find(t ⇒ findFirst(t)(pred).isDefined).flatMap(t ⇒ findFirst(t)(pred))
        case ProjectE(_, term) ⇒ findFirst(term)(pred)
        case MatchE(term, cases) ⇒ findFirst(term)(pred).orElse(cases.find(t ⇒ findFirst(t)(pred).isDefined).flatMap(t ⇒ findFirst(t)(pred)))
        case DisjunctE(_, _, term, _) ⇒ findFirst(term)(pred)
      }
    }
  }

  private def sameConstructor(t1: TypeExpr, t2: TypeExpr): Boolean = t1 match {
    case NamedConjunctT(c1, _, _, _) ⇒ t2 match {
      case NamedConjunctT(c2, _, _, _) ⇒ c1 === c2
      case _ ⇒ false
    }
    case DisjunctT(c1, _, _) ⇒ t2 match {
      case DisjunctT(c2, _, _) ⇒ c1 === c2
      case _ ⇒ false
    }
    case _ ⇒ false
  }

  private def atLeastOnce(x: Int): Int = math.max(x - 1, 0)

  private[ch] val monoidIntStandard: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(x: Int, y: Int): Int = x + y
  }

  implicit def monoidSeq[T]: Monoid[Seq[T]] = new Monoid[Seq[T]] {
    override def empty: Seq[T] = Seq()

    override def combine(x: Seq[T], y: Seq[T]): Seq[T] = x ++ y
  }

  implicit def monoidSet[T]: Monoid[Set[T]] = new Monoid[Set[T]] {
    override def empty: Set[T] = Set()

    override def combine(x: Set[T], y: Set[T]): Set[T] = x ++ y
  }

  // How many times each function uses its argument. Only counts when an argument is used more than once.
  private[ch] def argsMultiUseCountDeep(inExpr: TermExpr): Int = {
    implicit val monoidInt: Monoid[Int] = monoidIntStandard
    foldMap(inExpr) {
      case c@CurriedE(_, body) ⇒ argsMultiUseCountShallow(c) + argsMultiUseCountDeep(body)
    }
  }

  private[ch] def argsMultiUseCountShallow(inExpr: TermExpr): Int = inExpr match {
    case CurriedE(heads, body) ⇒ heads.map(head ⇒ atLeastOnce(body.varCount(head.name))).sum
    case _ ⇒ 0
  }

  // Count all "case" clauses.
  private[ch] def caseClausesCount(inExpr: TermExpr): Int = {
    implicit val monoidInt: Monoid[Int] = TermExpr.monoidIntStandard
    TermExpr.foldMap(inExpr) {
      case MatchE(term, cases) ⇒ cases.length + caseClausesCount(term) + cases.map(caseClausesCount).sum
    }
  }

  private[ch] def unequalTupleSize(inExpr: TermExpr): Double = {
    implicit val monoidDouble: Monoid[Double] = TermExpr.monoidDouble
    TermExpr.foldMap(inExpr) {
      case NamedConjunctE(terms, _) ⇒
        val sizeDifference = terms.groupBy(_.t) // Map[TypeExpr, Seq[TermExpr]]
          .filter(_._2.length > 1)
          .map { case (_, ts) ⇒
            val sizes = ts.map(TermExpr.size)
            sizes.max - sizes.min
          }.sum
        val sizeDifferenceInTerms = terms.map(unequalTupleSize).sum

        (sizeDifference + sizeDifferenceInTerms) / TermExpr.size(inExpr).toDouble
    }
  }

  implicit val monoidDouble: Monoid[Double] = new Monoid[Double] {
    override def empty: Double = 0.0

    override def combine(x: Double, y: Double): Double = x + y
  }

  private[ch] def conjunctionPermutationScore(inExpr: TermExpr): Double = foldMap(inExpr) {
    case NamedConjunctE(terms, tExpr) ⇒
      terms.map(conjunctionPermutationScore).sum +
        terms.zipWithIndex.flatMap { case (t, i) ⇒
          // Only count projections from terms of exactly the same type as this `NamedConjunctE`.
          findFirst(t) { case ProjectE(index, term) if sameConstructor(term.t, tExpr) ⇒
            conjunctionPermutationScore(term) / terms.length.toDouble +
              (if (index === i) 0 else 1)
          }
        }.sum
    case ConjunctE(terms) ⇒
      terms.zipWithIndex.flatMap { case (t, i) ⇒
        findFirst(t) { case ProjectE(index, term) if (term.t match {
          case ConjunctT(_) ⇒ true
          case _ ⇒ false
        }) ⇒
          conjunctionPermutationScore(term) / terms.length.toDouble +
            (if (index === i) 0 else 1)
        }
      }.sum
  }

  private[ch] def disjunctionPermutationScore(inExpr: TermExpr): Double = foldMap(inExpr) {
    case MatchE(term, cases) ⇒
      disjunctionPermutationScore(term) +
        cases.zipWithIndex.flatMap { case (t, i) ⇒
          // Only count disjunction constructions into terms of exactly the same type as the `term` being matched.
          findFirst(t) { case DisjunctE(index, _, t2, tExpr) if sameConstructor(term.t, tExpr) ⇒
            disjunctionPermutationScore(t2) / cases.length.toDouble +
              (if (index === i) 0 else 1)
          }
        }.sum
  }

  private[ch] def unusedArgs(termExpr: TermExpr): Set[String] = {
    implicit val monoidSetString: Monoid[Set[String]] = new Monoid[Set[String]] {
      override def empty: Set[String] = Set()

      override def combine(x: Set[String], y: Set[String]): Set[String] = x ++ y
    }
    foldMap(termExpr) {
      case CurriedE(heads, body) ⇒ (heads.map(_.name).toSet -- body.freeVarNames) ++ unusedArgs(body)
      case MatchE(term, cases) ⇒ unusedArgs(term) ++ cases.flatMap {
        // The unused heads in this CurriedE are counted separately by `unusedMatchClauseVars`.
        case CurriedE(List(_), body) ⇒ unusedArgs(body)
        case c ⇒ unusedArgs(c)
      }.toSet
    }

  }

  private[ch] def roundFactor(x: Double): Int = math.round(x * 10000).toInt

  /** Generate all necessary fresh variables for equality checking of functions that consume disjunction types.
    *
    * @param typeExpr The type of the argument expression.
    * @return A sequence of [[TermExpr]] values containing the necessary fresh variables.
    */
  def subtypeVars(typeExpr: TypeExpr): Seq[TermExpr] = typeExpr match {
    case dt@DisjunctT(constructor, typeParams, terms) ⇒ terms.zipWithIndex.flatMap { case (t, i) ⇒ subtypeVars(t).map(v ⇒ DisjunctE(i, terms.length, v, dt)) }
    case nct@NamedConjunctT(constructor, typeParams, accessors, wrapped) ⇒
      TheoremProver.explode(wrapped.map(subtypeVars)).map(NamedConjunctE(_, nct))
    case _ ⇒ Seq(VarE(freshIdents(), typeExpr))
  }

  /** Extensional equality check. If the term expressions are functions, fresh variables are substituted as arguments and the results are compared with `equiv`.
    *
    * @param termExpr1 The first term.
    * @param termExpr2 The second term.
    * @return `true` if the terms are extensionally equal.
    */
  def extEqual(termExpr1: TermExpr, termExpr2: TermExpr): Boolean = {
    val t1 = termExpr1.simplify
    val t2 = termExpr2.simplify
    (t1.t === t2.t) && (
      (t1 equiv t2) || {
        println(s"DEBUG: checking extensional equality of ${t1.prettyPrint} and ${t2.prettyPrint}")
        (t1, t2) match {
          case (CurriedE(h1 :: _, _), CurriedE(_ :: _, _)) ⇒
            subtypeVars(h1.t).forall { term ⇒
              val result = extEqual(t1(term), t2(term))
              if (!result) println(s"DEBUG: found inequality after substituting term ${term.prettyPrint}")
              result
            }
          case _ ⇒ false
        }
      }
      )
  }
}

sealed trait TermExpr {

  /** Provide :@ syntax for term application with automatic alpha-conversion for type variables in the function head.
    */
  def :@(terms: TermExpr*): TermExpr = t match {
    case #->(head, _) ⇒
      val arguments = if (terms.length === 1) terms.head else ConjunctE(terms)
      TypeExpr.leftUnify(head, arguments.t, t) match {
        case Left(errorMessage) ⇒
          throw new Exception(errorMessage)
        case Right(substitutions) ⇒
          AppE(substTypeVars(substitutions), arguments)
      }
    case _ ⇒ throw new Exception(s"Call to `:@` is invalid because the head term $this of type ${t.prettyPrint} is not a function")
  }

  /** Provide syntax for function composition.
    */
  def andThen(otherTerm: TermExpr): TermExpr = (this.t, otherTerm.t) match {
    case (#->(head1, body1), #->(head2, _)) ⇒
      if (head2 === body1) {
        val newVar = VarE(TheoremProver.freshVar(), head1)
        newVar =>: otherTerm(this (newVar))
      } else throw new Exception(s"Call to `.andThen` is invalid because the function types (${this.t.prettyPrint} and ${otherTerm.t.prettyPrint}) do not match")
    case _ ⇒ throw new Exception(s"Call to `.andThen` is invalid because the type of one of the arguments (${this.t.prettyPrint} and ${otherTerm.t.prettyPrint}) is not of a function type")
  }

  // Function composition with automatic alpha-conversion for type variables in the first function.
  def :@@(otherTerm: TermExpr): TermExpr = (this.t, otherTerm.t) match {
    case (#->(_, body1), #->(head2, _)) ⇒
      TypeExpr.leftUnify(body1, head2, this.t) match {
        case Left(errorMessage) ⇒
          throw new Exception(s"Call to `:@@` is invalid because the function types (${this.t.prettyPrint} and ${otherTerm.t.prettyPrint}) do not match: $errorMessage")
        case Right(substitutions) ⇒
          val substitutedTerm1 = substTypeVars(substitutions)
          val newVar = VarE(TheoremProver.freshVar(), substitutedTerm1.t.asInstanceOf[#->].head)
          newVar =>: otherTerm(substitutedTerm1(newVar))
      }
    case _ ⇒ throw new Exception(s"Call to `:@@` is invalid because the type of one of the arguments (${this.t.prettyPrint} and ${otherTerm.t.prettyPrint}) is not of a function type")
  }

  // Function composition with automatic alpha-conversion for type variables in the second function.
  def @@:(otherTerm: TermExpr): TermExpr = (otherTerm.t, this.t) match {
    case (#->(head1, body1), #->(head2, _)) ⇒
      TypeExpr.leftUnify(head2, body1, this.t) match {
        case Left(errorMessage) ⇒
          throw new Exception(s"Call to `:@@` is invalid because the function types (${this.t.prettyPrint} and ${otherTerm.t.prettyPrint}) do not match: $errorMessage")
        case Right(substitutions) ⇒
          val substitutedTerm = substTypeVars(substitutions)
          val newVar = VarE(TheoremProver.freshVar(), head1)
          newVar =>: substitutedTerm(otherTerm(newVar))
      }
    case _ ⇒ throw new Exception(s"Call to `@@:` is invalid because the type of one of the arguments (${this.t.prettyPrint} and ${otherTerm.t.prettyPrint}) is not of a function type")
  }

  // Syntax helpers.
  def =>:(y: VarE): TermExpr = CurriedE(List(y), this)

  def apply(i: Int): TermExpr = t match {
    case NamedConjunctT(_, _, accessors, _) ⇒
      if (i >= 0 && i < accessors.length)
        ProjectE(i, this)
      else throw new Exception(s".apply($i) is undefined since this conjunction type has only ${accessors.length} parts")
    case _ ⇒ throw new Exception(s".apply(index: Int) is defined only on conjunction types while this is ${t.prettyPrint}")
  }

  def apply(acc: String): TermExpr = t match {
    case NamedConjunctT(_, _, accessors, _) ⇒
      val i = accessors.indexOf(acc)
      if (i >= 0)
        ProjectE(i, this)
      else throw new Exception(s".apply($acc) is undefined since this conjunction type does not support this accessor (supported accessors: ${accessors.mkString(", ")})")
    case _ ⇒ throw new Exception(s".apply(accessor: String) is defined only on conjunction types while this is ${t.prettyPrint}")
  }

  def apply(terms: TermExpr*): TermExpr = t match {
    case #->(_, _) ⇒
      val arguments = if (terms.length === 1) terms.head else ConjunctE(terms)
      AppE(this, arguments)
    case _: ConjunctT | _: NamedConjunctT | _: DisjunctT | _: UnitT ⇒ t.apply(terms: _*)
    case _ ⇒ throw new Exception(s"t.apply(...) is not defined for the term $this of type ${t.prettyPrint}")
  }

  def cases(cases: TermExpr*): TermExpr = t match {
    case DisjunctT(_, _, termTypes) ⇒
      val typesOfCaseBodies = cases.zip(termTypes).collect { case (CurriedE(head :: _, body), t) if head.t === t ⇒ body.t }
      if (cases.length === termTypes.length && typesOfCaseBodies.length === cases.length && typesOfCaseBodies.toSet.size === 1)
        MatchE(this, cases.toList)
      else throw new Exception(s"Case match on ${t.prettyPrint} must use a sequence of ${termTypes.length} functions with matching types of arguments (${termTypes.map(_.prettyPrint).mkString("; ")}) and bodies, but have ${cases.map(_.t.prettyPrint).mkString("; ")}")
    case _ ⇒ throw new Exception(s".cases() is not defined for the term $this of type ${t.prettyPrint}")
  }

  def equiv(y: TermExpr): Boolean = simplify.prettyRename === y.simplify.prettyRename

  def substTypeVar(from: TermExpr, to: TermExpr): TermExpr = from.t match {
    case tp@TP(_) ⇒ TermExpr.substTypeVar(tp, to.t, this)
    case _ ⇒ throw new Exception(s"substTypeVar requires a type variable as type of expression $from, but found type ${from.t.prettyPrint}")
  }

  def substTypeVars(substitutions: Map[TP, TypeExpr]): TermExpr = TermExpr.substTypeVars(this, substitutions)

  // Need to split the tuple into parts because Ordering[] is undefined on tuples > 9
  private[ch] lazy val informationLossScore = (
    TermExpr.unusedArgs(this).size
    , TermExpr.roundFactor(unusedTupleParts.toDouble + unusedMatchClauseVars)
    , TermExpr.roundFactor(TermExpr.conjunctionPermutationScore(this) + TermExpr.disjunctionPermutationScore(this))
    , TermExpr.argsMultiUseCountShallow(this)
    , TermExpr.argsMultiUseCountDeep(this)
    //    , unequallyUsedTupleParts // experimental
    //        , TermExpr.roundFactor(TermExpr.unequalTupleSize(this))
    //    , TermExpr.caseClausesCount(this)
    //    , TermExpr.size(this) // Should not use this.
  )

  def t: TypeExpr

  lazy val prettyPrint: String = prettyPrintWithParentheses(0)

  lazy val prettyRenamePrint: String = prettyRename.prettyPrint

  override lazy val toString: String = this match {
    case VarE(name, _) ⇒ name
    case AppE(head, arg) ⇒ s"($head $arg)"
    case CurriedE(heads, body) ⇒ s"\\(${heads.map(h ⇒ "(" + h.name + ":" + h.t.prettyPrint + ")").mkString(" ⇒ ")} ⇒ $body)"
    case UnitE(_) ⇒ "()"
    case ConjunctE(terms) ⇒ "(" + terms.map(_.toString).mkString(", ") + ")"
    case NamedConjunctE(terms, tExpr) ⇒
      val prefix = if (tExpr.isAtomic) "<co>" else ""
      s"$prefix${tExpr.constructor.toString}(${terms.map(_.toString).mkString(", ")})"
    case ProjectE(index, term) ⇒ term.toString + "." + term.accessor(index)
    case MatchE(term, cases) ⇒ "(" + term.toString + " match { " + cases.map(_.toString).mkString("; ") + "})"
    case DisjunctE(index, total, term, _) ⇒
      val leftZeros = Seq.fill(index)("0")
      val leftZerosString = if (leftZeros.isEmpty) "" else " + "
      val rightZeros = Seq.fill(total - index - 1)("0")
      val rightZerosString = if (rightZeros.isEmpty) "" else " + "
      "(" + leftZeros.mkString(" + ") + leftZerosString + term.toString + rightZerosString + rightZeros.mkString(" + ") + ")"
  }

  private[ch] def prettyPrintWithParentheses(level: Int): String = this match {
    case VarE(name, _) ⇒ s"$name"
    case AppE(head, arg) ⇒
      val h = head.prettyPrintWithParentheses(1)
      val b = arg.prettyPrintWithParentheses(2)
      if (level > 1) s"($h $b)" else s"$h $b"
    case CurriedE(heads, body) ⇒
      val r = s"${heads.map(_.prettyPrintWithParentheses(0)).mkString(" ⇒ ")} ⇒ ${body.prettyPrintWithParentheses(0)}"
      if (level > 0) s"($r)" else r
    case UnitE(_) ⇒ "1"
    case ConjunctE(terms) ⇒ "(" + terms.map(_.prettyPrintWithParentheses(0)).mkString(", ") + ")"
    case NamedConjunctE(terms, tExpr) ⇒ s"${tExpr.constructor.toString}(${terms.map(_.prettyPrintWithParentheses(0)).mkString(", ")})"
    case ProjectE(index, term) ⇒ term.prettyPrintWithParentheses(1) + "." + term.accessor(index)
    case MatchE(term, cases) ⇒
      val r = term.prettyPrintWithParentheses(1) + " match { " + cases.map(_.prettyPrintWithParentheses(0)).mkString("; ") + " }"
      if (level > 0) s"($r)" else r
    case DisjunctE(index, total, term, _) ⇒
      val leftZeros = Seq.fill(index)("0")
      val leftZerosString = if (leftZeros.isEmpty) "" else " + "
      val rightZeros = Seq.fill(total - index - 1)("0")
      val rightZerosString = if (rightZeros.isEmpty) "" else " + "
      "(" + leftZeros.mkString(" + ") + leftZerosString + term.prettyPrintWithParentheses(0) + rightZerosString + rightZeros.mkString(" + ") + ")"
  }

  private def prettyVars: Iterator[String] = for {
    number ← Iterator.single("") ++ Iterator.from(1).map(_.toString)
    letter ← ('a' to 'z').toIterator
  } yield s"$letter$number"

  private[ch] lazy val renameBoundVars: TermExpr = TermExpr.substMap(this) {
    case CurriedE(heads, body) ⇒
      val oldAndNewVars = heads.map { v ⇒ (v, VarE(TermExpr.freshIdents(), v.t)) }
      val renamedBody = oldAndNewVars.foldLeft(body.renameBoundVars) { case (prev, (oldVar, newVar)) ⇒
        TermExpr.subst(oldVar, newVar, prev)
      }
      CurriedE(oldAndNewVars.map(_._2), renamedBody)
  }

  lazy val prettyRename: TermExpr = renameBoundVars.prettyRenameVars

  private lazy val prettyRenameVars: TermExpr = {
    val oldVars = usedVarNames diff freeVarNames // Do not rename free variables, since this leads to incorrect code!
    // Use a `Seq` here rather than a `Set` for the list of variable names.
    // This achieves deterministic renaming, which is important for checking that different terms are equivalent up to renaming.
    val newVars = prettyVars.take(oldVars.length).toSeq
    renameAllVars(oldVars, newVars)
  }

  private[ch] def accessor(index: Int): String = t match {
    case NamedConjunctT(_, _, accessors, _) ⇒ accessors(index).toString
    case ConjunctT(_) ⇒ s"_${index + 1}"
    case _ ⇒ throw new Exception(s"Internal error: Cannot perform projection for term $toString : ${t.prettyPrint} because its type is not a conjunction")
  }

  // Try optimizing the recursive `simplify` operation.
  private[ch] var simplified: Boolean = false

  lazy val simplify: TermExpr = if (simplified) this else {
    val result = TermExpr.simplifyWithEtaUntilStable(this)

    result.simplified = true

    result
  }

  private[ch] def simplifyOnceInternal(withEta: Boolean = false): TermExpr = this

  final private[ch] def simplifyOnce(withEta: Boolean = false): TermExpr = if (simplified) this else {
    val result = if (withEta) simplifyOnceWithEta else simplifyOnceWithoutEta
    if (result === this) {
      simplified = true
      this
    } else result
  }

  private lazy val simplifyOnceWithEta = simplifyOnceInternal(withEta = true)

  private lazy val simplifyOnceWithoutEta = simplifyOnceInternal(withEta = false)

  private[ch] lazy val unusedMatchClauseVars: Double = {
    import TermExpr.monoidDouble
    TermExpr.foldMap[Double](this) {
      case MatchE(_, cases) ⇒ cases.map {
        case CurriedE(List(prop), body) ⇒
          val thisValue = if (body.freeVarNames contains prop.name) 0.0 else 1.0
          thisValue + body.unusedMatchClauseVars
        case c ⇒ c.unusedMatchClauseVars
      }.sum / cases.length.toDouble
    }
  }

  private[ch] lazy val unequallyUsedTupleParts: Int = usedTuplePartsSeq
    .groupBy(_._1) // Map[TermExpr, Seq[(TermExpr, Int)]]
    .mapValues(_.map(_._2)) // Map[TermExpr, Seq[Int]]
    .map { case (term@_, partsUsed) ⇒
    // e.g. partsUsed = List(1, 1, 1, 1, 1, 2, 2, 2, 3)
    // We compute 5 - 1 = 4
    val sorted = partsUsed.groupBy(identity[Int]) // Map[Int, Seq[Int]]
      .mapValues(_.length) // Map[Int, Int]
      .values.toSeq.sorted
    sorted.max - sorted.min // It is guaranteed that the sequence is not empty because we used groupBy
  }.sum

  private[ch] lazy val unusedTupleParts: Int = usedTuplePartsSeq
    .groupBy(_._1) // Map[TermExpr, Seq[(TermExpr, Int)]]
    .mapValues(_.map(_._2).distinct) // Map[TermExpr, Seq[Int]]
    .map { case (term, partsUsed) ⇒
    val totalParts = term.t.conjunctSize
    totalParts - partsUsed.length
  }.count(_ > 0)

  /** Shows how many times each tuple part was used for any given term.
    * Example: `List((a,1), (a,2), (a,1), (a,1), (a,1))`
    */
  private[ch] lazy val usedTuplePartsSeq: Seq[(TermExpr, Int)] = {
    TermExpr.foldMap(this) {
      case ProjectE(index, term) ⇒ Seq((term, index + 1)) ++ term.usedTuplePartsSeq
    }
  }

  lazy val freeVarNames: Seq[String] = {
    import TermExpr.monoidSeq
    TermExpr.foldMap(this) {
      case VarE(name, _) ⇒ Seq(name)
      case CurriedE(heads, body) ⇒ body.freeVarNames.filterNot(heads.map(_.name).toSet.contains)
      case p: ProjectE ⇒ p.getProjection.map(_.freeVarNames).getOrElse(p.term.freeVarNames)
    }.distinct
  }

  lazy val usedVarNames: Seq[String] = {
    import TermExpr.monoidSeq
    TermExpr.foldMap(this) {
      case VarE(name, _) ⇒ Seq(name)
      case p: ProjectE ⇒ p.getProjection.map(_.usedVarNames).getOrElse(p.term.usedVarNames)
    }.distinct
  }

  lazy val freeVars: Set[VarE] = {
    import TermExpr.monoidSet
    TermExpr.foldMap(this) {
      case v@VarE(_, _) ⇒ Set(v)
      case CurriedE(heads, body) ⇒ body.freeVars -- heads.toSet
      case p: ProjectE ⇒ p.getProjection.map(_.freeVars).getOrElse(p.term.freeVars)
    }
  }

  def varCount(varName: String): Int = {
    implicit val monoidInt: Monoid[Int] = TermExpr.monoidIntStandard
    TermExpr.foldMap(this) {
      case VarE(name, _) ⇒ if (name === varName) 1 else 0
    }
  }

  // Rename all variable at once *everywhere* in the expression. (This is not an alpha-conversion!)
  def renameAllVars(oldNames: Seq[String], newNames: Seq[String]): TermExpr = {
    renameAllVars(oldNames.zip(newNames).toMap)
  }

  def renameAllVars(oldAndNewNames: Map[String, String]): TermExpr = {
    TermExpr.substMap(this) {
      case VarE(oldName, tExpr) ⇒
        val replacedName = oldAndNewNames.getOrElse(oldName, oldName)
        VarE(replacedName, tExpr)
    }
  }
}

final case class VarE(name: String, t: TypeExpr) extends TermExpr

final case class AppE(head: TermExpr, arg: TermExpr) extends TermExpr {

  // The type of AppE is computed from the types of its arguments.
  // Make this a `val` to catch bugs early.
  val t: TypeExpr = head.t match {
    case hd #-> body if hd === arg.t ⇒ body
    case _ ⇒ throw new Exception(s"Internal error: Invalid head type in application $this: ${head.t.prettyPrint} must be a function with argument type ${arg.t.prettyPrint}")
  }

  private[ch] override def simplifyOnceInternal(withEta: Boolean): TermExpr = {
    val headSimpl = head.simplifyOnce(withEta)
    val argSimpl = arg.simplifyOnce(withEta)

    headSimpl match {
      case CurriedE(heads, body) ⇒
        // substitute arg as first variable into body and return CurriedE unless we have no more arguments, else return new body
        val result: TermExpr = TermExpr.subst(heads.head, argSimpl, body.simplifyOnce(withEta)).simplifyOnce(withEta)
        heads.drop(1) match {
          case Nil ⇒ result
          case h ⇒ CurriedE(h, result)
        }
      case _ ⇒ this.copy(head = headSimpl, arg = argSimpl)
    }
  }
}

// The order of `heads` is straight, so `CurriedE(List(x1, x2, x3), body)` represents the term `x1 -> x2 -> x2 -> body`
final case class CurriedE(heads: List[VarE], body: TermExpr) extends TermExpr {
  private val headsLength = heads.length

  // The type is t1 -> t2 -> t3 -> b; here `heads` = List(t1, t2, t3).
  def t: TypeExpr = heads.reverse.foldLeft(body.t) { case (prev, head) ⇒ head.t ->: prev }

  private[ch] override def simplifyOnceInternal(withEta: Boolean): TermExpr = {
    body.simplifyOnce(withEta) match {
      // Check for eta-contraction: simplify x ⇒ y ⇒ ... ⇒ z ⇒ a ⇒ f a into x ⇒ y ⇒ ... ⇒ z ⇒ f.
      // Here fHead = a and fBody = f; the last element of `heads` must be equal to `a`
      case AppE(fHead, fBody) if withEta &&
        headsLength > 0 && {
        val lastHead = heads(headsLength - 1)
        lastHead.asInstanceOf[TermExpr] === fBody &&
          !fHead.freeVarNames.contains(lastHead.name) // Cannot replace a ⇒ f(... a ... ) a by f (... a ...)!
      } ⇒
        if (headsLength > 1)
          CurriedE(heads.slice(0, headsLength - 1), fHead)
        else
          fHead // no more function arguments left

      // Simplify nested CurriedE(CurriedE()) into a flat CurriedE().
      case CurriedE(heads1, body1) ⇒ CurriedE(heads ++ heads1, body1)

      case simplifiedBody ⇒ if (heads.isEmpty) simplifiedBody else this.copy(body = simplifiedBody)
    }
  }

}

final case class UnitE(t: TypeExpr) extends TermExpr

final case class NamedConjunctE(terms: Seq[TermExpr], t: NamedConjunctT) extends TermExpr {
  private[ch] override def simplifyOnceInternal(withEta: Boolean): TermExpr = {
    val simplifiedTerms = terms.map(_.simplifyOnce(withEta))
    // Detect the identity pattern:
    // NamedConjunctE(Seq(ProjectE(0, tt : N), ProjectE(1, tt: N), ...), N) with the same term `tt`
    val projectedTerms = simplifiedTerms.zipWithIndex.collect {
      case (ProjectE(j, tt), i) if i === j && tt.t === t ⇒ tt
    }
    projectedTerms.headOption match {
      case Some(pt) if projectedTerms.size === terms.size && projectedTerms.toSet.size === 1 ⇒ pt
      case _ ⇒ this.copy(terms = simplifiedTerms)
    }
  }
}

// This is now used only for java-style arg groups. A tuple is represented by a NamedConjunctE.
final case class ConjunctE(terms: Seq[TermExpr]) extends TermExpr {
  def t: TypeExpr = ConjunctT(terms.map(_.t))

  private[ch] override def simplifyOnceInternal(withEta: Boolean): TermExpr = this.copy(terms = terms.map(_.simplifyOnce(withEta)))
}

// The `term` should be a ConjunctT or a NamedConjunctT
final case class ProjectE(index: Int, term: TermExpr) extends TermExpr {
  def getProjection: Option[TermExpr] = term match {
    case c: ConjunctE ⇒ Some(c.terms(index))
    case c: NamedConjunctE ⇒ Some(c.terms(index))
    case _ ⇒ None
  }

  override def t: TypeExpr = term.t match {
    case ConjunctT(terms) ⇒ terms(index)
    case NamedConjunctT(constructor, _, _, wrapped) ⇒ wrapped match {
      case Nil if index === 0 ⇒ UnitT(constructor)
      case _ :: _ ⇒ wrapped(index)
      // Otherwise it is an error!
      case _ ⇒ throw new Exception(s"Internal error: Invalid projection to index $index for a named conjunct $term : ${term.t.prettyPrint} with multiplicity 1")
    }
    case _ ⇒ throw new Exception(s"Internal error: Invalid projection term $term whose type ${term.t.prettyPrint} is not a conjunction")
  }

  private[ch] override def simplifyOnceInternal(withEta: Boolean): TermExpr = term.simplifyOnce(withEta) match {
    case ConjunctE(terms) ⇒ terms(index).simplifyOnce(withEta)
    case NamedConjunctE(terms, _) ⇒ terms(index).simplifyOnce(withEta)
    case MatchE(t, cases) ⇒
      MatchE(t, cases.map {
        case CurriedE(List(propE), body) ⇒
          CurriedE(List(propE), ProjectE(index, body).simplifyOnce(withEta))
        case wrongTerm ⇒ throw new Exception(s"Internal error: MatchE contains $wrongTerm instead of CurriedE with a single argument")
      })
    case t ⇒ this.copy(term = t)
  }
}

/** Match a disjunction term of type A1 + A2 + ... + An against a list of n functions.
  * The type of the result of each function must be the same, and will be the type of this term.
  * This represents code of the form
  *
  * {{{
  *   term match {
  *     case a1: A1 ⇒ ...
  *     case a2: A2 ⇒ ...
  *     ...
  *     case an: An ⇒ ...
  *   }
  * }}}
  *
  * @param term  Term to be matched. Must be of disjunction type.
  * @param cases List of "case functions" of types A1 ⇒ Z, ..., An ⇒ Z.
  *              Each "case function" must be a function of the form CurriedE(PropE(_) :: _, _),
  *              that is, with at least one argument.
  */
final case class MatchE(term: TermExpr, cases: List[TermExpr]) extends TermExpr {
  override lazy val t: TypeExpr = cases match {
    case te :: tail ⇒
      te match {
        case CurriedE(List(_), body) ⇒
          val tpe = body.t
          if (tail.exists(_.t.asInstanceOf[#->].body !== tpe))
            throw new Exception(s"Internal error: unequal expression types in cases for $this")
          else
            tpe

        // TODO: this will throw an exception if we try to evaluate tExpr after simplifying,
        // when simplifying will bring together several arguments into a single CurriedE().
        // Instead of throwing an exception, we need to do something reasonable in this case.
        case _ ⇒ throw new Exception(s"Internal error: `case` expression for $this must contain functions of one argument")
      }
    case Nil ⇒ throw new Exception(s"Internal error: empty list of cases for $this")
  }

  private[ch] override def simplifyOnceInternal(withEta: Boolean): TermExpr = {
    val ncases = cases.length
    term.simplifyOnce(withEta) match {
      // Match a fixed part of the disjunction; can be simplified to just one clause.
      // Example: Left(a) match { case Left(x) => f(x); case Right(y) => ... } can be simplified to just f(a).
      case DisjunctE(index, total, termInjected, _) ⇒
        if (total === ncases) {
          AppE(cases(index).simplifyOnce(withEta), termInjected).simplifyOnce(withEta)
        } else throw new Exception(s"Internal error: MatchE with $ncases cases applied to DisjunctE with $total parts, but must be of equal size")

      // Match of an inner match, can be simplified to a single match.
      // Example: (q match { case Left(x) ⇒ ...; case Right(y) ⇒ ... }) match { case ... ⇒ ... }
      // can be simplified to q match { case Left(x) ⇒ ... match { case ... ⇒ ...}; case Right(y) ⇒ ... match { case ... ⇒ ... } }
      case MatchE(innerTerm, innerCases) ⇒
        MatchE(innerTerm, innerCases map { case CurriedE(List(head), body) ⇒ CurriedE(List(head), MatchE(body, cases)) })
          .simplifyOnce(withEta)

      // Detect the identity patterns:
      // MatchE(_, List(a ⇒ DisjunctE(0, total, a, _), a ⇒ DisjunctE(1, total, a, _), ...))
      // MatchE(_, a: T1 ⇒ DisjunctE(i, total, NamedConjunctE(List(ProjectE(0, a), Project(1, a), ...), T1), ...), _)
      case termSimplified ⇒

        // Replace redundant matches on the same term, can be simplified by eliminating one match subexpresssion.
        // Example: q match { case x ⇒ q match { case y ⇒ b; case other ⇒ ... } ... }
        // We already know that q was matched as Left(x). Therefore, we can replace y by x in b and remove the `case other` clause altogether.
        // Doing a .renameBoundVars on the cases leads to infinite loops somewhere due to incorrect alpha-conversion.
        val casesSimplified = cases.map(_.simplifyOnce(withEta))
          /*
          .zipWithIndex.map { case (c@CurriedE(List(headVar), _), i) ⇒
          TermExpr.substMap(c) {
            case MatchE(otherTerm, otherCases) if otherTerm === termSimplified ⇒
              // We already matched `otherTerm`, and we are now in case `c`, which is `case x ⇒ ...`.
              // Therefore we can discard any of the `otherCases` except the one corresponding to `c`.
              // We can replace the `q match { case y ⇒ b; ...}` by `b` after replacing `x` by `y` in `b`.
              val remainingCase = otherCases(i)
              val result = AppE(remainingCase, headVar).simplifyOnce(withEta)
              //            println(s"DEBUG: replacing ${MatchE(otherTerm, otherCases)} by $result in ${c.simplifyOnce(withEta)}")
              result
          }
        }
        */
        if (casesSimplified.nonEmpty && {
          casesSimplified.zipWithIndex.forall {
            // Detect a ⇒ a pattern
            case (CurriedE(List(head@VarE(_, _)), body@VarE(_, _)), _)
              if head.name === body.name
            ⇒ true
            case (CurriedE(List(head@VarE(_, _)), DisjunctE(i, len, x, _)), ind)
              if x === head && len === ncases && ind === i
            ⇒ true
            case (CurriedE(List(head@VarE(_, headT)), DisjunctE(i, len, NamedConjunctE(projectionTerms, conjT), _)), ind) ⇒
              len === ncases && ind === i && headT === conjT &&
                projectionTerms.zipWithIndex.forall {
                  case (ProjectE(k, head1), j) if k === j && head1 === head ⇒ true
                  case _ ⇒ false
                }
            case _ ⇒ false
          }
        }) {
          // We detected an identity function of the form term match { case a => a; case b => b; etc.}
          // so we can just replace that by `term`.
          term
        } else {
          // Detect the constant pattern:
          // MatchE(_, {f, f, ..., f}) where each clause is the same expression `f`, up to equivalence, and ignores its argument.
          // Then the entire term can be replaced by f.
          val bodyTerms: Set[Option[TermExpr]] = casesSimplified.map {
            case c@CurriedE(List(VarE(name, _)), body) if TermExpr.unusedArgs(c) contains name ⇒ Some(body)
            case _ ⇒ None
          }.toSet
          bodyTerms.headOption match { // Are all elements of the initial sequence the same and equal to Some(body) ?
            case Some(Some(body)) if bodyTerms.size === 1 ⇒ body
            case _ ⇒
              // We failed to detect the constant pattern.

              // This is the remaining catch-all case.

              // Detect the identity pattern in a clause that matches a named unit:
              // MatchE(_, { ... c : NCT("name", Nil, Nil, Nil) => DisjunctE(i, _, NamedConjunctE(Nil, NCT("name", Nil, Nil, Nil)), DisjunctT(...)) ... } )
              // and replace each such clause by c : NCT("name", Nil, Nil, Nil) => c : DisjunctT(...) where DisjunctT(...) is the same type as `term.t`.
              // This is safe to replace at this point in every case clause.
              // However, this breaks types of named units that are parts of different disjunctions, for example in Option[Option[Int]] ⇒ Option[Option[Int]].
              // So we will not do this now.

              /*              val casesReplaced = if (withEta) casesSimplified.zipWithIndex.map {
                              case (CurriedE(vs@(VarE(vName, nct@NamedConjunctT(_, Nil, Nil, Nil)) :: Nil), DisjunctE(j, total, NamedConjunctE(Nil, nct2), disjunctType)), i)
                                if i === j && total === cases.length && nct === nct2 && disjunctType === t ⇒
                                CurriedE(vs, VarE(vName, disjunctType))
                              case (c, _) ⇒ c
                            } else casesSimplified
              */
              MatchE(termSimplified, casesSimplified)
          }
        }
    }

  }

}

// Inject a value into the i-th part of the disjunction of type tExpr.
final case class DisjunctE(index: Int, total: Int, term: TermExpr, t: TypeExpr) extends TermExpr {
  private[ch] override def simplifyOnceInternal(withEta: Boolean): TermExpr = this.copy(term = term.simplifyOnce(withEta))
}

// Function classes carrying the symbolic term information.

final class Function0Lambda[+Res](f: Function0[Res], val lambdaTerm: TermExpr) extends Function0[Res] {
  override def apply(): Res = f()
}

final class Function1Lambda[-Ar, +Res](f: Ar ⇒ Res, val lambdaTerm: TermExpr) extends (Ar ⇒ Res) {
  override def apply(a: Ar): Res = f(a)
}

final class Function2Lambda[-Arg1, -Arg2, +Res](f: (Arg1, Arg2) ⇒ Res, val lambdaTerm: TermExpr) extends ((Arg1, Arg2) ⇒ Res) {
  override def apply(a1: Arg1, a2: Arg2): Res = f(a1, a2)
}

final class Function3Lambda[-Arg1, -Arg2, -Arg3, +Res](f: (Arg1, Arg2, Arg3) ⇒ Res, val lambdaTerm: TermExpr) extends ((Arg1, Arg2, Arg3) ⇒ Res) {
  override def apply(a1: Arg1, a2: Arg2, a3: Arg3): Res = f(a1, a2, a3)
}
