package io.chymyst.ch

import io.chymyst.ch.TermExpr.VarName

import scala.annotation.tailrec

object TermExpr {
  type VarName = String
  type ProofTerm[T] = TermExpr[T]

  @tailrec
  def simplifyWithEtaUntilStable[T](t: TermExpr[T]): TermExpr[T] = {
    val simplified = t.simplify(withEta = true)
    if (t == simplified) t else simplifyWithEtaUntilStable(simplified)
  }

  def propositions[T](termExpr: TermExpr[T]): Seq[PropE[T]] = (termExpr match {
    case p: PropE[T] ⇒ Seq(p) // Need to specify type parameter in match... `case p@PropE(_)` does not work.
    case AppE(head, arg) ⇒ propositions(head) ++ propositions(arg)
    case CurriedE(heads, body) ⇒ // Can't pattern-match directly for some reason! Some trouble with the type parameter T.
      heads.asInstanceOf[List[PropE[T]]] ++ propositions(body)
    case ConjunctE(terms) ⇒ terms.flatMap(propositions)
    case ProjectE(_, term) ⇒ propositions(term)
    case NamedConjunctE(terms, _) ⇒ terms.flatMap(propositions)
    case MatchE(term, cases) ⇒ propositions(term) ++ cases.flatMap(propositions)
    case DisjunctE(_, _, term, _) ⇒ propositions(term)
    case UnitE(_) ⇒ Seq()
  }).distinct

  private val freshIdents = new FreshIdents("z")

  private val makeFreshNames: Iterator[VarName] = Iterator.iterate(freshIdents())(_ ⇒ freshIdents())

  def allFreshNames(names1: Seq[VarName], names2: Seq[VarName], namesToExclude: Seq[VarName]): Seq[VarName] = {
    val requiredNumber = names1.length
    val allExcluded = (names1 ++ names2 ++ namesToExclude).toSet
    makeFreshNames.filterNot(allExcluded.contains).take(requiredNumber).toSeq
  }

  // Apply this term to a number of vars at once.
  def applyToVars[T](termExpr: TermExpr[T], args: Seq[TermExpr[T]]): TermExpr[T] = {
    args.foldLeft[TermExpr[T]](termExpr) { case (prev, arg) ⇒ AppE(prev, arg) }
  }

  // Compare terms up to renaming. Note: this is not alpha-conversion yet.
  def equiv[T](e1: TermExpr[T], e2: TermExpr[T]): Boolean = e1 match {
    case CurriedE(heads1, body1) ⇒ e2 match {
      case CurriedE(heads2, body2) if heads1.lengthCompare(heads2.length) == 0 ⇒
        // rename all vars to new names
        val vars1 = heads1.map(_.name)
        val vars2 = heads2.map(_.name)
        val freshNames = allFreshNames(vars1, vars2, body1.freeVars ++ body2.freeVars)
        heads1.map(_.renameAllVars(vars1, freshNames)) == heads2.map(_.renameAllVars(vars2, freshNames)) &&
          equiv(body1.renameAllVars(vars1, freshNames), body2.renameAllVars(vars2, freshNames))
      case _ ⇒ false
    }
    case _ ⇒ e1 == e2
  }

  def subst[T](replaceVar: PropE[T], expr: TermExpr[T], inExpr: TermExpr[T]): TermExpr[T] = inExpr match {
    case PropE(name, tExpr) if name == replaceVar.name ⇒
      if (tExpr == replaceVar.tExpr) expr else throw new Exception(s"Incorrect type ${replaceVar.tExpr.prettyPrint} in subst($replaceVar, $expr, $inExpr), expected ${tExpr.prettyPrint}")
    case AppE(head, arg) ⇒ AppE(subst(replaceVar, expr, head), subst(replaceVar, expr, arg))
    case CurriedE(heads, body) ⇒ CurriedE(heads.asInstanceOf[List[PropE[T]]], TermExpr.subst(replaceVar, expr, body))
    case ConjunctE(terms) ⇒ ConjunctE(terms.map(t ⇒ TermExpr.subst(replaceVar, expr, t)))
    case NamedConjunctE(terms, tExpr: NamedConjunctT[_]) ⇒ NamedConjunctE(terms.map(t ⇒ TermExpr.subst(replaceVar, expr, t)), tExpr)
    case ProjectE(index, term) ⇒ ProjectE(index, TermExpr.subst(replaceVar, expr, term))
    case MatchE(term, cases) ⇒ MatchE(TermExpr.subst(replaceVar, expr, term), cases.map(t ⇒ TermExpr.subst(replaceVar, expr, t)))
    case DisjunctE(index, total, term, tExpr) ⇒ DisjunctE(index, total, TermExpr.subst(replaceVar, expr, term), tExpr)
    case _ ⇒ inExpr
  }

}

sealed trait TermExpr[+T] {
  def informationLossScore = (unusedArgs.size + unusedTupleParts + unusedMatchClauseVars, argsMultiUseCount)

  def tExpr: TypeExpr[T]

  def prettyPrint: String = prettyRename.prettyPrintWithParentheses(0)

  override lazy val toString: String = this match {
    case PropE(name, _) ⇒ s"$name"
    case AppE(head, arg) ⇒ s"($head $arg)"
    case CurriedE(heads, body) ⇒ s"\\(${heads.map(h ⇒ "(" + h.name + ":" + h.tExpr.prettyPrint + ")").mkString(" ⇒ ")} ⇒ $body)"
    case UnitE(_) ⇒ "()"
    case ConjunctE(terms) ⇒ "(" + terms.map(_.toString).mkString(", ") + ")"
    case NamedConjunctE(terms, tExpr) ⇒
      val prefix = if (tExpr.isAtomic) "<co>" else ""
      s"$prefix${tExpr.constructor.toString}(${terms.map(_.toString).mkString(", ")})"
    case ProjectE(index, term) ⇒ term.toString + "." + term.accessor(index)
    case MatchE(term, cases) ⇒ "(" + term.toString + " match " + cases.map(_.toString).mkString("; ") + ")"
    case DisjunctE(index, total, term, _) ⇒
      val leftZeros = Seq.fill(index)("0")
      val leftZerosString = if (leftZeros.isEmpty) "" else " + "
      val rightZeros = Seq.fill(total - index - 1)("0")
      val rightZerosString = if (rightZeros.isEmpty) "" else " + "
      "(" + leftZeros.mkString(" + ") + leftZerosString + term.toString + rightZerosString + rightZeros.mkString(" + ") + ")"
  }

  private[ch] def prettyPrintWithParentheses(level: Int): String = this match {
    case PropE(name, _) ⇒ s"$name"
    case AppE(head, arg) ⇒
      val r = s"${head.prettyPrintWithParentheses(0)} ${arg.prettyPrintWithParentheses(1)}"
      if (level == 1) s"($r)" else r
    case CurriedE(heads, body) ⇒ s"(${heads.map(_.prettyPrintWithParentheses(0)).mkString(" ⇒ ")} ⇒ ${body.prettyPrintWithParentheses(0)})"
    case UnitE(_) ⇒ "1"
    case ConjunctE(terms) ⇒ "(" + terms.map(_.prettyPrintWithParentheses(0)).mkString(", ") + ")"
    case NamedConjunctE(terms, tExpr) ⇒ s"${tExpr.constructor.toString}(${terms.map(_.prettyPrintWithParentheses(0)).mkString(", ")})"
    case ProjectE(index, term) ⇒ term.prettyPrintWithParentheses(1) + "." + term.accessor(index)
    case MatchE(term, cases) ⇒ "(" + term.prettyPrintWithParentheses(1) + " match " + cases.map(_.prettyPrintWithParentheses(0)).mkString("; ") + ")"
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

  def prettyRename: TermExpr[T] = {
    val oldVars = usedVars // Use a `Seq` here rather than a `Set` for the list of variable names.
    // This achieves deterministic renaming, which is important for checking that different terms are equivalent up to renaming.
    val newVars = prettyVars.take(oldVars.length).toSeq
    this.renameAllVars(oldVars, newVars)
  }

  def accessor(index: Int): String = tExpr match {
    case NamedConjunctT(_, _, accessors, _) ⇒ accessors(index).toString
    case ConjunctT(_) ⇒ s"_${index + 1}"
    case _ ⇒ throw new Exception(s"Internal error: Cannot perform projection for term $toString : ${tExpr.prettyPrint} because its type is not a conjunction")
  }

  def map[U](f: T ⇒ U): TermExpr[U]

  def simplify(withEta: Boolean = false): TermExpr[T] = this

  private[ch] def unusedArgs: Set[VarName] = this match {
    case PropE(_, _) ⇒ Set()
    case AppE(head, arg) ⇒ head.unusedArgs ++ arg.unusedArgs
    case CurriedE(heads, body) ⇒ (heads.map(_.name).toSet -- body.freeVars) ++ body.unusedArgs
    case UnitE(_) ⇒ Set()
    case NamedConjunctE(terms, tExpr) ⇒ terms.flatMap(_.unusedArgs).toSet
    case ConjunctE(terms) ⇒ terms.flatMap(_.unusedArgs).toSet
    case ProjectE(_, term) ⇒ term.unusedArgs
    case MatchE(term, cases) ⇒ term.unusedArgs ++ cases.flatMap {
      case CurriedE(List(prop), body) ⇒ body.unusedArgs // the case arg is counted in unusedMatchClauseVars
      case c ⇒ c.unusedArgs
    }.toSet
    case DisjunctE(_, _, term, _) ⇒ term.unusedArgs
  }

  private[ch] def unusedMatchClauseVars: Double = this match {
    case PropE(_, _) ⇒ 0
    case AppE(head, arg) ⇒ head.unusedMatchClauseVars + arg.unusedMatchClauseVars
    case CurriedE(_, body) ⇒ body.unusedMatchClauseVars
    case UnitE(_) ⇒ 0
    case NamedConjunctE(terms, _) ⇒ terms.map(_.unusedMatchClauseVars).sum
    case ConjunctE(terms) ⇒ terms.map(_.unusedMatchClauseVars).sum
    case ProjectE(_, term) ⇒ term.unusedMatchClauseVars
    case MatchE(_, cases) ⇒ cases.map{
      case CurriedE(List(prop), body) ⇒ if (body.freeVars contains prop.name) 0.0 else 1.0
      case c ⇒ c.unusedMatchClauseVars
    }.sum / cases.length.toDouble
    case DisjunctE(_, _, term, _) ⇒ term.unusedMatchClauseVars
  }

  private[ch] lazy val unusedTupleParts: Int =
    usedTuplePartsSeq
      .groupBy(_._1) // Map[TermExpr[T], Seq[(TermExpr[Int], Int)]]
      .mapValues(_.map(_._2).distinct) // Map[TermExpr[T], Seq[Int]]
      .map { case (term, parts) ⇒
      val totalParts = term.tExpr.conjunctSize
      totalParts - parts.length
    }.count(_ > 0)

  // Can't use Set[TermExpr[T]] because of lack of covariance in `Set[A]`.
  private[ch] lazy val usedTuplePartsSeq: Seq[(TermExpr[T], Int)] = this match {
    case PropE(_, _) ⇒ Seq()
    case AppE(head, arg) ⇒ head.usedTuplePartsSeq ++ arg.usedTuplePartsSeq
    case CurriedE(_, body) ⇒ body.usedTuplePartsSeq
    case UnitE(_) ⇒ Seq()
    case ConjunctE(terms) ⇒ terms.flatMap(_.usedTuplePartsSeq)
    case NamedConjunctE(terms, _) ⇒ terms.flatMap(_.usedTuplePartsSeq)
    case ProjectE(index, term) ⇒ Seq((term, index + 1)) ++ term.usedTuplePartsSeq
    case MatchE(term, cases) ⇒ term.usedTuplePartsSeq ++ cases.flatMap(_.usedTuplePartsSeq)
    case DisjunctE(_, _, term, _) ⇒ term.usedTuplePartsSeq
  }

  lazy val freeVars: Seq[VarName] = (this match {
    case PropE(name, _) ⇒ Seq(name)
    case AppE(head, arg) ⇒ head.freeVars ++ arg.freeVars
    case CurriedE(heads, body) ⇒ body.freeVars.filterNot(heads.map(_.name).toSet.contains)
    case UnitE(_) ⇒ Seq()
    case ConjunctE(terms) ⇒ terms.flatMap(_.freeVars)
    case NamedConjunctE(terms, _) ⇒ terms.flatMap(_.freeVars)
    case p: ProjectE[T] ⇒ p.getProjection.map(_.freeVars).getOrElse(p.term.freeVars)
    case MatchE(term, cases) ⇒ term.freeVars ++ cases.flatMap(_.freeVars)
    case d: DisjunctE[T] ⇒ d.term.freeVars
  }).distinct

  lazy val usedVars: Seq[VarName] = (this match {
    case PropE(name, _) ⇒ Seq(name)
    case AppE(head, arg) ⇒ head.usedVars ++ arg.usedVars
    case CurriedE(heads, body) ⇒ body.usedVars ++ heads.map(_.name)
    case UnitE(_) ⇒ Seq()
    case ConjunctE(terms) ⇒ terms.flatMap(_.usedVars)
    case NamedConjunctE(terms, _) ⇒ terms.flatMap(_.usedVars)
    case p: ProjectE[T] ⇒ p.getProjection.map(_.usedVars).getOrElse(p.term.usedVars)
    case MatchE(term, cases) ⇒ term.usedVars ++ cases.flatMap(_.usedVars)
    case d: DisjunctE[T] ⇒ d.term.usedVars
  }).distinct

  def varCount(varName: VarName): Int = this match {
    case PropE(name, _) ⇒ if (name == varName) 1 else 0
    case AppE(head, arg) ⇒ head.varCount(varName) + arg.varCount(varName)
    case CurriedE(_, body) ⇒ body.varCount(varName)
    case UnitE(_) ⇒ 0
    case NamedConjunctE(terms, _) ⇒ terms.map(_.varCount(varName)).sum
    case ConjunctE(terms) ⇒ terms.map(_.varCount(varName)).sum
    case ProjectE(_, term) ⇒ term.varCount(varName)
    case MatchE(term, cases) ⇒ term.varCount(varName) + cases.map(_.varCount(varName)).sum
    case DisjunctE(_, _, term, _) ⇒ term.varCount(varName)
  }

  lazy val argsMultiUseCount: Int = 0

  // Rename all variable at once *everywhere* in the expression. (This is not the alpha-conversion!)
  def renameAllVars(oldNames: Seq[VarName], newNames: Seq[VarName]): TermExpr[T] = {
    renameAllVars(oldNames.zip(newNames).toMap)
  }

  def renameAllVars(oldAndNewNames: Map[VarName, VarName]): TermExpr[T] = {
    def rename(t: TermExpr[T]): TermExpr[T] = t.renameAllVars(oldAndNewNames)

    this match {
      case PropE(oldName, tExpr) ⇒
        val replacedName = oldAndNewNames.getOrElse(oldName, oldName)
        PropE(replacedName, tExpr)
      case AppE(head, arg) ⇒ AppE(rename(head), rename(arg))
      case CurriedE(heads, body) ⇒ CurriedE(heads.map(h ⇒ rename(h).asInstanceOf[PropE[T]]), rename(body))
      case UnitE(_) ⇒ this
      case ConjunctE(terms) ⇒ ConjunctE(terms.map(rename))
      case NamedConjunctE(terms, tExpr) ⇒ NamedConjunctE(terms map rename, tExpr)
      case ProjectE(index, term) ⇒ ProjectE(index, rename(term))
      case MatchE(term, cases) ⇒ MatchE(rename(term), cases.map(rename))
      case d: DisjunctE[T] ⇒ d.copy(term = rename(d.term))
    }
  }
}

final case class PropE[T](name: VarName, tExpr: TypeExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): PropE[U] = PropE(name, tExpr map f)
}

final case class AppE[T](head: TermExpr[T], arg: TermExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = AppE(head map f, arg map f)

  // The type of AppE is computed from the types of its arguments.
  // Make this a `val` to catch bugs early.
  val tExpr: TypeExpr[T] = head.tExpr match {
    case hd #-> body if hd == arg.tExpr ⇒ body
    case _ ⇒ throw new Exception(s"Internal error: Invalid head type in application $this: `${head.tExpr.prettyPrint}` must be a function with argument type `${arg.tExpr.prettyPrint}`")
  }

  override def simplify(withEta: Boolean): TermExpr[T] = {
    val headSimpl = head.simplify(withEta)
    val argSimpl = arg.simplify(withEta)

    headSimpl match {
      case CurriedE(heads, body) ⇒
        // substitute arg as first variable into body and return CurriedE unless we have no more arguments, else return new body
        val result: TermExpr[T] = TermExpr.subst(heads.head, argSimpl, body.simplify(withEta)).simplify(withEta)
        heads.drop(1) match {
          case Nil ⇒ result
          case h ⇒ CurriedE(h, result)
        }
      case _ ⇒ this.copy(head = headSimpl, arg = argSimpl)
    }
  }
}

// The order of `heads` is straight, so `CurriedE(List(x1, x2, x3), body)` represents the term `x1 -> x2 -> x2 -> body`
final case class CurriedE[T](heads: List[PropE[T]], body: TermExpr[T]) extends TermExpr[T] {
  private val headsLength = heads.length

  override def map[U](f: T ⇒ U): TermExpr[U] = CurriedE(heads map (_ map f), body map f)

  // The type is t1 -> t2 -> t3 -> b; here `heads` = List(t1, t2, t3).
  def tExpr: TypeExpr[T] = heads.reverse.foldLeft(body.tExpr) { case (prev, head) ⇒ head.tExpr ->: prev }

  override def simplify(withEta: Boolean): TermExpr[T] = {
    body.simplify(withEta) match {
      // Check for eta-contraction: simplify x ⇒ y ⇒ ... ⇒ z ⇒ a ⇒ f a into x ⇒ y ⇒ ... ⇒ z ⇒ f.
      // Here fHead = a and fBody = f; the last element of `heads` must be equal to `a`
      case AppE(fHead, fBody) if withEta &&
        headsLength > 0 && {
        val lastHead = heads(headsLength - 1)
        lastHead == fBody &&
          !fHead.freeVars.contains(lastHead.name) // Cannot replace a ⇒ f(... a ... ) a by f (... a ...)!
      } ⇒
        if (headsLength > 1)
          CurriedE(heads.slice(0, headsLength - 1), fHead)
        else
          fHead // no more function arguments left

      // Simplify nested CurriedE(CurriedE()) into a flat CurriedE().
      case CurriedE(heads1, body1) ⇒ CurriedE(heads ++ heads1, body1)
      case simplifiedBody ⇒ this.copy(body = simplifiedBody)
    }
  }

  override lazy val argsMultiUseCount: Int = heads.map(head ⇒ body.varCount(head.name)).sum
}

final case class UnitE[T](tExpr: TypeExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = UnitE(tExpr map f)
}

final case class NamedConjunctE[T](terms: Seq[TermExpr[T]], tExpr: NamedConjunctT[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = NamedConjunctE(terms map (_ map f), tExpr map f)

  override def simplify(withEta: Boolean): TermExpr[T] = this.copy(terms = terms.map(_.simplify(withEta)))
}

final case class ConjunctE[T](terms: Seq[TermExpr[T]]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = ConjunctE(terms.map(_.map(f)))

  def tExpr: TypeExpr[T] = ConjunctT(terms.map(_.tExpr))

  override def simplify(withEta: Boolean): TermExpr[T] = this.copy(terms = terms.map(_.simplify(withEta)))
}

// The `term` should be a ConjunctT or a NamedConjunctT
final case class ProjectE[T](index: Int, term: TermExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = ProjectE(index, term map f)

  def getProjection: Option[TermExpr[T]] = term match {
    case c: ConjunctE[T] ⇒ Some(c.terms(index))
    case _ ⇒ None
  }

  override def tExpr: TypeExpr[T] = term.tExpr match {
    case ConjunctT(terms) ⇒ terms(index)
    case NamedConjunctT(constructor, _, _, wrapped) ⇒ wrapped match {
      case Nil if index == 0 ⇒ UnitT(constructor)
      case _ :: _ ⇒ wrapped(index)
      // Otherwise it is an error!
      case _ ⇒ throw new Exception(s"Internal error: Invalid projection to index $index for a named conjunct $term : ${term.tExpr.prettyPrint} with multiplicity 1")
    }
    case _ ⇒ throw new Exception(s"Internal error: Invalid projection term $term whose type ${term.tExpr.prettyPrint} is not a conjunction")
  }

  override def simplify(withEta: Boolean): TermExpr[T] = term.simplify(withEta) match {
    case ConjunctE(terms) ⇒ terms(index).simplify(withEta)
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
  *              Each "case function" must be a function of the form CurriedE(List(PropE(_)), _),
  *              that is, with exactly one argument of the correct type.
  * @tparam T Type of the internal representation of the term names.
  */
final case class MatchE[T](term: TermExpr[T], cases: List[TermExpr[T]]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = MatchE(term map f, cases map (_.map(f)))

  override def tExpr: TypeExpr[T] = cases match {
    case te :: tail ⇒
      te match {
        case CurriedE(List(_), body) ⇒
          val tpe = body.tExpr
          if (tail.exists(_.asInstanceOf[CurriedE[T]].body.tExpr != tpe))
            throw new Exception(s"Internal error: unequal expression types in cases for $this")
          else
            tpe

        case _ ⇒ throw new Exception(s"Internal error: `case` expression for $this must contain functions of one argument")
      }
    case Nil ⇒ throw new Exception(s"Internal error: empty list of cases for $this")
  }

  override def simplify(withEta: Boolean): TermExpr[T] = {
    lazy val casesSimplified = cases.map(_.simplify(withEta))
    term.simplify(withEta) match {
      case DisjunctE(index, total, t, _) ⇒
        if (total == cases.length) {
          AppE(cases(index).simplify(withEta), t).simplify(withEta)
        } else throw new Exception(s"Internal error: MatchE with ${cases.length} cases applied to DisjunctE with $total parts, but must be of equal size")

      // Detect the identity pattern:
      // MatchE(_, a: T1 ⇒ DisjunctE(i, total, NamedConjunctE(List(ProjectE(0, a), Project(1, a), ...), T1), ...), _)
      case _ if cases.nonEmpty && casesSimplified.zipWithIndex.forall {
        case (CurriedE(List(head@PropE(_, headT)), DisjunctE(i, len, NamedConjunctE(projectionTerms, conjT), _)), ind) ⇒
          len == cases.length && ind == i && headT == conjT &&
            projectionTerms.zipWithIndex.forall {
              case (ProjectE(k, head1), j) if k == j && head1 == head ⇒ true
              case _ ⇒ false
            }
        case _ ⇒ false
      } ⇒
        // We detected an identity function of the form term match { case a => a; case b => b; etc.}
        // so we can just replace that by `term`.
        term

      // Detect the constant pattern:
      // MatchE(_, {f, f, ..., f}) where each clause is the same expression `f`, up to equivalence, and ignores its argument.
      // Then the entire term can be replaced by f.
      case t ⇒
        val bodyTerms: Set[Option[TermExpr[T]]] = casesSimplified.map {
          case c@CurriedE(List(head@PropE(name, headT)), body) if c.unusedArgs contains name ⇒ Some(body)
          case _ ⇒ None
        }.toSet
        bodyTerms.headOption match {
          case Some(Some(body)) if bodyTerms.size == 1 ⇒ body
          case _ ⇒
            // We failed to detect the constant pattern.
            // This is the default, catch-all case.
            MatchE(t, casesSimplified)
        }

    }

  }

}

// Inject a value into the i-th part of the disjunction of type tExpr.
final case class DisjunctE[T](index: Int, total: Int, term: TermExpr[T], tExpr: TypeExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = DisjunctE(index, total, term map f, tExpr map f)

  override def simplify(withEta: Boolean): TermExpr[T] = this.copy(term = term.simplify(withEta))
}
