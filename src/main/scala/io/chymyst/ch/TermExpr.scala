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
    case p@PropE(_, _) ⇒ Seq(p.asInstanceOf[PropE[T]]) // Need to specify type parameter in match... `case p@PropE(_)` does not work.
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

  def findFirst[T, R](inExpr: TermExpr[T])(pred: PartialFunction[TermExpr[T], R]): Option[R] = {
    Some(inExpr).collect(pred).orElse {
      inExpr match {
        case PropE(_, _) ⇒ None
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

  private def sameConstructor[T](t1: TypeExpr[T], t2: TypeExpr[T]): Boolean = t1 match {
    case NamedConjunctT(c1, _, _, _) ⇒ t2 match {
      case NamedConjunctT(c2, _, _, _) ⇒ c1 == c2
      case _ ⇒ false
    }
    case DisjunctT(c1, _, _) ⇒ t2 match {
      case DisjunctT(c2, _, _) ⇒ c1 == c2
      case _ ⇒ false
    }
    case _ ⇒ false
  }

  private def atLeastOnce(x: Int): Int = math.max(x - 1, 0)

  // How many times each function uses its argument. Only counts when an argument is used more than once.
  def argsMultiUseCountDeep[T](inExpr: TermExpr[T]): Int = inExpr match {
    case PropE(_, _) ⇒ 0
    case AppE(head, arg) ⇒ argsMultiUseCountDeep(head) + argsMultiUseCountDeep(arg)
    case CurriedE(heads, body) ⇒ heads.map(head ⇒ atLeastOnce(body.varCount(head.name))).sum + argsMultiUseCountDeep(body)
    case UnitE(_) ⇒ 0
    case NamedConjunctE(terms, tExpr) ⇒ terms.map(argsMultiUseCountDeep).sum
    case ConjunctE(terms) ⇒ terms.map(argsMultiUseCountDeep).sum
    case ProjectE(index, term) ⇒ argsMultiUseCountDeep(term)
    case MatchE(term, cases) ⇒ argsMultiUseCountDeep(term) + cases.map(argsMultiUseCountDeep).sum
    case DisjunctE(index, total, term, tExpr) ⇒ argsMultiUseCountDeep(term)
  }

  def argsMultiUseCountShallow[T](inExpr: TermExpr[T]): Int = inExpr match {
    case CurriedE(heads, body) ⇒ heads.map(head ⇒ atLeastOnce(body.varCount(head.name))).sum
    case _ ⇒ 0
  }

  def conjunctionPermutationScore[T](inExpr: TermExpr[T]): Double = {
    inExpr match {
      case PropE(_, _) ⇒ 0
      case AppE(head, arg) ⇒ conjunctionPermutationScore(head) + conjunctionPermutationScore(arg)
      case CurriedE(_, body) ⇒ conjunctionPermutationScore(body)
      case UnitE(_) ⇒ 0
      case NamedConjunctE(terms, tExpr) ⇒
        terms.map(conjunctionPermutationScore).sum +
          terms.zipWithIndex.flatMap { case (t, i) ⇒
            // Only count projections from terms of exactly the same type as this `NamedConjunctE`.
            findFirst(t) { case ProjectE(index, term) if sameConstructor(term.tExpr, tExpr) ⇒
              conjunctionPermutationScore(term) / terms.length.toDouble +
                (if (index == i) 0 else 1)
            }
          }.sum
      case ConjunctE(terms) ⇒
        terms.zipWithIndex.flatMap { case (t, i) ⇒
          findFirst(t) { case ProjectE(index, term) if (term.tExpr match {
            case ConjunctT(_) ⇒ true
            case _ ⇒ false
          }) ⇒
            conjunctionPermutationScore(term) / terms.length.toDouble +
              (if (index == i) 0 else 1)
          }
        }.sum
      case ProjectE(_, term) ⇒ conjunctionPermutationScore(term)
      case MatchE(term, cases) ⇒ conjunctionPermutationScore(term) + cases.map(conjunctionPermutationScore).sum
      case DisjunctE(_, _, term, _) ⇒ conjunctionPermutationScore(term)
    }
  }

  def disjunctionPermutationScore[T](inExpr: TermExpr[T]): Double = {
    inExpr match {
      case PropE(name, tExpr) ⇒ 0
      case AppE(head, arg) ⇒ disjunctionPermutationScore(head) + disjunctionPermutationScore(arg)
      case CurriedE(heads, body) ⇒ disjunctionPermutationScore(body)
      case UnitE(tExpr) ⇒ 0
      case NamedConjunctE(terms, tExpr) ⇒ terms.map(disjunctionPermutationScore).sum
      case ConjunctE(terms) ⇒ terms.map(disjunctionPermutationScore).sum
      case ProjectE(index, term) ⇒ disjunctionPermutationScore(term)
      case MatchE(term, cases) ⇒
        disjunctionPermutationScore(term) +
          cases.zipWithIndex.flatMap { case (t, i) ⇒
            // Only count disjunction constructions into terms of exactly the same type as the `term` being matched.
            findFirst(t) { case DisjunctE(index, _, t2, tExpr) if sameConstructor(term.tExpr, tExpr) ⇒
              disjunctionPermutationScore(t2) / cases.length.toDouble +
                (if (index == i) 0 else 1)
            }
          }.sum
      case DisjunctE(index, total, term, tExpr) ⇒ disjunctionPermutationScore(term)
    }
  }

}

sealed trait TermExpr[+T] {
  def informationLossScore = (
    ()
    , unusedArgs.size
    , unusedTupleParts + unusedMatchClauseVars
    , TermExpr.conjunctionPermutationScore(this) + TermExpr.disjunctionPermutationScore(this)
    , TermExpr.argsMultiUseCountDeep(this)
  )

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
    case MatchE(term, cases) ⇒ "(" + term.toString + " match { " + cases.map(_.toString).mkString("; ") + "})"
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

  def simplify(withEta: Boolean = false): TermExpr[T] = this

  private[ch] def unusedArgs: Set[VarName] = this match {
    case PropE(_, _) ⇒ Set()
    case AppE(head, arg) ⇒ head.unusedArgs ++ arg.unusedArgs
    case CurriedE(heads, body) ⇒ (heads.map(_.name).toSet -- body.freeVars) ++ body.unusedArgs
    case UnitE(_) ⇒ Set()
    case NamedConjunctE(terms, _) ⇒ terms.flatMap(_.unusedArgs).toSet
    case ConjunctE(terms) ⇒ terms.flatMap(_.unusedArgs).toSet
    case ProjectE(_, term) ⇒ term.unusedArgs
    case MatchE(term, cases) ⇒ term.unusedArgs ++ cases.flatMap {
      // the unused heads in this CurriedE are counted separately by `unusedMatchClauseVars`
      case CurriedE(List(_), body) ⇒ body.unusedArgs
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
    case MatchE(_, cases) ⇒ cases.map {
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

final case class PropE[T](name: VarName, tExpr: TypeExpr[T]) extends TermExpr[T]

final case class AppE[T](head: TermExpr[T], arg: TermExpr[T]) extends TermExpr[T] {

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

}

final case class UnitE[T](tExpr: TypeExpr[T]) extends TermExpr[T]

final case class NamedConjunctE[T](terms: Seq[TermExpr[T]], tExpr: NamedConjunctT[T]) extends TermExpr[T] {
  override def simplify(withEta: Boolean): TermExpr[T] = this.copy(terms = terms.map(_.simplify(withEta)))
}

final case class ConjunctE[T](terms: Seq[TermExpr[T]]) extends TermExpr[T] {
  def tExpr: TypeExpr[T] = ConjunctT(terms.map(_.tExpr))

  override def simplify(withEta: Boolean): TermExpr[T] = this.copy(terms = terms.map(_.simplify(withEta)))
}

// The `term` should be a ConjunctT or a NamedConjunctT
final case class ProjectE[T](index: Int, term: TermExpr[T]) extends TermExpr[T] {
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
    case MatchE(t, cases) ⇒
      MatchE(t, cases.map {
        case CurriedE(List(propE), body) ⇒
          CurriedE(List(propE), ProjectE(index, body).simplify(withEta))
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
  * @tparam T Type of the internal representation of the term names.
  */
final case class MatchE[T](term: TermExpr[T], cases: List[TermExpr[T]]) extends TermExpr[T] {
  override def tExpr: TypeExpr[T] = cases match {
    case te :: tail ⇒
      te match {
        case CurriedE(List(_), body) ⇒
          val tpe = body.tExpr
          if (tail.exists(_.asInstanceOf[CurriedE[T]].body.tExpr != tpe))
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

  override def simplify(withEta: Boolean): TermExpr[T] = {
    lazy val casesSimplified = cases.map(_.simplify(withEta))
    term.simplify(withEta) match {
      case DisjunctE(index, total, t, _) ⇒
        if (total == cases.length) {
          AppE(cases(index).simplify(withEta), t).simplify(withEta)
        } else throw new Exception(s"Internal error: MatchE with ${cases.length} cases applied to DisjunctE with $total parts, but must be of equal size")

      // Detect the identity pattern:
      // MatchE(_, a: T1 ⇒ DisjunctE(i, total, NamedConjunctE(List(ProjectE(0, a), Project(1, a), ...), T1), ...), _)
      case t ⇒
        if (cases.nonEmpty && {
          casesSimplified.zipWithIndex.forall {
            case (CurriedE(List(head@PropE(_, headT)), DisjunctE(i, len, NamedConjunctE(projectionTerms, conjT), _)), ind) ⇒
              val result = len == cases.length && ind == i && headT == conjT &&
                projectionTerms.zipWithIndex.forall {
                  case (ProjectE(k, head1), j) if k == j && head1 == head ⇒ true
                  case _ ⇒ false
                }
              result
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
          val bodyTerms: Set[Option[TermExpr[T]]] = casesSimplified.map {
            case c@CurriedE(List(PropE(name, _)), body) if c.unusedArgs contains name ⇒ Some(body)
            case _ ⇒ None
          }.toSet
          bodyTerms.headOption match { // Are all elements of the initial sequence the same and equal to Some(body) ?
            case Some(Some(body)) if bodyTerms.size == 1 ⇒ body
            case _ ⇒
              // We failed to detect the constant pattern.
              // This is the default, catch-all case.
              MatchE(t, casesSimplified)
          }
        }
    }

  }

}

// Inject a value into the i-th part of the disjunction of type tExpr.
final case class DisjunctE[T](index: Int, total: Int, term: TermExpr[T], tExpr: TypeExpr[T]) extends TermExpr[T] {
  override def simplify(withEta: Boolean): TermExpr[T] = this.copy(term = term.simplify(withEta))
}
