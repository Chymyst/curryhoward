package io.chymyst.ch

import io.chymyst.ch.TermExpr.VarName

object TermExpr {
  type VarName = String
  type ProofTerm[T] = TermExpr[T]

  def propositions[T](termExpr: TermExpr[T]): Set[PropE[T]] = termExpr match {
    case p: PropE[T] ⇒ Set(p) // Need to specify type parameter in match... `case p@PropE(_)` does not work.
    case AppE(head, arg) ⇒ propositions(head) ++ propositions(arg)
    case CurriedE(heads, body) ⇒ // Can't pattern-match directly for some reason! Some trouble with the type parameter T.
      heads.asInstanceOf[List[PropE[T]]].toSet ++ propositions(body)
    case ConjunctE(terms) ⇒ terms.flatMap(propositions).toSet
    case ProjectE(index, term) ⇒ propositions(term)
    case NamedConjunctE(terms, tExpr) ⇒ terms.flatMap(propositions).toSet
    case MatchE(term, cases) ⇒ propositions(term) ++ cases.flatMap(propositions).toSet
    case DisjunctE(_, _, term, _) ⇒ propositions(term)
    case UnitE(_) ⇒ Set()
  }

  private val freshIdents = new FreshIdents("z")

  private val makeFreshNames: Iterator[VarName] = Iterator.iterate(freshIdents())(_ ⇒ freshIdents())

  def allFreshNames(names1: Seq[VarName], names2: Seq[VarName], namesToExclude: Set[VarName]): Seq[VarName] = {
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
        val e1New = e1.renameAllVars(vars1, freshNames)
        val e2New = e2.renameAllVars(vars2, freshNames)
        heads1.map(_.renameAllVars(vars1, freshNames)) == heads2.map(_.renameAllVars(vars2, freshNames)) &&
          equiv(body1.renameAllVars(vars1, freshNames), body2.renameAllVars(vars2, freshNames))
      case _ ⇒ false
    }
    case _ ⇒ e1 == e2
  }

  def subst[T](replaceVar: PropE[T], expr: TermExpr[T], inExpr: TermExpr[T]): TermExpr[T] = inExpr match {
    case PropE(name, tExpr) if name == replaceVar.name ⇒
      if (tExpr == replaceVar.tExpr) expr else throw new Exception(s"Incorrect type ${replaceVar.tExpr} in subst($replaceVar, $expr, $inExpr), expected $tExpr")
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
  def tExpr: TypeExpr[T]

  def prettyPrint: String = prettyRename.prettyPrintWithParentheses(0)

  override lazy val toString: String = this match {
    case PropE(name, tExpr) ⇒ s"($name:${tExpr.prettyPrint})"
    case AppE(head, arg) ⇒ s"($head)($arg)"
    case CurriedE(heads, body) ⇒ s"\\(${heads.mkString(" ⇒ ")} ⇒ $body)"
    case UnitE(_) ⇒ "()"
    case ConjunctE(terms) ⇒ "(" + terms.map(_.toString).mkString(", ") + ")"
    case NamedConjunctE(terms, tExpr) ⇒ s"${tExpr.constructor.toString}(${terms.map(_.toString).mkString(", ")})"
    case ProjectE(index, term) ⇒ term.toString + "._" + (index + 1).toString
    case MatchE(term, cases) ⇒ "(" + term.toString + " match " + cases.map(_.toString).mkString(" + ") + ")"
    case DisjunctE(index, total, term, _) ⇒
      val leftZeros = Seq.fill(index)("0")
      val leftZerosString = if (leftZeros.isEmpty) "" else " + "
      val rightZeros = Seq.fill(total - index - 1)("0")
      val rightZerosString = if (rightZeros.isEmpty) "" else " + "
      "(" + leftZeros.mkString(" + ") + leftZerosString + term.toString + rightZerosString + rightZeros.mkString(" + ") + ")"
  }

  private[ch] def prettyPrintWithParentheses(level: Int): String = this match {
    case PropE(name, tExpr) ⇒ s"$name"
    case AppE(head, arg) ⇒
      val r = s"${head.prettyPrintWithParentheses(0)} ${arg.prettyPrintWithParentheses(1)}"
      if (level == 1) s"($r)" else r
    case CurriedE(heads, body) ⇒ s"(${heads.map(_.prettyPrintWithParentheses(0)).mkString(" ⇒ ")} ⇒ ${body.prettyPrintWithParentheses(0)})"
    case UnitE(tExpr) ⇒ "1"
    case ConjunctE(terms) ⇒ "(" + terms.map(_.prettyPrintWithParentheses(0)).mkString(", ") + ")"
    case NamedConjunctE(terms, tExpr) ⇒ s"${tExpr.constructor.toString}(${terms.map(_.prettyPrintWithParentheses(0)).mkString(", ")})"
    case ProjectE(index, term) ⇒ term.prettyPrintWithParentheses(1) + "." + term.accessor(index)
    case MatchE(term, cases) ⇒ "(" + term.prettyPrintWithParentheses(1) + " match " + cases.map(_.prettyPrintWithParentheses(0)).mkString(" + ") + ")"
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
    val oldVars = usedVars.toSeq.sorted.reverse // Let's see if reversing helps achieve a more natural style, a -> b -> c -> .... rather than c -> b -> a -> ...
    val newVars = prettyVars.take(oldVars.length).toSeq
    this.renameAllVars(oldVars, newVars)
  }

  def accessor(index: Int): String = tExpr match {
    case NamedConjunctT(_, _, accessors, _) ⇒ accessors(index).toString
    case ConjunctT(terms) ⇒ s"_${index + 1}"
    case _ ⇒ throw new Exception(s"Internal error: Cannot perform projection for term $toString : ${tExpr.prettyPrint} because its type is not a conjunction")
  }

  def map[U](f: T ⇒ U): TermExpr[U]

  def simplify: TermExpr[T] = this

  def unusedArgs: Set[VarName] = Set()

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
    case PropE(name, tExpr) ⇒ Seq()
    case AppE(head, arg) ⇒ head.usedTuplePartsSeq ++ arg.usedTuplePartsSeq
    case CurriedE(heads, body) ⇒ body.usedTuplePartsSeq
    case UnitE(tExpr) ⇒ Seq()
    case ConjunctE(terms) ⇒ terms.flatMap(_.usedTuplePartsSeq)
    case NamedConjunctE(terms, tExpr) ⇒ terms.flatMap(_.usedTuplePartsSeq)
    case ProjectE(index, term) ⇒ Seq((term, index + 1)) ++ term.usedTuplePartsSeq
    case MatchE(term, cases) ⇒ term.usedTuplePartsSeq ++ cases.flatMap(_.usedTuplePartsSeq)
    case DisjunctE(index, total, term, tExpr) ⇒ term.usedTuplePartsSeq
  }

  lazy val freeVars: Set[VarName] = this match {
    case PropE(name, tExpr) ⇒ Set(name)
    case AppE(head, arg) ⇒ head.freeVars ++ arg.freeVars
    case CurriedE(heads, body) ⇒ body.freeVars -- heads.map(_.name).toSet
    case UnitE(tExpr) ⇒ Set()
    case ConjunctE(terms) ⇒ terms.flatMap(_.freeVars).toSet
    case NamedConjunctE(terms, tExpr) ⇒ terms.flatMap(_.freeVars).toSet
    case p: ProjectE[T] ⇒ p.getProjection.map(_.freeVars).getOrElse(p.term.freeVars)
    case MatchE(term, cases) ⇒ term.freeVars ++ cases.flatMap(_.freeVars).toSet
    case d: DisjunctE[T] ⇒ d.term.freeVars
  }

  lazy val usedVars: Set[VarName] = this match {
    case PropE(name, tExpr) ⇒ Set(name)
    case AppE(head, arg) ⇒ head.usedVars ++ arg.usedVars
    case CurriedE(heads, body) ⇒ body.usedVars ++ heads.map(_.name).toSet
    case UnitE(tExpr) ⇒ Set()
    case ConjunctE(terms) ⇒ terms.flatMap(_.usedVars).toSet
    case NamedConjunctE(terms, tExpr) ⇒ terms.flatMap(_.usedVars).toSet
    case p: ProjectE[T] ⇒ p.getProjection.map(_.usedVars).getOrElse(p.term.usedVars)
    case MatchE(term, cases) ⇒ term.usedVars ++ cases.flatMap(_.usedVars).toSet
    case d: DisjunctE[T] ⇒ d.term.usedVars
  }

  def varCount(varName: VarName): Int = this match {
    case PropE(name, tExpr) ⇒ if (name == varName) 1 else 0
    case AppE(head, arg) ⇒ head.varCount(varName) + arg.varCount(varName)
    case CurriedE(heads, body) ⇒ body.varCount(varName)
    case UnitE(tExpr) ⇒ 0
    case NamedConjunctE(terms, tExpr) ⇒ terms.map(_.varCount(varName)).sum
    case ConjunctE(terms) ⇒ terms.map(_.varCount(varName)).sum
    case ProjectE(index, term) ⇒ term.varCount(varName)
    case MatchE(term, cases) ⇒ term.varCount(varName) + cases.map(_.varCount(varName)).sum
    case DisjunctE(index, total, term, tExpr) ⇒ term.varCount(varName)
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
      case UnitE(tExpr) ⇒ this
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
    case _ ⇒ throw new Exception(s"Internal error: Invalid head type in application $this: `${head.tExpr.prettyPrint}` must be a function with argument type `${arg.tExpr}`")
  }

  override def simplify: TermExpr[T] = {
    val headSimpl = head.simplify
    val argSimpl = arg.simplify

    headSimpl match {
      case CurriedE(heads, body) ⇒
        // substitute arg as first variable into body and return CurriedE unless we have no more arguments, else return new body
        val result: TermExpr[T] = TermExpr.subst(heads.head, argSimpl, body.simplify).simplify
        heads.tail match {
          case Nil ⇒ result
          case h ⇒ CurriedE(h, result)
        }
      case _ ⇒ this.copy(head = headSimpl, arg = argSimpl)
    }
  }
}

// The order of `heads` is straight, so `CurriedE(List(x1, x2, x3), body)` represents the term `x1 -> x2 -> x2 -> body`
final case class CurriedE[T](heads: List[PropE[T]], body: TermExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = CurriedE(heads map (_ map f), body map f)

  // The type is t1 -> t2 -> t3 -> b; here `heads` = List(t1, t2, t3).
  def tExpr: TypeExpr[T] = heads.reverse.foldLeft(body.tExpr) { case (prev, head) ⇒ head.tExpr ->: prev }

  override def simplify: TermExpr[T] = (heads, body.simplify) match {
    // Check for inverse eta-conversion: simplify x ⇒ y ⇒ ... ⇒ z ⇒ a ⇒ f a into x ⇒ y ⇒ ... ⇒ z ⇒ f.
    // TODO figure out why this does not work
    //    case (_ :: _, AppE(fHead, fBody)) if heads.last == fBody ⇒ CurriedE(heads.slice(0, heads.length - 1), fHead)
    case (_, simplifiedBody) ⇒ this.copy(body = simplifiedBody)
  }

  override def unusedArgs: Set[VarName] = heads.map(_.name).toSet -- body.freeVars

  override lazy val argsMultiUseCount: Int = heads.map(head ⇒ body.varCount(head.name)).sum
}

final case class UnitE[T](tExpr: TypeExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = UnitE(tExpr map f)
}

final case class NamedConjunctE[T](terms: Seq[TermExpr[T]], tExpr: NamedConjunctT[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = NamedConjunctE(terms map (_ map f), tExpr map f)

  override def simplify: TermExpr[T] = this.copy(terms = terms.map(_.simplify))
}

final case class ConjunctE[T](terms: Seq[TermExpr[T]]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = ConjunctE(terms.map(_.map(f)))

  def tExpr: TypeExpr[T] = ConjunctT(terms.map(_.tExpr))

  override def simplify: TermExpr[T] = this.copy(terms = terms.map(_.simplify))
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
    case NamedConjunctT(_, _, _, wrapped) ⇒ wrapped match {
      case ConjunctT(terms) ⇒ terms(index)
      case _ if index == 0 ⇒ wrapped
      // Otherwise it is an error!
      case _ ⇒ throw new Exception(s"Internal error: Invalid projection to index $index for a named conjunct $term : ${term.tExpr.prettyPrint} with multiplicity 1")
    }
    case _ ⇒ throw new Exception(s"Internal error: Invalid projection term $term whose type ${term.tExpr.prettyPrint} is not a conjunction")
  }

  override def simplify: TermExpr[T] = term.simplify match {
    case ConjunctE(terms) ⇒ terms(index).simplify
    case t ⇒ this.copy(term = t)
  }
}

// Match a disjunct term with n functions.
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

  // TODO: simplify when term is a DisjunctE
  override def simplify: TermExpr[T] = MatchE(term.simplify, cases.map(_.simplify))
}

// Inject a value into the i-th part of the disjunction of type tExpr.
final case class DisjunctE[T](index: Int, total: Int, term: TermExpr[T], tExpr: TypeExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = DisjunctE(index, total, term map f, tExpr map f)

  override def simplify: TermExpr[T] = this.copy(term = term.simplify)
}
