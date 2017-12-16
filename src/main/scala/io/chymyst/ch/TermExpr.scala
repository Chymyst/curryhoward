package io.chymyst.ch

import io.chymyst.ch.TermExpr.VarName

object TermExpr {
  type VarName = String
  type ProofTerm[T] = TermExpr[T]

  def propositions[T](termExpr: TermExpr[T]): Set[PropE[T]] = termExpr match {
    case p: PropE[T] ⇒ Set(p) // Need to specify type parameter in match... `case p@PropE(_)` does not work.
    case AppE(head, arg) ⇒ propositions(head) ++ propositions(arg)
    case l: CurriedE[T] ⇒ // Can't pattern-match directly for some reason! Some trouble with the type parameter T.
      l.heads.toSet ++ propositions(l.body)
    case ConjunctE(terms) ⇒ terms.flatMap(propositions).toSet
    case _ ⇒ Set()
  }

  private val freshIdents = new FreshIdents("z")

  private val makeFreshNames: Iterator[VarName] = Iterator.iterate(freshIdents())(_ ⇒ freshIdents())

  def allFreshNames(names1: Seq[VarName], names2: Seq[VarName], namesToExclude: Set[VarName]): Seq[VarName] = {
    val requiredNumber = names1.length
    val allExcluded = (names1 ++ names2 ++ namesToExclude).toSet
    makeFreshNames.filterNot(allExcluded.contains).take(requiredNumber).toSeq
  }

  // Apply this term to a number of vars at once.
  def applyToVars[T](termExpr: TermExpr[T], args: List[TermExpr[T]]): TermExpr[T] = {
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
    case CurriedE(heads: List[PropE[T]], body) ⇒ CurriedE(heads, TermExpr.subst(replaceVar, expr, body))
    case ConjunctE(terms) ⇒ ConjunctE(terms.map(t ⇒ TermExpr.subst(replaceVar, expr, t)))
    case DisjunctE(index, total, term, tExpr) ⇒ DisjunctE(index, total, TermExpr.subst(replaceVar, expr, term), tExpr)
    case _ ⇒ inExpr
  }

}

sealed trait TermExpr[+T] {
  def tExpr: TypeExpr[T]

  override lazy val toString: String = this match {
    case PropE(name, tExpr) ⇒ s"($name:$tExpr)"
    case AppE(head, arg) ⇒ s"($head)($arg)"
    case CurriedE(heads, body) ⇒ s"\\(${heads.mkString(" -> ")} -> $body)"
    case UnitE(tExpr) ⇒ "()"
    case ConjunctE(terms) ⇒ "(" + terms.map(_.toString).mkString(", ") + ")"
    case DisjunctE(index, total, term, _) ⇒
      val leftZeros = Seq.fill(index)("0")
      val leftZerosString = if (leftZeros.isEmpty) "" else " + "
      val rightZeros = Seq.fill(total - index - 1)("0")
      val rightZerosString = if (rightZeros.isEmpty) "" else " + "
      "(" + leftZeros.mkString(" + ") + leftZerosString + term.toString + rightZerosString + rightZeros.mkString(" + ") + ")"
  }

  def map[U](f: T ⇒ U): TermExpr[U]

  def simplify: TermExpr[T] = this

  lazy val freeVars: Set[VarName] = this match {
    case PropE(name, tExpr) ⇒ Set(name)
    case AppE(head, arg) ⇒ head.freeVars ++ arg.freeVars
    case CurriedE(heads, body) ⇒ body.freeVars -- heads.map(_.name).toSet
    case UnitE(tExpr) ⇒ Set()
    case ConjunctE(terms) ⇒ terms.flatMap(_.freeVars).toSet
    case d: DisjunctE[T] ⇒ d.term.freeVars
  }

  // Rename a variable *everywhere* in the expression.
  def renameVar(oldName: VarName, newName: VarName): TermExpr[T] = {
    def rename(t: TermExpr[T]): TermExpr[T] = t.renameVar(oldName, newName)

    this match {
      case PropE(name, tExpr) ⇒
        val replacedName = if (name == oldName) newName else name
        PropE(replacedName, tExpr)
      case AppE(head, arg) ⇒ AppE(rename(head), rename(arg))
      case CurriedE(heads, body) ⇒ CurriedE(heads.map(h ⇒ rename(h).asInstanceOf[PropE[T]]), rename(body))
      case UnitE(tExpr) ⇒ this
      case ConjunctE(terms) ⇒ ConjunctE(terms.map(rename))
      case d: DisjunctE[T] ⇒ d.copy(term = rename(d.term))
    }
  }

  def renameAllVars(oldNames: Seq[VarName], newNames: Seq[VarName]): TermExpr[T] = {
    oldNames.zip(newNames)
      .foldLeft(this) { case (prev, (oldName, newName)) ⇒ prev.renameVar(oldName, newName) }
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
    case _ ⇒ throw new Exception(s"Internal error: Invalid head type in application $this: `${head.tExpr}` must be a function with argument type `${arg.tExpr}`")
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

  override def simplify: TermExpr[T] = this.copy(body = body.simplify)
}

final case class UnitE[T](tExpr: TypeExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = UnitE(tExpr map f)
}

final case class ConjunctE[+T](terms: Seq[TermExpr[T]]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = ConjunctE(terms.map(_.map(f)))

  def tExpr: TypeExpr[T] = ConjunctT(terms.map(_.tExpr))

  override def simplify: TermExpr[T] = this.copy(terms = terms.map(_.simplify))
}

final case class DisjunctE[T](index: Int, total: Int, term: TermExpr[T], tExpr: TypeExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = DisjunctE(index, total, term map f, tExpr map f)

  override def simplify: TermExpr[T] = this.copy(term = term.simplify)
}
