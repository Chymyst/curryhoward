package io.chymyst.ch

object TermExpr {
  def propositions[T](termExpr: TermExpr[T]): Set[PropE[T]] = termExpr match {
    case p: PropE[T] ⇒ Set(p) // Need to specify type parameter in match... `case p@PropE(_)` does not work.
    case AppE(head, arg) ⇒ propositions(head) ++ propositions(arg)
    case l: CurriedE[T] ⇒ // Can't pattern-match directly for some reason! Some trouble with the type parameter T.
      l.heads.toSet ++ propositions(l.body)
    case ConjunctE(terms) ⇒ terms.flatMap(propositions).toSet
    case _ ⇒ Set()
  }

  type ProofTerm[T] = TermExpr[T]

  // Apply this term to a number of vars at once.
  def applyToVars[T](termExpr: TermExpr[T], args: List[TermExpr[T]]): TermExpr[T] = {
    args.foldLeft[TermExpr[T]](termExpr) { case (prev, arg) ⇒ AppE(prev, arg) }
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

  lazy val freeVars: Set[String] = this match {
    case PropE(name, tExpr) ⇒ Set(name)
    case AppE(head, arg) ⇒ head.freeVars ++ arg.freeVars
    case CurriedE(heads, body) ⇒ body.freeVars -- heads.map(_.name).toSet
    case UnitE(tExpr) ⇒ Set()
    case ConjunctE(terms) ⇒ terms.flatMap(_.freeVars).toSet
    case DisjunctE(index, total, term, tExpr) ⇒ term.freeVars
  }
}

final case class PropE[T](name: String, tExpr: TypeExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): PropE[U] = PropE(name, tExpr map f)
}

final case class AppE[T](head: TermExpr[T], arg: TermExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = AppE(head map f, arg map f)

  // The type of AppE is computed from the types of its arguments.
  def tExpr: TypeExpr[T] = head.tExpr match {
    case hd :-> body if hd == arg.tExpr ⇒ body
    case _ ⇒ throw new Exception(s"Internal error: Invalid head type in application, ${head.tExpr}: must be a function with argument type ${arg.tExpr}")
  }
}

// The order of `heads` is straight, so `CurriedE(List(x1, x2, x3), body)` represents the term `x1 -> x2 -> x2 -> body`
final case class CurriedE[T](heads: List[PropE[T]], body: TermExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = CurriedE(heads map (_ map f), body map f)

  // The type is t1 -> t2 -> t3 -> b
  def tExpr: TypeExpr[T] = heads.reverse.foldLeft(body.tExpr) { case (prev, head) ⇒ head.tExpr :-> prev }
}

final case class UnitE[T](tExpr: TypeExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = UnitE(tExpr map f)
}

final case class ConjunctE[+T](terms: Seq[TermExpr[T]]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = ConjunctE(terms.map(_.map(f)))

  def tExpr: TypeExpr[T] = ConjunctT(terms.map(_.tExpr))
}

final case class DisjunctE[T](index: Int, total: Int, term: TermExpr[T], tExpr: TypeExpr[T]) extends TermExpr[T] {
  override def map[U](f: T ⇒ U): TermExpr[U] = DisjunctE(index, total, term map f, tExpr map f)
}

