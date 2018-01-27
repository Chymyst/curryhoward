package io.chymyst.ch

import scala.annotation.tailrec

object TermExpr {
  @tailrec
  private[ch] def simplifyWithEtaUntilStable(t: TermExpr): TermExpr = {
    val simplified = t.simplifyOnce(withEta = true)
    if (t == simplified) t else simplifyWithEtaUntilStable(simplified)
  }

  def foldMap[R: Monoid](termExpr: TermExpr)(p: PartialFunction[TermExpr, R]): R = {
    if (p isDefinedAt termExpr)
      p(termExpr)
    else {
      import io.chymyst.ch.Monoid.MonoidSyntax
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
  def lambdaTerm(f: Any): Option[TermExpr] = //Try(WithLambdaTerm(f).lambdaTerm).toOption fails to compile
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

  def substMap(termExpr: TermExpr)(p: PartialFunction[TermExpr, TermExpr]): TermExpr =
    if (p isDefinedAt termExpr)
      p(termExpr)
    else {
      def subst(termExpr: TermExpr): TermExpr = substMap(termExpr)(p)

      termExpr match {
        case AppE(head, arg) ⇒ AppE(subst(head), subst(arg))
        case CurriedE(heads, body) ⇒ CurriedE(heads.map(subst).asInstanceOf[List[VarE]], subst(body))
        case ConjunctE(terms) ⇒ ConjunctE(terms.map(subst))
        case NamedConjunctE(terms, tExpr) ⇒ NamedConjunctE(terms.map(subst), tExpr)
        case ProjectE(index, term) ⇒ ProjectE(index, subst(term))
        case MatchE(term, cases) ⇒ MatchE(subst(term), cases.map(subst))
        case DisjunctE(index, total, term, tExpr) ⇒ DisjunctE(index, total, subst(term), tExpr)
        case _ ⇒ termExpr
      }
    }

  def subst(replaceVar: VarE, expr: TermExpr, inExpr: TermExpr): TermExpr = substMap(inExpr) {
    case VarE(name, tExpr) if name == replaceVar.name ⇒
      if (tExpr == replaceVar.tExpr) expr else throw new Exception(s"Incorrect type ${replaceVar.tExpr.prettyPrint} in subst($replaceVar, $expr, $inExpr), expected ${tExpr.prettyPrint}")
  }

  def substTypeVar(replaceTypeVar: TP, newTypeExpr: TypeExpr, inExpr: TermExpr): TermExpr = substMap(inExpr) {
    case VarE(name, tExpr) ⇒ VarE(name, tExpr.substTypeVar(replaceTypeVar, newTypeExpr))
    case NamedConjunctE(terms, tExpr) ⇒ NamedConjunctE(terms.map(substTypeVar(replaceTypeVar, newTypeExpr, _)), tExpr.substTypeVar(replaceTypeVar, newTypeExpr).asInstanceOf[NamedConjunctT])
    case DisjunctE(index, total, term, tExpr) ⇒ DisjunctE(index, total, substTypeVar(replaceTypeVar, newTypeExpr, term), tExpr.substTypeVar(replaceTypeVar, newTypeExpr))
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

  val monoidIntStandard: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(x: Int, y: Int): Int = x + y
  }

  implicit def monoidSeq[T]: Monoid[Seq[T]] = new Monoid[Seq[T]] {
    override def empty: Seq[T] = Seq()

    override def combine(x: Seq[T], y: Seq[T]): Seq[T] = x ++ y
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

  implicit val monoidDouble: Monoid[Double] = new Monoid[Double] {
    override def empty: Double = 0.0

    override def combine(x: Double, y: Double): Double = x + y
  }

  private[ch] def conjunctionPermutationScore(inExpr: TermExpr): Double = foldMap(inExpr) {
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
  }

  private[ch] def disjunctionPermutationScore(inExpr: TermExpr): Double = foldMap(inExpr) {
    case MatchE(term, cases) ⇒
      disjunctionPermutationScore(term) +
        cases.zipWithIndex.flatMap { case (t, i) ⇒
          // Only count disjunction constructions into terms of exactly the same type as the `term` being matched.
          findFirst(t) { case DisjunctE(index, _, t2, tExpr) if sameConstructor(term.tExpr, tExpr) ⇒
            disjunctionPermutationScore(t2) / cases.length.toDouble +
              (if (index == i) 0 else 1)
          }
        }.sum
  }

  private[ch] def unusedArgs(termExpr: TermExpr): Set[String] = {
    implicit val monoidSetString: Monoid[Set[String]] = new Monoid[Set[String]] {
      override def empty: Set[String] = Set()

      override def combine(x: Set[String], y: Set[String]): Set[String] = x ++ y
    }
    foldMap(termExpr) {
      case CurriedE(heads, body) ⇒ (heads.map(_.name).toSet -- body.freeVars) ++ unusedArgs(body)
      case MatchE(term, cases) ⇒ unusedArgs(term) ++ cases.flatMap {
        // The unused heads in this CurriedE are counted separately by `unusedMatchClauseVars`.
        case CurriedE(List(_), body) ⇒ unusedArgs(body)
        case c ⇒ unusedArgs(c)
      }.toSet
    }

  }
}

sealed trait TermExpr {
  // Syntax helpers.
  def =>:(y: VarE): TermExpr = CurriedE(List(y), this)

  def apply(terms: TermExpr*): TermExpr = tExpr match {
    case #->(_, _) ⇒ AppE(this, if (terms.length == 1) terms.head else ConjunctE(terms))
    case _: ConjunctT | _: NamedConjunctT | _: DisjunctT | _: UnitT ⇒ tExpr.apply(terms: _*)
    case _ ⇒ throw new Exception(s"t.apply(...) is not defined for this term t=$this of type ${tExpr.prettyPrint}")
  }

  def cases(cases: TermExpr*): TermExpr = tExpr match {
    case DisjunctT(_, _, termTypes) ⇒
      val typesOfCaseBodies = cases.zip(termTypes).collect { case (CurriedE(head :: _, body), t) if head.tExpr == t ⇒ body.tExpr }
      if (cases.length == termTypes.length && typesOfCaseBodies.length == cases.length && typesOfCaseBodies.toSet.size == 1)
        MatchE(this, cases.toList)
      else throw new Exception(s"Case match on ${tExpr.prettyPrint} must use a sequence of ${termTypes.length} functions with matching types of arguments (${termTypes.map(_.prettyPrint).mkString("; ")}) and bodies, but have ${cases.map(_.tExpr.prettyPrint).mkString("; ")}")
    case _ ⇒ throw new Exception(s".cases() is not defined for this term of type ${tExpr.prettyPrint}")
  }

  def equiv(y: TermExpr): Boolean = simplify == y.simplify

  def substTypeVar(from: TermExpr, to: TermExpr): TermExpr = from.tExpr match {
    case tp@TP(_) ⇒ TermExpr.substTypeVar(tp, to.tExpr, this)
    case _ ⇒ throw new Exception(s"substTypeVar requires a type variable as type of expression $from, but found ${from.tExpr.prettyPrint}")
  }

  def apply(i: Int): TermExpr = tExpr match {
    case NamedConjunctT(_, _, accessors, _) ⇒
      if (i >= 0 && i < accessors.length)
        ProjectE(i, this)
      else throw new Exception(s".apply($i) is undefined since this conjunction type has only ${accessors.length} parts")
    case _ ⇒ throw new Exception(s".apply(i: Int) is defined only on conjunction types while this is ${tExpr.prettyPrint}")
  }

  def apply(acc: String): TermExpr = tExpr match {
    case NamedConjunctT(_, _, accessors, _) ⇒
      val i = accessors.indexOf(acc)
      if (i >= 0)
        ProjectE(i, this)
      else throw new Exception(s".apply($acc) is undefined since this conjunction type does not support this accessor (supported accessors: ${accessors.mkString(", ")})")
    case _ ⇒ throw new Exception(s".apply(acc: String) is defined only on conjunction types while this is ${tExpr.prettyPrint}")
  }

  def informationLossScore = (
    TermExpr.unusedArgs(this).size
    , unusedTupleParts + unusedMatchClauseVars
    , TermExpr.conjunctionPermutationScore(this) + TermExpr.disjunctionPermutationScore(this)
    , TermExpr.argsMultiUseCountShallow(this)
    , TermExpr.argsMultiUseCountDeep(this)
  )

  def tExpr: TypeExpr

  def prettyPrint: String = prettyRename.prettyPrintWithParentheses(0)

  override lazy val toString: String = this match {
    case VarE(name, _) ⇒ s"$name"
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

  def prettyRename: TermExpr = {
    val oldVars = usedVars // Use a `Seq` here rather than a `Set` for the list of variable names.
    // This achieves deterministic renaming, which is important for checking that different terms are equivalent up to renaming.
    val newVars = prettyVars.take(oldVars.length).toSeq
    this.renameAllVars(oldVars, newVars)
  }

  private[ch] def accessor(index: Int): String = tExpr match {
    case NamedConjunctT(_, _, accessors, _) ⇒ accessors(index).toString
    case ConjunctT(_) ⇒ s"_${index + 1}"
    case _ ⇒ throw new Exception(s"Internal error: Cannot perform projection for term $toString : ${tExpr.prettyPrint} because its type is not a conjunction")
  }

  def simplify: TermExpr = TermExpr.simplifyWithEtaUntilStable(this)

  private[ch] def simplifyOnce(withEta: Boolean = false): TermExpr = this

  private[ch] def unusedMatchClauseVars: Double = {
    import TermExpr.monoidDouble
    TermExpr.foldMap[Double](this) {
      case MatchE(_, cases) ⇒ cases.map {
        case CurriedE(List(prop), body) ⇒ if (body.freeVars contains prop.name) 0.0 else 1.0
        case c ⇒ c.unusedMatchClauseVars
      }.sum / cases.length.toDouble
    }
  }

  private[ch] lazy val unusedTupleParts: Int = usedTuplePartsSeq
    .groupBy(_._1) // Map[TermExpr, Seq[(TermExpr, Int)]]
    .mapValues(_.map(_._2).distinct) // Map[TermExpr, Seq[Int]]
    .map { case (term, parts) ⇒
    val totalParts = term.tExpr.conjunctSize
    totalParts - parts.length
  }.count(_ > 0)

  private[ch] lazy val usedTuplePartsSeq: Seq[(TermExpr, Int)] = {
    TermExpr.foldMap(this) {
      case ProjectE(index, term) ⇒ Seq((term, index + 1)) ++ term.usedTuplePartsSeq
    }
  }

  lazy val freeVars: Seq[String] = {
    import TermExpr.monoidSeq
    TermExpr.foldMap(this) {
      case VarE(name, _) ⇒ Seq(name)
      case CurriedE(heads, body) ⇒ body.freeVars.filterNot(heads.map(_.name).toSet.contains)
      case p: ProjectE ⇒ p.getProjection.map(_.freeVars).getOrElse(p.term.freeVars)
    }.distinct
  }

  lazy val usedVars: Seq[String] = {
    import TermExpr.monoidSeq
    TermExpr.foldMap(this) {
      case VarE(name, _) ⇒ Seq(name)
      case p: ProjectE ⇒ p.getProjection.map(_.usedVars).getOrElse(p.term.usedVars)
    }.distinct
  }

  def varCount(varName: String): Int = {
    implicit val monoidInt: Monoid[Int] = TermExpr.monoidIntStandard
    TermExpr.foldMap(this) {
      case VarE(name, _) ⇒ if (name == varName) 1 else 0
    }
  }

  // Rename all variable at once *everywhere* in the expression. (This is not the alpha-conversion!)
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

final case class VarE(name: String, tExpr: TypeExpr) extends TermExpr

final case class AppE(head: TermExpr, arg: TermExpr) extends TermExpr {

  // The type of AppE is computed from the types of its arguments.
  // Make this a `val` to catch bugs early.
  val tExpr: TypeExpr = head.tExpr match {
    case hd #-> body if hd == arg.tExpr ⇒ body
    case _ ⇒ throw new Exception(s"Internal error: Invalid head type in application $this: `${head.tExpr.prettyPrint}` must be a function with argument type `${arg.tExpr.prettyPrint}`")
  }

  private[ch] override def simplifyOnce(withEta: Boolean): TermExpr = {
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
  def tExpr: TypeExpr = heads.reverse.foldLeft(body.tExpr) { case (prev, head) ⇒ head.tExpr ->: prev }

  private[ch] override def simplifyOnce(withEta: Boolean): TermExpr = {
    body.simplifyOnce(withEta) match {
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

      case simplifiedBody ⇒ if (heads.isEmpty) simplifiedBody else this.copy(body = simplifiedBody)
    }
  }

}

final case class UnitE(tExpr: TypeExpr) extends TermExpr

final case class NamedConjunctE(terms: Seq[TermExpr], tExpr: NamedConjunctT) extends TermExpr {
  private[ch] override def simplifyOnce(withEta: Boolean): TermExpr = {
    val simplifiedTerms = terms.map(_.simplifyOnce(withEta))
    // Detect the identity pattern:
    // NamedConjunctE(Seq(ProjectE(0, t : N), ProjectE(1, t: N), ...), N) with the same term `t`
    val projectedTerms = simplifiedTerms.zipWithIndex.collect {
      case (ProjectE(j, t), i) if i == j && t.tExpr == tExpr ⇒ t
    }
    projectedTerms.headOption match {
      case Some(t) if projectedTerms.size == terms.size && projectedTerms.toSet.size == 1 ⇒ t
      case _ ⇒ this.copy(terms = simplifiedTerms)
    }
  }
}

// This is now used only for java-style arg groups. A tuple is represented by a NamedConjunctE.
final case class ConjunctE(terms: Seq[TermExpr]) extends TermExpr {
  def tExpr: TypeExpr = ConjunctT(terms.map(_.tExpr))

  private[ch] override def simplifyOnce(withEta: Boolean): TermExpr = this.copy(terms = terms.map(_.simplifyOnce(withEta)))
}

// The `term` should be a ConjunctT or a NamedConjunctT
final case class ProjectE(index: Int, term: TermExpr) extends TermExpr {
  def getProjection: Option[TermExpr] = term match {
    case c: ConjunctE ⇒ Some(c.terms(index))
    case c: NamedConjunctE ⇒ Some(c.terms(index))
    case _ ⇒ None
  }

  override def tExpr: TypeExpr = term.tExpr match {
    case ConjunctT(terms) ⇒ terms(index)
    case NamedConjunctT(constructor, _, _, wrapped) ⇒ wrapped match {
      case Nil if index == 0 ⇒ UnitT(constructor)
      case _ :: _ ⇒ wrapped(index)
      // Otherwise it is an error!
      case _ ⇒ throw new Exception(s"Internal error: Invalid projection to index $index for a named conjunct $term : ${term.tExpr.prettyPrint} with multiplicity 1")
    }
    case _ ⇒ throw new Exception(s"Internal error: Invalid projection term $term whose type ${term.tExpr.prettyPrint} is not a conjunction")
  }

  private[ch] override def simplifyOnce(withEta: Boolean): TermExpr = term.simplifyOnce(withEta) match {
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
  override def tExpr: TypeExpr = cases match {
    case te :: tail ⇒
      te match {
        case CurriedE(List(_), body) ⇒
          val tpe = body.tExpr
          if (tail.exists(_.asInstanceOf[CurriedE].body.tExpr != tpe))
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

  private[ch] override def simplifyOnce(withEta: Boolean): TermExpr = {
    lazy val casesSimplified = cases.map(_.simplifyOnce(withEta))
    term.simplifyOnce(withEta) match {
      case DisjunctE(index, total, t, _) ⇒
        if (total == cases.length) {
          AppE(cases(index).simplifyOnce(withEta), t).simplifyOnce(withEta)
        } else throw new Exception(s"Internal error: MatchE with ${cases.length} cases applied to DisjunctE with $total parts, but must be of equal size")

      // Detect the identity patterns:
      // MatchE(_, List(a ⇒ DisjunctE(0, total, a, _), a ⇒ DisjunctE(1, total, a, _), ...))
      // MatchE(_, a: T1 ⇒ DisjunctE(i, total, NamedConjunctE(List(ProjectE(0, a), Project(1, a), ...), T1), ...), _)
      case t ⇒
        if (cases.nonEmpty && {
          casesSimplified.zipWithIndex.forall {
            case (CurriedE(List(head@VarE(_, _)), DisjunctE(i, len, x, _)), ind)
              if x == head && len == cases.length && ind == i
            ⇒ true
            case (CurriedE(List(head@VarE(_, headT)), DisjunctE(i, len, NamedConjunctE(projectionTerms, conjT), _)), ind) ⇒
              len == cases.length && ind == i && headT == conjT &&
                projectionTerms.zipWithIndex.forall {
                  case (ProjectE(k, head1), j) if k == j && head1 == head ⇒ true
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
final case class DisjunctE(index: Int, total: Int, term: TermExpr, tExpr: TypeExpr) extends TermExpr {
  private[ch] override def simplifyOnce(withEta: Boolean): TermExpr = this.copy(term = term.simplifyOnce(withEta))
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

