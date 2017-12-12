package io.chymyst.ch

import java.util.concurrent.atomic.AtomicInteger

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox

// TODO:
/*  Priority is given in parentheses.
- refactor disjuncts to contain at most 2 elements (0) ??? probably not necessary.
- reverse the direction of CurriedE because we want to reuse argument lists more (0)
- implement all rules of the LJT calculus (1)
- check unused arguments and sort results accordingly (3)
- only output the results with smallest number of unused arguments (3)
- implement uncurried functions (6)
- make sure Unit works (2)
- support natural syntax def f[T](x: T): T = implement (3)
- use c.Type instead of String (3)
- use blackbox macros instead of whitebox if possible (5)
- use a special subclass of Function1 that also carries symbolic information about the lambda-term (6)
- add more error messages: print alternative lambda-terms when we refuse to implement (5)
- use a symbolic evaluator to simplify the lambda-terms (5)
- support sealed traits / case classes (5)

 Release as a separate open-source project after (1)-(4) are done.
 */

class FreshIdents(prefix: String) {
  private val identCount = new AtomicInteger(0)

  private def newIdentCount: Int = identCount.incrementAndGet()

  def apply(): String = prefix + newIdentCount.toString
}

object CHTypes {
  private val freshSubformulas = new FreshIdents(prefix = "f")

  def subformulas[T](typeStructure: TypeExpr[T]): Set[TypeExpr[T]] = Set(typeStructure) ++ (typeStructure match {
    case DisjunctT(terms) ⇒ terms.flatMap(subformulas)
    case ConjunctT(terms) ⇒ terms.flatMap(subformulas)
    case head :-> body ⇒ subformulas(head) ++ subformulas(body) ++ (head match {
      case DisjunctT(terms) ⇒ terms.flatMap(t ⇒ subformulas(:->(t, body)))
      case ConjunctT(terms) ⇒ subformulas(terms.foldRight(body) { case (t, prev) ⇒ t :-> prev })
      case _ :-> bd ⇒ subformulas(bd :-> body) // Special subformula case for implication of the form (hd ⇒ bd) ⇒ body
      case _ ⇒ Seq() // `head` is an atomic type
    })
    case _ ⇒ Seq() // `typeStructure` is an atomic type
  }).toSet

  def explode[T](src: Seq[Seq[T]]): Seq[Seq[T]] = {
    src.foldLeft[Seq[Seq[T]]](Seq(Seq())) { case (prevSeqSeq, newSeq) ⇒
      for {
        prev ← prevSeqSeq
        next ← newSeq
      } yield prev :+ next
    }
  }

  // Subformula index.
  type SFIndex = Int

  // Premises are reverse ordered.
  final case class Sequent[T](premises: List[SFIndex], goal: SFIndex, sfIndexOfTExpr: Map[TypeExpr[T], SFIndex]) {
    val tExprAtSFIndex: Map[SFIndex, TypeExpr[T]] = sfIndexOfTExpr.map { case (k, v) ⇒ v → k }

    val premiseVars: List[PropE[T]] = premises.map { premiseSFIndex ⇒ PropE(freshVar(), tExprAtSFIndex(premiseSFIndex)) }

    def substitute(p: ProofTerm[T]): ProofTerm[T] = {
      // Assuming that p takes as many arguments as our premises, substitute all premises into p.
      // AppE( AppE( AppE(p, premise3), premise2), premise1)
      premiseVars.foldLeft(p) { case (prev, premise) ⇒ AppE(prev, premise) }
    }

    private def constructResultType(result: CHTypes.TypeExpr[T], skip: Int = 0): TypeExpr[T] = {
      premiseVars.drop(skip).foldLeft(result) { case (prev, premiseVar) ⇒ premiseVar.tExpr :-> prev }
    }

    def constructResultTerm(result: TermExpr[T]): TermExpr[T] = CurriedE(premiseVars, result)

    // Convenience method.
    def goalExpr: TypeExpr[T] = tExprAtSFIndex(goal)
  }

  type ProofTerm[T] = TermExpr[T]

  type BackTransform[T] = Seq[ProofTerm[T]] ⇒ ProofTerm[T]

  final case class ForwardRule[T](name: String, applyTo: Sequent[T] ⇒ Option[(Seq[Sequent[T]], BackTransform[T])])

  private val freshVar = new FreshIdents(prefix = "x")

  def followsFromAxioms[T](sequent: Sequent[T]): Seq[ProofTerm[T]] = {
    // The LJT calculus has three axioms. We use the Id axiom and the T axiom only, because the F axiom is not useful for code generation.

    val fromIdAxiom: Seq[TermExpr[T]] = sequent.premiseVars
      .zip(sequent.premises)
      .filter(_._2 == sequent.goal && sequent.goalExpr.isAtomic)
      .map { case (premiseVar, _) ⇒
        // Generate a new term x1 ⇒ x2 ⇒ ... ⇒ xN ⇒ xK with fresh names. Here `xK` is one of the variables, selecting the premise that is equal to the goal.
        // At this iteration, we already selected the premise that is equal to the goal.
        sequent.constructResultTerm(premiseVar)
      }
    val fromTAxiom: Seq[TermExpr[T]] = sequent.goalExpr match {
      case unitT: UnitT[T] ⇒ Seq(sequent.constructResultTerm(UnitE(unitT)))
      case _ ⇒ Seq()
    }
    fromIdAxiom ++ fromTAxiom
  }

  // G* |- A ⇒ B when (G*, A) |- B  -- rule ->R
  private def ruleImplicationAtRight[T] = ForwardRule[T](name = "->R", sequent ⇒
    sequent.goalExpr match {
      case a :-> b ⇒
        val aIndex = sequent.sfIndexOfTExpr(a)
        val bIndex = sequent.sfIndexOfTExpr(b)
        val newSequent = sequent.copy(premises = aIndex :: sequent.premises, goal = bIndex)
        Some(Seq(newSequent), { proofTerms ⇒
          // This rule expects only one sub-proof term.
          val subProof = proofTerms.head
          // `subProof` is the proof of (G, A) |- B, and we need a proof of G |- A ⇒ B.
          // `subProof` is x ⇒ y ⇒ ... ⇒ z ⇒ a ⇒ <some term depending on (a, x, y, z)>
          // This proof will be exactly what we need if we reverse the order of curried arguments w.r.t. the list order of premises.
          subProof
        })
      case _ ⇒ None
    })

  // (G*, X, X ⇒ A) |- B when (G*, X, A) |- B  -- rule ->L1
  // Note: there may be several ways of choosing X and X ⇒ A, which lead to equivalent derivations - if so, we have an ambiguous implementation, which is an error.
  // We can signal this error early since this rule is invertible.
  private def ruleImplicationAtLeft1[T] = ForwardRule[T](name = "->L1", sequent ⇒
    for {
      atomicPremise ← sequent.premises.find(sfIndex ⇒ sequent.tExprAtSFIndex(sfIndex).isAtomic)
      implicationPremise ← sequent.premises.find(sfIndex ⇒ sequent.tExprAtSFIndex(sfIndex) match {
        case head :-> body ⇒ sequent.sfIndexOfTExpr(head) == atomicPremise
        case _ ⇒ false
      })
    } yield {

      ???
    }
  )

  /*

  Axioms:
  -------

  (G*, X) |- X  -- axiom Id -- here X is atomic, although the same rule would be valid for non-atomic X, we will not use it because in that way we avoid duplication of derivations.
  G* |- 1  -- axiom T -- here 1 represents the Unit type, and the expression must be constructed somehow in the term.
  (G*, 0) |- A  -- axiom F -- we will not use it. Instead, we will treat `Nothing` as just another type parameter. (We will treat `Any` in this way, too.)

  Invertible rules:
  -----------------

  (G*, A & B) |- C when (G*, A, B) |- C  -- rule &L
  G* |- A & B when G* |- A and G* |- B  -- rule &R -- duplicates the context G*
  (G*, A + B) |- C when (G*, A) |- C and (G*, B) |- C  -- rule +L -- duplicates the context G*
  G* |- A ⇒ B when (G*, A) |- B  -- rule ->R
  (G*, X, X ⇒ A) |- B when (G*, X, A) |- B  -- rule ->L1 -- here X is atomic, although the same rule would be valid for non-atomic X.
  (G*, (A & B) ⇒ C) |- D when (G*, A ⇒ B ⇒ C) |- D  -- rule ->L2
  (G*, (A + B) ⇒ C) |- D when (G*, A ⇒ C, B ⇒ C) |- D  - rule ->L3

  Non-invertible rules:
  ---------------------

  G* |- A + B when G* |- A  -- rule +R1
  G* |- A + B when G* |- B  -- rule +R2
  (G*, (A ⇒ B) ⇒ C) |- D when (G*, C) |- D and (G*, B ⇒ C) |- A ⇒ B  -- rule ->L4

   */

  def invertibleRules[T]: Seq[ForwardRule[T]] = Seq(
    ruleImplicationAtRight,
    ruleImplicationAtLeft1,
    ruleConjunctionAtRight // Put this later in the sequence because it duplicates the context G*.
  )

  // G* |- A & B when G* |- A and G* |- B  -- rule &R -- duplicates the context G*
  private def ruleConjunctionAtRight[T] = ForwardRule[T](name = "&R", sequent ⇒
    sequent.goalExpr match {
      case conjunctType: ConjunctT[T] ⇒ Some((conjunctType.terms.map(t ⇒ sequent.copy(goal = sequent.sfIndexOfTExpr(t))), { proofTerms ⇒
        // This rule takes any number of proof terms.
        sequent.constructResultTerm(ConjunctE(proofTerms.map(p ⇒ sequent.substitute(p))))
      })
      )
      case _ ⇒ None
    }
  )

  // G* |- A + B when G* |- A  -- rule +R1
  // Generate all such rules for any disjunct.
  private def ruleDisjunctionAtRight[T](indexInDisjunct: Int) = ForwardRule[T](name = "+R1", sequent ⇒
    sequent.goalExpr match {
      case disjunctType: DisjunctT[T] ⇒
        val mainExpression = disjunctType.terms(indexInDisjunct)
        Some((List(sequent.copy(goal = sequent.sfIndexOfTExpr(mainExpression))), { proofTerms ⇒
          // This rule expects a single proof term.
          val proofTerm = proofTerms.head
          proofTerm match {
            case CurriedE(heads, body) ⇒ // The goal had some premises.
              CurriedE(heads, DisjunctE(indexInDisjunct, disjunctType.terms.length, body, disjunctType))
            case _ ⇒ // The goal has no premises.
              DisjunctE(0, disjunctType.terms.length, proofTerm, disjunctType)
          }

        })
        )
      case _ ⇒ None
    }
  )

  // (G*, (A ⇒ B) ⇒ C) |- D when (G*, C) |- D and (G*, B ⇒ C) |- A ⇒ B  -- rule ->L4
  // This rule needs a lemma:  |-  ((A ⇒ B) ⇒ C) ⇒ B ⇒ C
  private def ruleImplicationAtLeft4[T] = ForwardRule[T](name = "->L4", sequent ⇒
    Some((List(sequent.copy(goal = ???)), { proofTerms ⇒
      // This rule expects two different proof terms.
      ???
    }
    )

    )
  )

  def nonInvertibleRules[T](sequent: Sequent[T]): Seq[ForwardRule[T]] = {
    // Generate all +Rn rules if the sequent has a disjunction goal.
    (sequent.goalExpr match {
      case DisjunctT(terms) ⇒ terms.indices.map(ruleDisjunctionAtRight[T])
      case _ ⇒ Seq()
    }) ++ Seq(
      //    ruleImplicationAtLeft4
    )
  }

  // Main recursive function that computes the list of available proofs for a sequent.
  // The main assumption is that the depth-first proof search terminates.
  // No loop checking is performed on sequents.
  def findProofTerms[T](sequent: Sequent[T]): Seq[ProofTerm[T]] = {
    // Check whether the sequent follows directly from an axiom.
    val fromAxioms: Seq[ProofTerm[T]] = followsFromAxioms(sequent) // This could be empty or non-empty.
    // Even if the sequent follows from axioms, we should try applying rules in hopes of getting more proofs.

    // Try each rule on sequent. If rule applies, obtain the next sequent.
    // If all rules were invertible, we would return `fromAxioms ++ fromInvertibleRules`.

    // We try applying just one invertible rule and proceed from there.
    val fromRules: Seq[ProofTerm[T]] = invertibleRules[T].view.flatMap(_.applyTo(sequent)).headOption match {
      case Some((newSequents, backTransform)) ⇒
        // All the new sequents need to be proved before we can continue. They may have several proofs each.
        val newProofs: Seq[Seq[ProofTerm[T]]] = newSequents.map(findProofTerms)
        val explodedNewProofs: Seq[Seq[ProofTerm[T]]] = explode(newProofs)
        explodedNewProofs.map(backTransform) ++ fromAxioms

      case None ⇒
        // No invertible rules apply, so we need to try all non-invertible (i.e. not guaranteed to work) rules.
        // Each non-invertible rule will generate some proofs or none.
        // If a rule generates no proofs, another rule should be used.
        // If a rule generates some proofs, we append them to `fromAxioms` and keep trying another rule.
        // If no more rules apply here, we return `fromAxioms`.
        // Use flatMap to concatenate all results from all applicable non-invertible rules.
        val fromNoninvertibleRules: Seq[ProofTerm[T]] = nonInvertibleRules[T](sequent)
          .flatMap(_.applyTo(sequent))
          .flatMap { case ((newSequents, backTransform)) ⇒
            val newProofs: Seq[Seq[ProofTerm[T]]] = newSequents.map(findProofTerms)
            val explodedNewProofs: Seq[Seq[ProofTerm[T]]] = explode(newProofs)
            val finalNewProofs: Seq[ProofTerm[T]] = explodedNewProofs.map(backTransform)
            finalNewProofs
          }
        fromNoninvertibleRules ++ fromAxioms
    }
    fromRules
  }

  sealed trait TypeExpr[+T] {
    override def toString: String = this match {
      case DisjunctT(terms) ⇒ terms.map(_.toString).mkString(" + ")
      case ConjunctT(terms) ⇒ "(" + terms.map(_.toString).mkString(", ") + ")"
      case head :-> body ⇒ s"($head) ..=>.. $body"
      case BasicT(name) ⇒ s"<basic>$name"
      case ConstructorT(fullExpr) ⇒ s"<constructor>$fullExpr"
      case TP(name) ⇒ s"<tparam>$name"
      case OtherT(name) ⇒ s"<other>$name"
      case NothingT(_) ⇒ "0"
      case UnitT(_) ⇒ "1"
    }

    def isAtomic: Boolean

    def map[U](f: T ⇒ U): TypeExpr[U]
  }

  sealed trait NonAtomicTypeExpr {
    def isAtomic: Boolean = false
  }

  sealed trait AtomicTypeExpr[T] {
    def isAtomic: Boolean = true

    def name: T
  }

  object TypeExpr {

    implicit class WithImplication[T](tpe1: TypeExpr[T]) {
      def :->(tpe2: TypeExpr[T]): TypeExpr[T] = CHTypes.:->(tpe1, tpe2)
    }

  }

  final case class DisjunctT[T](terms: Seq[TypeExpr[T]]) extends TypeExpr[T] with NonAtomicTypeExpr {
    override def map[U](f: T ⇒ U): TypeExpr[U] = DisjunctT(terms.map(_.map(f)))
  }

  final case class ConjunctT[T](terms: Seq[TypeExpr[T]]) extends TypeExpr[T] with NonAtomicTypeExpr {
    override def map[U](f: T ⇒ U): TypeExpr[U] = ConjunctT(terms.map(_.map(f)))
  }

  final case class :->[T](head: TypeExpr[T], body: TypeExpr[T]) extends TypeExpr[T] with NonAtomicTypeExpr {
    override def map[U](f: T ⇒ U): TypeExpr[U] = :->(head.map(f), body.map(f))
  }

  final case class NothingT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
    override def map[U](f: T ⇒ U): TypeExpr[U] = NothingT(f(name))
  }

  final case class UnitT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
    override def map[U](f: T ⇒ U): TypeExpr[U] = UnitT(f(name))
  }

  // Type parameter. Use a short name for convenience.
  case class TP[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
    override def map[U](f: T ⇒ U): TypeExpr[U] = TP(f(name))
  }

  case class OtherT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
    override def map[U](f: T ⇒ U): TypeExpr[U] = OtherT(f(name))
  }

  final case class BasicT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
    override def map[U](f: T ⇒ U): TypeExpr[U] = BasicT(f(name))
  }

  final case class ConstructorT[T](name: T) extends TypeExpr[T] with AtomicTypeExpr[T] {
    override def map[U](f: T ⇒ U): TypeExpr[U] = ConstructorT(f(name))
  }

  object TermExpr {
    def propositions[T](termExpr: TermExpr[T]): Set[PropE[T]] = termExpr match {
      case p: PropE[T] ⇒ Set(p) // Need to specify type parameter in match... `case p@PropE(_)` does not work.
      case AppE(head, arg) ⇒ propositions(head) ++ propositions(arg)
      case l: CurriedE[T] ⇒ // Can't pattern-match directly for some reason! Some trouble with the type parameter T.
        l.heads.toSet ++ propositions(l.body)
      case ConjunctE(terms) ⇒ terms.flatMap(propositions).toSet
      case _ ⇒ Set()
    }

  }

  sealed trait TermExpr[+T] {
    def tExpr: TypeExpr[T]

    override def toString: String = this match {
      case PropE(name, tExpr) => s"($name:$tExpr)"
      case AppE(head, arg) => s"($head)($arg)"
      case CurriedE(heads, body) => s"\\(${heads.reverse.mkString(" -> ")} -> $body)"
      case UnitE(tExpr) => "()"
      case ConjunctE(terms) => "(" + terms.map(_.toString).mkString(", ") + ")"
      case DisjunctE(index, total, term, _) => "(" + Seq.fill(index)("0").mkString(" + ") + term.toString + Seq.fill(total - index - 1)("0").mkString(" + ") + ")"
    }

    def map[U](f: T ⇒ U): TermExpr[U]
  }

  final case class PropE[T](name: String, tExpr: TypeExpr[T]) extends TermExpr[T] {
    override def map[U](f: T ⇒ U): PropE[U] = PropE(name, tExpr map f)
  }

  final case class AppE[T](head: TermExpr[T], arg: TermExpr[T]) extends TermExpr[T] {
    override def map[U](f: T ⇒ U): TermExpr[U] = AppE(head map f, arg map f)

    def tExpr: TypeExpr[T] = head match {
      case CurriedE(heads, body) ⇒ CurriedE(heads.drop(1), body).tExpr
    }
  }

  // Note: the order of `heads` is reversed, so `CurriedE(List(1,2,3), body, ...)` represents the term `x3 -> x2 -> x1 -> body`
  final case class CurriedE[T](heads: List[PropE[T]], body: TermExpr[T]) extends TermExpr[T] {
    override def map[U](f: T ⇒ U): TermExpr[U] = CurriedE(heads map (_ map f), body map f)

    def tExpr: TypeExpr[T] = heads.foldLeft(body.tExpr) { case (prev, head) ⇒ head.tExpr :-> prev }
  }

  final case class UnitE[T](tExpr: TypeExpr[T]) extends TermExpr[T] {
    override def map[U](f: T ⇒ U): TermExpr[U] = UnitE(tExpr map f)
  }

  final case class ConjunctE[T](terms: Seq[TermExpr[T]]) extends TermExpr[T] {
    override def map[U](f: T ⇒ U): TermExpr[U] = ConjunctE(terms.map(_.map(f)))

    def tExpr: TypeExpr[T] = ConjunctT(terms.map(_.tExpr))
  }

  final case class DisjunctE[T](index: Int, total: Int, term: TermExpr[T], tExpr: TypeExpr[T]) extends TermExpr[T] {
    override def map[U](f: T ⇒ U): TermExpr[U] = DisjunctE(index, total, term map f, tExpr map f)
  }

}

object CurryHoward {

  import CHTypes._

  private[example] def testType[T]: (String, String) = macro testTypeImpl[T]

  private[example] val basicTypes = List("Int", "String", "Boolean", "Float", "Double", "Long", "Symbol", "Char")
  private val basicRegex = s"(?:scala.|java.lang.)*(${basicTypes.mkString("|")})".r

  // TODO: use c.Type instead of String
  def matchType(c: whitebox.Context)(t: c.Type): TypeExpr[String] = {
    // Could be the weird type [X, Y] => (type expression), or it could be an actual tuple type.
    // `finalResultType` seems to help here.
    val args = t.finalResultType.typeArgs
    val typeParams = t.typeParams // This is nonempty only for the weird types mentioned above.

    t.typeSymbol.fullName match {
      case name if name matches "scala.Tuple[0-9]+" ⇒ ConjunctT(args.map(matchType(c))) //s"(${args.map(matchType(c)).mkString(", ")})"
      case "scala.Function1" ⇒ :->(matchType(c)(args.head), matchType(c)(args(1))) // s"${matchType(c)(args(0))} ..=>.. ${matchType(c)(args(1))}"
      case "scala.Option" ⇒ DisjunctT(Seq(UnitT("Unit"), matchType(c)(args.head))) //s"(1 + ${matchType(c)(args.head)})"
      case "scala.util.Either" ⇒ DisjunctT(Seq(matchType(c)(args.head), matchType(c)(args(1)))) //s"(${matchType(c)(args(0))} + ${matchType(c)(args(1))})"
      case "scala.Any" ⇒ OtherT("_")
      case "scala.Nothing" ⇒ NothingT("Nothing")
      case "scala.Unit" ⇒ UnitT("Unit")
      case basicRegex(name) ⇒ BasicT(name)
      case _ if args.isEmpty && t.baseClasses.map(_.fullName) == Seq("scala.Any") ⇒ TP(t.toString)
      case _ if args.isEmpty ⇒ OtherT(t.toString)
      case _ ⇒ ConstructorT(t.toString)
    }
  }

  def reifyParam(c: whitebox.Context)(term: PropE[String]): c.Tree = {
    import c.universe._
    term match {
      case PropE(name, tExpr) ⇒
        val tpt = tExpr match {
          case _: NothingT[String] ⇒ tq""
          // TODO: Stop using String as type parameter T, use c.Type instead
          case TP(nameT) ⇒
            val tpn = TypeName(nameT)
            tq"$tpn"
        }
        val termName = TermName("t_" + name)
        val param = q"val $termName: $tpt"
        param
    }
  }

  def reifyTerms(c: whitebox.Context)(termExpr: TermExpr[String], paramTerms: Map[PropE[String], c.Tree]): c.Tree = {
    import c.universe._

    termExpr match {
      case p@PropE(name, typeName) =>
        val tn = TermName("t_" + name)
        q"$tn"
      case AppE(head, arg) => q"${reifyTerms(c)(head, paramTerms)}(${reifyTerms(c)(arg, paramTerms)})"
      case CurriedE(heads, body) ⇒ heads.foldLeft(reifyTerms(c)(body, paramTerms)) { case (prevTree, paramE) ⇒
        val param = paramTerms(paramE)
        q"($param ⇒ $prevTree)"
      }
      case UnitE(_) => q"()"
      case ConjunctE(terms) => q"(..${terms.map(t ⇒ reifyTerms(c)(t, paramTerms))})"
    }
  }

  def testTypeImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[(String, String)] = {
    import c.universe._

    val typeT: c.Type = c.weakTypeOf[T]
    val enclosingType = c.internal.enclosingOwner.typeSignature

    val s1 = matchType(c)(typeT.resultType).toString
    val s2 = matchType(c)(enclosingType).toString

    c.Expr[(String, String)](q"($s1,$s2)")
  }

  def ofType[T]: T = macro ofTypeImpl[T]

  def implement[T]: T = macro inhabitImpl[T]

  // TODO: can we replace this with blackbox? Probably, as long as `def f3[X, Y]: X ⇒ Y ⇒ X = ofType` does not work with whitebox anyway.
  def ofTypeImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    val typeT: c.Type = c.weakTypeOf[T]
    inhabitInternal(c)(typeT)
  }

  def inhabitImpl[T](c: whitebox.Context): c.Tree = {
    val typeT = c.internal.enclosingOwner.typeSignature
    inhabitInternal(c)(typeT)
  }

  def inhabitInternal(c: whitebox.Context)(typeT: c.Type): c.Tree = {
    import c.universe._
    val typeStructure: TypeExpr[String] = matchType(c)(typeT)
    val termFound: TermExpr[String] = ITP(typeStructure) match {
      case Nil ⇒
        c.error(c.enclosingPosition, s"type $typeStructure cannot be inhabited")
        null
      case List(term) ⇒ term
      case list ⇒
        c.error(c.enclosingPosition, s"type $typeStructure can be inhabited in ${list.length} different ways")
        null
    }

    println(s"DEBUG: Term found: $termFound, propositions: ${TermExpr.propositions(termFound)}")
    val paramTerms: Map[PropE[String], c.Tree] = TermExpr.propositions(termFound).toSeq.map(p ⇒ p → reifyParam(c)(p)).toMap
    val result = reifyTerms(c)(termFound, paramTerms)
    println(s"DEBUG: returning code: ${showCode(result)}")
    result
  }
}

object ITP {

  def apply(typeStructure: CHTypes.TypeExpr[String]): List[CHTypes.TermExpr[String]] = findProofs(typeStructure)

  def findProofs[T](typeStructure: CHTypes.TypeExpr[T]): List[CHTypes.TermExpr[T]] = {
    import CHTypes._
    val subformulaDictionary: Map[TypeExpr[T], SFIndex] = CHTypes.subformulas(typeStructure).zipWithIndex.toMap
    val mainSequent = Sequent[T](List(), subformulaDictionary(typeStructure), subformulaDictionary)
    val proofs: Seq[ProofTerm[T]] = CHTypes.findProofTerms(mainSequent)
    proofs.toList
  }
}
