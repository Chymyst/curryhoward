package io.chymyst.ch

import java.util.concurrent.atomic.AtomicInteger

import io.chymyst.ch.TermExpr.ProofTerm

// Premises are straight ordered, so (A, B, C) |- D is Sequent(List(A, B, C), D, _)
// New bound variables are always introduced fresh, so the generated code never has any need for alpha-conversion.
final case class Sequent[T](premises: List[TypeExpr[T]], goal: TypeExpr[T], freshVar: FreshIdents) {
  lazy val premiseVars: List[PropE[T]] = premises.map(PropE(freshVar(), _))

  /** Assuming that p takes as many arguments as our premises, substitute all premises into p.
    * This will construct the term AppE( AppE( AppE(p, premise1), premise2), premise3)
    *
    * @param p Proof term to apply to our premises.
    * @return Resulting term.
    */
  def substitute(p: ProofTerm[T]): ProofTerm[T] = TermExpr.applyToVars(p, premiseVars)

  /** Construct a function term that takes all the sequent's premises as arguments and returns a given term.
    *
    * @param result A given term that should use the sequent's `premiseVars`.
    * @return A function term.
    */
  def constructResultTerm(result: TermExpr[T]): TermExpr[T] = CurriedE(premiseVars, result)

  override lazy val toString: String = s"[(${premises.mkString(", ")}) |- $goal]"
}

class FreshIdents(prefix: String) {
  private val identCount = new AtomicInteger(0)

  private def newIdentCount: Int = identCount.incrementAndGet()

  def apply(): String = prefix + newIdentCount.toString
}
