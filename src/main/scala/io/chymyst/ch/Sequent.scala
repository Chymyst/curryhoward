package io.chymyst.ch

import java.util.concurrent.atomic.AtomicInteger

// Premises are straight ordered, so (A, B, C) |- D is Sequent(List(A, B, C), D, _)
// New bound variables are always introduced fresh, so the generated code never has any need for alpha-conversion.
final case class Sequent(premises: List[TypeExpr], goal: TypeExpr, freshVar: FreshIdents) {
  lazy val premiseVars: List[VarE] = premises.map(VarE(freshVar(), _))

  /** Assuming that p takes as many arguments as our premises, substitute all premises into p.
    * This will construct the term AppE( AppE( AppE(p, premise1), premise2), premise3)
    *
    * @param p Proof term to apply to our premises.
    * @return Resulting term.
    */
  def substituteInto(p: TermExpr): TermExpr = TermExpr.applyCurried(p, premiseVars).simplifyOnce()

  /** Construct a function term that takes all the sequent's premises as arguments and returns a given term.
    *
    * @param result A given term that should use the sequent's `premiseVars`.
    * @return A function term.
    */
  def constructResultTerm(result: TermExpr): TermExpr = CurriedE(premiseVars, result).simplifyOnce()

  override lazy val toString: String = s"[${premises.map(_.prettyPrint).mkString("; ")} |- ${goal.prettyPrint}]"
}

class FreshIdents(prefix: String) {
  private val identCount = new AtomicInteger(0)

  private def newIdentCount: Int = identCount.incrementAndGet()

  def apply(): String = prefix + newIdentCount.toString
}
