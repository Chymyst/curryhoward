package io.chymyst.ch

import io.chymyst.ch.TermExpr.ProofTerm

// Premises are reverse ordered.
final case class Sequent[T](premises: List[TypeExpr[T]], goal: TypeExpr[T], freshVar: FreshIdents) {
  val premiseVars: List[PropE[T]] = premises.map(PropE(freshVar(), _))

  def substitute(p: ProofTerm[T]): ProofTerm[T] = {
    // Assuming that p takes as many arguments as our premises, substitute all premises into p.
    // AppE( AppE( AppE(p, premise3), premise2), premise1)
    premiseVars.foldLeft(p) { case (prev, premise) ⇒ AppE(prev, premise) }
  }

  private def constructResultType(result: TypeExpr[T], skip: Int = 0): TypeExpr[T] = {
    premiseVars.drop(skip).foldLeft(result) { case (prev, premiseVar) ⇒ premiseVar.tExpr :-> prev }
  }

  def constructResultTerm(result: TermExpr[T]): TermExpr[T] = CurriedE(premiseVars, result)
}
