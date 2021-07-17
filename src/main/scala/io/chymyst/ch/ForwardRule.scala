package io.chymyst.ch

import io.chymyst.ch.ForwardRule.BackTransform

final case class ForwardRule(
  name: String,
  applyTo: Sequent => Seq[RuleResult]
)

object ForwardRule {
  type BackTransform = Seq[TermExpr] => TermExpr
}

final case class RuleResult(ruleName: String, newSequents: Seq[Sequent], backTransform: BackTransform)
