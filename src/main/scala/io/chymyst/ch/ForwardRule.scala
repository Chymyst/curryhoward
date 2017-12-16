package io.chymyst.ch

import io.chymyst.ch.ForwardRule.BackTransform
import io.chymyst.ch.TermExpr.ProofTerm

final case class ForwardRule[T](
  name: String,
  applyTo: Sequent[T] ⇒ Seq[RuleResult[T]]
)

object ForwardRule {
  type BackTransform[T] = Seq[ProofTerm[T]] ⇒ ProofTerm[T]
}

final case class RuleResult[T](ruleName: String, newSequents: Seq[Sequent[T]], backTransform: BackTransform[T])
