package io.chymyst.ch

import io.chymyst.ch.LJT.{followsFromAxioms, invertibleRules, invertibleAmbiguousRules, nonInvertibleRulesForSequent}
import io.chymyst.ch.TermExpr.ProofTerm


object TheoremProver {
  def explode[T](src: Seq[Seq[T]]): Seq[Seq[T]] = {
    src.foldLeft[Seq[Seq[T]]](Seq(Seq())) { case (prevSeqSeq, newSeq) ⇒
      for {
        prev ← prevSeqSeq
        next ← newSeq
      } yield prev :+ next
    }
  }

  def apply[T](typeStructure: TypeExpr[T]): List[TermExpr[T]] = findProofs(typeStructure)

  private[ch] val freshVar = new FreshIdents(prefix = "x")

  def findProofs[T](typeStructure: TypeExpr[T]): List[TermExpr[T]] = {
    val mainSequent = Sequent[T](List(), typeStructure, freshVar)
    val proofs: Seq[ProofTerm[T]] = findProofTerms(mainSequent)
    proofs.toList
  }

  // Main recursive function that computes the list of available proofs for a sequent.
  // The main assumption is that the depth-first proof search terminates.
  // No loop checking is performed on sequents.
  def findProofTerms[T](sequent: Sequent[T]): Seq[ProofTerm[T]] = {
    def concatProofs[T](ruleResult: RuleResult[T]): Seq[ProofTerm[T]] = {
      println(s"debug: applied rule ${ruleResult.ruleName} to sequent $sequent, new sequents ${ruleResult.newSequents}")
      // All the new sequents need to be proved before we can continue. They may have several proofs each.
      val newProofs: Seq[Seq[ProofTerm[T]]] = ruleResult.newSequents.map(findProofTerms)
      val explodedNewProofs: Seq[Seq[ProofTerm[T]]] = TheoremProver.explode(newProofs)
      val transformedProofs = explodedNewProofs.map(ruleResult.backTransform).map(_.simplify)
      println(s"debug: transformed proof terms $transformedProofs")
      transformedProofs
    }

    // Check whether the sequent follows directly from an axiom.
    val fromAxioms: Seq[ProofTerm[T]] = followsFromAxioms(sequent) // This could be empty or non-empty.
    // Even if the sequent follows directly from axioms, we should try applying rules in hopes of getting more proofs.

    if (fromAxioms.nonEmpty) println(s"debug: sequent $sequent followsFromAxioms: $fromAxioms")

    // Try each rule on sequent. If rule applies, obtain the next sequent.
    // If all rules were invertible and non-ambiguous, we would return `fromAxioms ++ fromInvertibleRules`.

    // If some non-ambiguous invertible rule applies, there is no need to try any other rules.
    // We should apply that invertible rule and proceed from there.
    val fromRules: Seq[ProofTerm[T]] = invertibleRules[T].view.flatMap(_.applyTo(sequent)).headOption match {
      case Some(ruleResult) ⇒ concatProofs(ruleResult) ++ fromAxioms
      case None ⇒
        // Try invertible ambiguous rules. Each of these rules may generate more than one new sequent,
        // and each of these sequents yields a proof if the original formula has a proof.
        // We need to gather and concatenate all these proofs.
        // We proceed to non-invertible rules only if no rules apply at this step.
        val fromInvertibleAmbiguousRules = invertibleAmbiguousRules[T]
          .flatMap(_.applyTo(sequent))
          .flatMap(concatProofs)
        if (fromInvertibleAmbiguousRules.nonEmpty)
          fromInvertibleAmbiguousRules ++ fromAxioms
        else {
          // No invertible rules apply, so we need to try all non-invertible (i.e. not guaranteed to work) rules.
          // Each non-invertible rule will generate some proofs or none.
          // If a rule generates no proofs, another rule should be used.
          // If a rule generates some proofs, we append them to `fromAxioms` and keep trying another rule.
          // If no more rules apply here, we return `fromAxioms`.
          // Use flatMap to concatenate all results from all applicable non-invertible rules.
          val fromNoninvertibleRules: Seq[ProofTerm[T]] = nonInvertibleRulesForSequent[T](sequent)
            .flatMap(_.applyTo(sequent))
            .flatMap(concatProofs)
          fromNoninvertibleRules ++ fromAxioms
        }
    }
    val terms = fromRules.distinct
    println(s"debug: returning terms $terms")
    terms
  }

}
