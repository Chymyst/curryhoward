package io.chymyst.ch

import io.chymyst.ch.LJT.{followsFromAxioms, invertibleRules, invertibleAmbiguousRules, nonInvertibleRulesForSequent}
import io.chymyst.ch.TermExpr.ProofTerm


object TheoremProver {

  private def debug = Macros.options contains "prover"

  private[ch] def inhabitInternal[T](typeStructure: TypeExpr[T]): Either[String, (Option[String], TermExpr[T])] = {
    // TODO Check that there aren't repeated types among the curried arguments, print warning.
    TheoremProver.findProofs(typeStructure) match {
      case (Nil, _) ⇒
        Left(s"type ${typeStructure.prettyPrint} cannot be implemented")
      case (List(termFound), allTerms) ⇒
        allTerms.length match {
          case count if count > 1 ⇒
            val message = s"type ${typeStructure.prettyPrint} has $count implementations (laws need checking?):\n ${allTerms.map(_.prettyPrint).mkString(" ;\n ")} ."
            Right((Some(message), termFound))
          case _ ⇒
            Right((None, termFound))
        }
      case (list, _) ⇒
        Left(s"type ${typeStructure.prettyPrint} can be implemented in ${list.length} inequivalent ways:\n ${list.map(_.prettyPrint).mkString(" ;\n ")} .")
    }
  }

  def explode[T](src: Seq[Seq[T]]): Seq[Seq[T]] = {
    src.foldLeft[Seq[Seq[T]]](Seq(Seq())) { case (prevSeqSeq, newSeq) ⇒
      for {
        prev ← prevSeqSeq
        next ← newSeq
      } yield prev :+ next
    }
  }

  private[ch] val freshVar = new FreshIdents(prefix = "x")

  private[ch] def findProofs[T](typeStructure: TypeExpr[T]): (List[TermExpr[T]], Seq[TermExpr[T]]) = {
    val mainSequent = Sequent[T](List(), typeStructure, freshVar)
    // We can do simplifyWithEta only at this last stage. Otherwise rule transformers will not be able to find the correct number of arguments in premises.
    val proofTerms = findProofTerms(mainSequent).map(_.prettyRename.simplify(withEta = true)).distinct
    if (debug) {
      val prettyPT = proofTerms.map(p ⇒ (p.prettyPrint, p.unusedArgs.size, p.unusedTupleParts, p.unusedArgs, p.usedTuplePartsSeq.distinct.map { case (te, i) ⇒ (te.prettyPrint, i) }))
        .sortBy { case (_, s1, s2, _, _) ⇒ s1 + s2 }
      val proofTermsMessage = if (prettyPT.isEmpty) "no proof terms." else s"proof terms:\n ${prettyPT.mkString(" ;\n ")} ."
      println(s"DEBUG: for sequent $mainSequent, obtained $proofTermsMessage")
    }
    // Return the group of proofs that leave the smallest number of values unused, but has the smallest use count of those that are used.
    val chosenTerms = proofTerms
      .map(proofTerm ⇒ (proofTerm, (proofTerm.unusedArgs.size + proofTerm.unusedTupleParts + proofTerm.unusedMatchClauseVars, proofTerm.argsMultiUseCount)))
      .groupBy(_._2) // Map[Int, Seq[(ProofTerm[T], Int)]]
      .mapValues(_.map(_._1)) // Map[Int, Seq[ProofTerm[T]]]
      .toSeq.sortBy(_._1) // Seq[(Int, Seq[ProofTerm[T]])]
      .headOption // Option[(Int, Seq[ProofTerm[T]])]
      .map(_._2.toList) // Option[List[ProofTerm[T]]]
      .getOrElse(List())
    (chosenTerms, proofTerms)
  }

  // Main recursive function that computes the list of available proofs for a sequent.
  // The main assumption is that the depth-first proof search terminates.
  // No loop checking is performed on sequents.
  private[ch] def findProofTerms[T](sequent: Sequent[T]): Seq[ProofTerm[T]] = {
    def concatProofs(ruleResult: RuleResult[T]): Seq[ProofTerm[T]] = {
      if (debug) println(s"DEBUG: applied rule ${ruleResult.ruleName} to sequent $sequent, new sequents ${ruleResult.newSequents.map(_.toString).mkString("; ")}")
      // All the new sequents need to be proved before we can continue. They may have several proofs each.
      val newProofs: Seq[Seq[ProofTerm[T]]] = ruleResult.newSequents.map(findProofTerms)
      val explodedNewProofs: Seq[Seq[ProofTerm[T]]] = TheoremProver.explode(newProofs)
      val transformedProofs = explodedNewProofs.map(ruleResult.backTransform)
      val result = transformedProofs.map(_.simplify()).distinct // Note: at this point, it is a mistake to do prettyRename, because we are calling this function recursively.
      // We will call prettyRename() at the very end of the proof search.
      if (debug) {
        println(s"DEBUG: for sequent $sequent, after rule ${ruleResult.ruleName}, transformed ${transformedProofs.length} proof terms:\n ${transformedProofs.mkString(" ;\n ")} ,\nafter simplifying:\n ${result.mkString(" ;\n ")} .")
        //        println(s"debug: types of transformed proofs:\n ${transformedProofs.map(_.tExpr.prettyPrint).mkString(";\n ")},\nafter simplify:\n ${result.map(_.tExpr.prettyPrint).mkString(";\n ")}")
      }
      result
    }

    // Check whether the sequent follows directly from an axiom.
    val fromAxioms: Seq[ProofTerm[T]] = followsFromAxioms(sequent) // This could be empty or non-empty.
    // Even if the sequent follows directly from axioms, we should try applying rules in hopes of getting more proofs.

    if (debug && fromAxioms.nonEmpty) println(s"DEBUG: sequent $sequent followsFromAxioms: ${fromAxioms.map(_.prettyPrint)}")

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
    if (debug) {
      val termsMessage = if (terms.nonEmpty) "terms:\n " + terms.map(_.prettyPrint).mkString(" ;\n ") + " ,\n" else "no terms"
      println(s"DEBUG: returning $termsMessage for sequent $sequent")
    }
    terms
  }

}
