package io.chymyst.ch

import io.chymyst.ch.LJT.{followsFromAxioms, invertibleAmbiguousRules, invertibleRules, nonInvertibleRulesForSequent}

import scala.collection.mutable


object TheoremProver {
  private def debug = Macros.options contains "prover"

  private def debugTrace = Macros.options contains "trace"

  // Heuristic to speed up the proof search: select the terms with the best scores and ignore others.
  // The number of terms to take depends on the number of premises in the sequent.
  // We will never take less than 64 = 2 << 6 or more than a million terms (2 << 20) at any step.
  private def maxTermsToSelect(sequent: Sequent): Int = 2 << math.min(6, math.max(20, 2 + sequent.premises.length))

  private[ch] def inhabitInternal(typeStructure: TypeExpr): Either[String, (Option[String], TermExpr)] = {
    // TODO Check that there aren't repeated types among the curried arguments, print warning.
    TheoremProver.findProofs(typeStructure) match {
      case (Nil, _) ⇒
        Left(s"type ${typeStructure.prettyPrint} cannot be implemented")
      case (List(termFound), allTerms) ⇒
        allTerms.length match {
          case count if count > 1 ⇒
            val message = s"type ${typeStructure.prettyPrint} has $count implementations (laws need checking?):\n ${allTerms.map(t ⇒ s"${t.prettyRenamePrint} [score: ${t.informationLossScore}]").mkString(";\n ")}."
            Right((Some(message), termFound))
          case _ ⇒
            Right((None, termFound))
        }
      case (list, _) ⇒
        Left(s"type ${typeStructure.prettyPrint} can be implemented in ${list.length} inequivalent ways:\n ${list.map(t ⇒ s"${t.prettyRenamePrint} [score: ${t.informationLossScore}]").mkString(";\n ")}.")
    }
  }

  def explode[A](src: Seq[Seq[A]]): Seq[Seq[A]] = {
    src.foldLeft[Seq[Seq[A]]](Seq(Seq())) { case (prevSeqSeq, newSeq) ⇒
      for {
        prev ← prevSeqSeq
        next ← newSeq
      } yield prev :+ next
    }
  }

  private[ch] val freshVar = new FreshIdents(prefix = "x")

  private val maxTermsPrinted = 16

  private val sequentsAlreadyProved = mutable.Map[Sequent, Seq[TermExpr]]()
  private val sequentsAlreadyRequested = mutable.Set[Sequent]()

  // This is the main API for proof search. This method is not recursive.
  private[ch] def findProofs(typeStructure: TypeExpr): (List[TermExpr], Seq[TermExpr]) = {
    val t0 = System.currentTimeMillis()
    sequentsAlreadyProved.clear()
    sequentsAlreadyRequested.clear()
    val mainSequent = Sequent(List(), typeStructure, freshVar)
    // We can do simplifyWithEta only at this last stage. Otherwise rule transformers will not be able to find the correct number of arguments in premises.
    val allTermExprs = findTermExprs(mainSequent).map(t ⇒ TermExpr.simplifyWithEtaUntilStable(t.prettyRename).prettyRename).distinct
    if (debug || debugTrace) {
      val prettyPT = allTermExprs.map(p ⇒ (p.informationLossScore, s"${p.prettyRenamePrint}; score = ${p.informationLossScore}: ${TermExpr.unusedArgs(p).size} unused args: ${TermExpr.unusedArgs(p)}; unusedMatchClauseVars=${p.unusedMatchClauseVars}; unusedTupleParts=${p.unusedTupleParts}; used tuple parts: ${p.usedTuplePartsSeq.distinct.map { case (te, i) ⇒ (te.prettyRenamePrint, i) }}"))
        .sortBy(_._1).map(_._2)
      val TermExprsMessage = if (prettyPT.isEmpty) "no final proof terms." else s"${prettyPT.length} final proof terms:\n ${prettyPT.take(maxTermsPrinted).mkString(" ;\n ")} ."
      println(s"DEBUG: for main sequent $mainSequent, obtained $TermExprsMessage This took ${System.currentTimeMillis() - t0} ms")
      // Very verbose
      //      val sequentsSeenMoreThanOnce = sequentsSeen.filter { case (_, v) ⇒ v.length > 1 }.mapValues(_.length)
      //      if (debug && sequentsSeenMoreThanOnce.nonEmpty) println(s"DEBUG: sequents seen more than once are $sequentsSeenMoreThanOnce")
    }
    // Return the group of proofs with the smallest information loss score, and also return all terms found.
    val chosenTerms = allTermExprs
      .groupBy(_.informationLossScore) // Map[score, Seq[TermExpr]]
      .toSeq.sortBy(_._1) // Seq[(score, Seq[TermExpr])] sorted by increasing score
      .headOption // Option[(score, Seq[TermExpr])] the first group of terms all having the lowest score
      .map(_._2.toList) // Option[List[TermExpr]] discard the score value
      .getOrElse(List())
    (chosenTerms, allTermExprs)
  }

  // Main recursive function that computes the list of available proofs for a sequent.
  // The main assumption is that the depth-first proof search terminates.
  // Loop checking is performed on sequents by looking up in `sequentsAlreadyRequested`.
  // The calculus LJT does not generate any loops, but loop checking is activated now because
  // we might add more rules and/or modify the calculus LJT in the future.
  private[ch] def findTermExprs(sequent: Sequent): Seq[TermExpr] = {

    def concatProofs(ruleResult: RuleResult): Seq[TermExpr] = {
      if (debug) println(s"DEBUG: applied rule ${ruleResult.ruleName} to sequent $sequent, new sequents ${ruleResult.newSequents.map(_.toString).mkString("; ")}")
      // All the new sequents need to be proved before we can continue. They may have several proofs each.
      val newProofs: Seq[Seq[TermExpr]] = ruleResult.newSequents.map(findTermExprs)
      val explodedNewProofs: Seq[Seq[TermExpr]] = TheoremProver.explode(newProofs)
      val transformedProofs = explodedNewProofs.map(ruleResult.backTransform)
      val t0 = System.currentTimeMillis()

      val result = transformedProofs.map(_.simplifyOnce()).distinct.sortBy(_.informationLossScore).take(maxTermsToSelect(sequent))
      // Note: at this point, it is a mistake to do prettyRename, because we are calling this function recursively.
      // We will call prettyRename() at the very end of the proof search.
      if (debug) {
        println(s"DEBUG: transformedProofs.map(_.simplify()).distinct took ${System.currentTimeMillis() - t0} ms and produced ${result.size} terms out of ${transformedProofs.size} back-transformed terms; after rule ${ruleResult.ruleName}")
        //        println(s"DEBUG: for sequent $sequent, after rule ${ruleResult.ruleName}, transformed ${transformedProofs.length} proof terms:\n ${transformedProofs.mkString(" ;\n ")} ,\nafter simplifying:\n ${result.mkString(" ;\n ")} .")
      }
      result
    }

    // Check whether we already saw this sequent. We may already have proved it, or we may not yet proved it but already saw it.
    sequentsAlreadyProved.get(sequent) match {
      case Some(terms) ⇒ terms
      case None ⇒

        if (sequentsAlreadyRequested contains sequent) {
          if (debug) println(s"DEBUG: sequent $sequent is looping; will return an empty sequence of proof terms")
          Seq()
        } else {
          sequentsAlreadyRequested.add(sequent)

          // Check whether the sequent follows directly from an axiom.
          val (fromIdAxiom, fromTAxiom) = followsFromAxioms(sequent) // This could be empty or non-empty.
          // If the sequent follows from Id axiom, we will ignore `fromTAxiom`, because this will most likely not yield a good solution.
          // If it follows from axioms, we will still try applying other rules, in hopes of getting more proofs.
          val fromAxioms = if (fromIdAxiom.nonEmpty) fromIdAxiom else fromTAxiom

          if (debug && fromAxioms.nonEmpty) println(s"DEBUG: sequent $sequent followsFromAxioms: ${fromAxioms.map(_.prettyRenamePrint).mkString("; ")}")

          // Try each rule on sequent. If rule applies, obtain the next sequent.
          // If all rules were invertible and non-ambiguous, we would return `fromAxioms ++ fromInvertibleRules`.

          // If some non-ambiguous invertible rule applies, there is no need to try any other rules.
          // We should apply that invertible rule and proceed from there.
          val fromRules: Seq[TermExpr] = invertibleRules.view.flatMap(_.applyTo(sequent)).headOption match {
            case Some(ruleResult) ⇒ concatProofs(ruleResult) ++ fromAxioms
            case None ⇒
              // Try invertible ambiguous rules. Each of these rules may generate more than one new sequent,
              // and each of these sequents yields a proof if the original formula has a proof.
              // We need to gather and concatenate all these proofs.
              // We proceed to non-invertible rules only if no rules apply at this step.
              //              val fromInvertibleAmbiguousRules = invertibleAmbiguousRules
              //                .flatMap(_.applyTo(sequent))
              //                .flatMap(concatProofs)
              //              if (fromInvertibleAmbiguousRules.nonEmpty) {
              //                fromInvertibleAmbiguousRules ++ fromAxioms
              //              } else {
              // No invertible rules apply, so we need to try all non-invertible (i.e. not guaranteed to work) rules.
              // Each non-invertible rule will generate some proofs or none.
              // If a rule generates no proofs, another rule should be used.
              // If a rule generates some proofs, we append them to `fromAxioms` and keep trying another rule.
              // If no more rules apply here, we return `fromAxioms`.
              // Use flatMap to concatenate all results from all applicable non-invertible rules.
              val fromNoninvertibleRules: Seq[TermExpr] =
                (invertibleAmbiguousRules ++ nonInvertibleRulesForSequent(sequent))
                  .flatMap(_.applyTo(sequent))
                  .flatMap(concatProofs)
              fromNoninvertibleRules ++ fromAxioms
            //              }
          }
          val termsFound = fromRules.map(_.simplifyOnce()).distinct
          if (debug || debugTrace) {
            val termsMessage = termsFound.length match {
              case 0 ⇒ "no terms"
              case x ⇒
                val messagePrefix = if (x > maxTermsPrinted) s"first $maxTermsPrinted out of " else ""
                s"$messagePrefix$x terms:\n " + termsFound.take(maxTermsPrinted).map(_.prettyRenamePrint).mkString(" ;\n ") + " ,\n"
            }
            println(s"DEBUG: returning $termsMessage for sequent $sequent")
          }
          // TODO: refactor this if there is a useful caching heuristic
          sequentsAlreadyProved.update(sequent, termsFound)
          termsFound
        }
    }
  }

}
