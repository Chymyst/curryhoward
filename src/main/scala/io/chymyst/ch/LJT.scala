package io.chymyst.ch

import io.chymyst.ch.TermExpr.ProofTerm

object LJT {
  def followsFromAxioms[T](sequent: Sequent[T]): Seq[ProofTerm[T]] = {
    // The LJT calculus has three axioms. We use the Id axiom and the T axiom only, because the F axiom is not useful for code generation.

    val fromIdAxiom: Seq[TermExpr[T]] = sequent.premiseVars
      .zip(sequent.premises)
      .filter(_._2 == sequent.goal && sequent.goal.isAtomic)
      .map { case (premiseVar, _) ⇒
        // Generate a new term x1 ⇒ x2 ⇒ ... ⇒ xN ⇒ xK with fresh names. Here `xK` is one of the variables, selecting the premise that is equal to the goal.
        // At this iteration, we already selected the premise that is equal to the goal.
        sequent.constructResultTerm(premiseVar)
      }
    val fromTAxiom: Seq[TermExpr[T]] = sequent.goal match {
      case unitT: UnitT[T] ⇒ Seq(sequent.constructResultTerm(UnitE(unitT)))
      case _ ⇒ Seq()
    }
    fromIdAxiom ++ fromTAxiom
  }

  // G* |- A ⇒ B when (G*, A) |- B  -- rule ->R
  private def ruleImplicationAtRight[T] = ForwardRule[T](name = "->R", sequent ⇒
    sequent.goal match {
      case a :-> b ⇒
        val newSequent = sequent.copy(premises = a :: sequent.premises, goal = b)
        Some(RuleResult(Seq(newSequent), { proofTerms ⇒
          // This rule expects only one sub-proof term.
          val subProof = proofTerms.head
          // `subProof` is the proof of (G, A) |- B, and we need a proof of G |- A ⇒ B.
          // `subProof` is x ⇒ y ⇒ ... ⇒ z ⇒ a ⇒ <some term depending on (a, x, y, z)>
          // This proof will be exactly what we need if we reverse the order of curried arguments w.r.t. the list order of premises.
          subProof
        }))
      case _ ⇒ None
    })

  // (G*, X, X ⇒ A) |- B when (G*, X, A) |- B  -- rule ->L1
  // Note: there may be several ways of choosing X and X ⇒ A, which lead to equivalent derivations - if so, we have an ambiguous implementation, which is an error.
  // We can signal this error early since this rule is invertible.
  private def ruleImplicationAtLeft1[T] = ForwardRule[T](name = "->L1", sequent ⇒
    for {
      atomicPremise ← sequent.premises.find(_.isAtomic)
      implicationPremise ← sequent.premises.find{
        case head :-> body ⇒ head == atomicPremise
        case _ ⇒ false
      }
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
    sequent.goal match {
      case conjunctType: ConjunctT[T] ⇒ Some(RuleResult(conjunctType.terms.map(t ⇒ sequent.copy(goal = t)), { proofTerms ⇒
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
    sequent.goal match {
      case disjunctType: DisjunctT[T] ⇒
        val mainExpression = disjunctType.terms(indexInDisjunct)
        Some(RuleResult(List(sequent.copy(goal = mainExpression)), { proofTerms ⇒
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
    Some(RuleResult(List(sequent.copy(goal = ???)), { proofTerms ⇒
      // This rule expects two different proof terms.
      ???
    }
    )

    )
  )

  def nonInvertibleRules[T](sequent: Sequent[T]): Seq[ForwardRule[T]] = {
    // Generate all +Rn rules if the sequent has a disjunction goal.
    (sequent.goal match {
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
      case Some(RuleResult(newSequents, backTransform)) ⇒
        // All the new sequents need to be proved before we can continue. They may have several proofs each.
        val newProofs: Seq[Seq[ProofTerm[T]]] = newSequents.map(findProofTerms)
        val explodedNewProofs: Seq[Seq[ProofTerm[T]]] = ITP.explode(newProofs)
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
          .flatMap { case RuleResult(newSequents, backTransform) ⇒
            val newProofs: Seq[Seq[ProofTerm[T]]] = newSequents.map(findProofTerms)
            val explodedNewProofs: Seq[Seq[ProofTerm[T]]] = ITP.explode(newProofs)
            val finalNewProofs: Seq[ProofTerm[T]] = explodedNewProofs.map(backTransform)
            finalNewProofs
          }
        fromNoninvertibleRules ++ fromAxioms
    }
    fromRules
  }



}
