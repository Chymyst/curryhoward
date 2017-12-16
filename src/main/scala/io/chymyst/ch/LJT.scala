package io.chymyst.ch

import io.chymyst.ch.TermExpr.ProofTerm

/*
The calculus LJT as presented by Galmiche and Larchey-Wendling (1998).

Axioms:
-------

+ (G*, X) |- X  -- axiom Id -- here X is atomic, although the same rule would be valid for non-atomic X, we will not use it because in that way we avoid duplication of derivations.
+ G* |- 1  -- axiom T -- here 1 represents the Unit type, and the expression must be constructed somehow in the term.
- (G*, 0) |- A  -- axiom F -- we will not use it. Instead, we will treat `Nothing` as just another type parameter. (We will treat `Any` in this way, too.)

Invertible rules:
-----------------

(G*, A & B) |- C when (G*, A, B) |- C  -- rule &L
+ G* |- A & B when G* |- A and G* |- B  -- rule &R -- duplicates the context G*
(G*, A + B) |- C when (G*, A) |- C and (G*, B) |- C  -- rule +L -- duplicates the context G*
+ G* |- A ⇒ B when (G*, A) |- B  -- rule ->R
+ (G*, X, X ⇒ A) |- B when (G*, X, A) |- B  -- rule ->L1 -- here X is atomic, although the same rule would be valid for non-atomic X.
(G*, (A & B) ⇒ C) |- D when (G*, A ⇒ B ⇒ C) |- D  -- rule ->L2
(G*, (A + B) ⇒ C) |- D when (G*, A ⇒ C, B ⇒ C) |- D  - rule ->L3

Non-invertible rules:
---------------------

+ G* |- A + B when G* |- A  -- rule +R1
+ G* |- A + B when G* |- B  -- rule +R2
+ (G*, (A ⇒ B) ⇒ C) |- D when (G*, C) |- D and (G*, B ⇒ C) |- A ⇒ B  -- rule ->L4

 */

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

  // (G*, A & B) |- C when (G*, A, B) |- C  -- rule &L
  private def ruleConjunctionAtLeft[T] = ForwardRule[T](name = "&L", sequent ⇒
    Seq(RuleResult("&L", List(sequent.copy(goal = ???)), { proofTerms ⇒
      // This rule expects two different proof terms.
      ???
    }
    )

    )
  )

  // (G*, A + B) |- C when (G*, A) |- C and (G*, B) |- C  -- rule +L -- duplicates the context G*
  private def ruleDisjunctionAtLeft[T] = ForwardRule[T](name = "+L", sequent ⇒
    Seq(RuleResult("+L", List(sequent.copy(goal = ???)), { proofTerms ⇒
      // This rule expects two different proof terms.
      ???
    }
    )

    )
  )

  // (G*, (A & B) ⇒ C) |- D when (G*, A ⇒ B ⇒ C) |- D  -- rule ->L2
  private def ruleImplicationAtLeft2[T] = ForwardRule[T](name = "->L2", sequent ⇒
    Seq(RuleResult("->L2", List(sequent.copy(goal = ???)), { proofTerms ⇒
      // This rule expects two different proof terms.
      ???
    }
    )

    )
  )

  // (G*, (A + B) ⇒ C) |- D when (G*, A ⇒ C, B ⇒ C) |- D  - rule ->L3
  private def ruleImplicationAtLeft3[T] = ForwardRule[T](name = "->L3", sequent ⇒
    Seq(RuleResult("->L3", List(sequent.copy(goal = ???)), { proofTerms ⇒
      // This rule expects two different proof terms.
      ???
    }
    )

    )
  )

  // G* |- A ⇒ B when (G*, A) |- B  -- rule ->R
  private def ruleImplicationAtRight[T] = ForwardRule[T](name = "->R", sequent ⇒
    sequent.goal match {
      case a #-> b ⇒ // The new sequent is (G*, A) |- B
        val newSequent = sequent.copy(premises = a :: sequent.premises, goal = b)
        Seq(RuleResult("->R", Seq(newSequent), { proofTerms ⇒
          // This rule expects only one sub-proof term, and it must be a function.
          proofTerms.head match {
            // `proofTerms.head` is the proof of (G*, A) |- B, and we need a proof of G* |- A ⇒ B.
            // `proofTerms.head` must be of the form a ⇒ x ⇒ ... ⇒ y ⇒ ... ⇒ z ⇒ f(a, x, y, ..., z),
            // where f is some term depending on (a, x, y, ..., z), and the type B is Y ⇒ ... ⇒ Z ⇒ F.
            case CurriedE(heads, f) ⇒
              // We need to construct x ⇒ ... ⇒ a ⇒ y ⇒ ... ⇒ z ⇒ f instead.
              // Note that sequent.premises.length is the number of implications in x ⇒ ... before ⇒ a.
              val newHeads = heads.tail.take(sequent.premises.length) ++ Seq(heads.head) ++ heads.drop(sequent.premises.length + 1)
              CurriedE(newHeads, f)
          }
        }))
      case _ ⇒ Seq()
    })

  // (G*, X, X ⇒ A) |- B when (G*, X, A) |- B  -- rule ->L1
  // Note: there may be several ways of choosing X and X ⇒ A, which lead to equivalent derivations - if so, we have an ambiguous implementation, which is an error.
  // We can signal this error early since this rule is invertible.
  private def ruleImplicationAtLeft1[T] = ForwardRule[T](name = "->L1", { sequent ⇒
    val indexedPremises = sequent.premises.zipWithIndex
    for {
      atomicPremiseXi ← indexedPremises.filter(_._1.isAtomic)
      (atomicPremiseX, atomicPremiseI) = atomicPremiseXi
      // We only need to keep `body` and `ind` here.
      implPremiseAi ← indexedPremises.collect { case (head #-> body, ind) if head == atomicPremiseX ⇒ (body, ind) }
      (implPremiseA, implPremiseI) = implPremiseAi
    } yield {
      // Build the sequent (G*, X, A) |- B by excluding the premise X ⇒ A from the initial context, and by prepending A to it.
      // In other words, the new premises are (A, G* \ { X ⇒ A }).
      val newPremises = implPremiseA :: indexedPremises.filterNot(_._2 == implPremiseI).map(_._1)
      val newSequent = sequent.copy(premises = newPremises)
      RuleResult[T]("->L1", Seq(newSequent), { proofTerms ⇒
        // This rule expects only one sub-proof term.
        val proofTerm = proofTerms.head
        // This term is of the type A => G* \ { X ⇒ A } => B.
        // We need to build the term G* => B, knowing that X is in G* at index atomicPremiseI, and X ⇒ A at index implPremiseI.
        // The arguments of proofTerm is the list (A, P1, P2, ..., X, ... PN) of some variables. We need to use sequent.premiseVars instead, or else sequent.constructResultTerm won't work.
        // That is, we need to apply the term CurriedE(heads, f) to our sequent's premise vars, slightly reordered.
        val implPremiseVar = sequent.premiseVars(implPremiseI) // X ⇒ A

        val atomicPremiseVar = sequent.premiseVars(atomicPremiseI) // X

        val valueA = AppE(implPremiseVar, atomicPremiseVar)
        // The list of vars is (A, P1, P2, ..., X, ... PN)
        val oldPremisesWithoutImplPremiseA = sequent.premiseVars.zipWithIndex.filterNot(_._2 == implPremiseI).map(_._1)
        val result = TermExpr.applyToVars(proofTerm, valueA :: oldPremisesWithoutImplPremiseA)
        sequent.constructResultTerm(result)
      })
    }
  }
  )

  def invertibleRules[T]: Seq[ForwardRule[T]] = Seq(
    ruleImplicationAtRight,
    ruleConjunctionAtRight // Put this later in the sequence because it duplicates the context G*.
  )

  def invertibleAmbiguousRules[T]: Seq[ForwardRule[T]] = Seq(ruleImplicationAtLeft1)

  // G* |- A & B when G* |- A and G* |- B  -- rule &R -- duplicates the context G*
  private def ruleConjunctionAtRight[T] = ForwardRule[T](name = "&R", sequent ⇒
    sequent.goal match {
      case conjunctType: ConjunctT[T] ⇒ Seq(RuleResult("&R", conjunctType.terms.map(t ⇒ sequent.copy(goal = t)), { proofTerms ⇒
        // This rule takes any number of proof terms.
        sequent.constructResultTerm(ConjunctE(proofTerms.map(sequent.substitute)))
      })
      )
      case _ ⇒ Seq()
    }
  )

  // G* |- A + B when G* |- A  -- rule +R1
  // Generate all such rules for any disjunct.
  private def ruleDisjunctionAtRight[T](indexInDisjunct: Int) = ForwardRule[T](name = "+R1", sequent ⇒
    sequent.goal match {
      case disjunctType: DisjunctT[T] ⇒
        val mainExpression = disjunctType.terms(indexInDisjunct)
        Seq(RuleResult(s"+R$indexInDisjunct", List(sequent.copy(goal = mainExpression)), { proofTerms ⇒
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
      case _ ⇒ Seq()
    }
  )

  // (G*, (A ⇒ B) ⇒ C) |- D when (G*, C) |- D and (G*, B ⇒ C) |- A ⇒ B  -- rule ->L4
  // This rule needs a lemma:  |-  ((A ⇒ B) ⇒ C) ⇒ B ⇒ C
  private def ruleImplicationAtLeft4[T] = ForwardRule[T](name = "->L4", { sequent ⇒
    val indexedPremises = sequent.premises.zipWithIndex
    for {
      premiseABCi ← indexedPremises.collect { case ((headA #-> headB) #-> bodyC, ind) ⇒ (headA, headB, bodyC, ind) }
    } yield {
      val (a, b, c, i) = premiseABCi
      val newPremisesCD = c :: indexedPremises.filterNot(_._2 == i).map(_._1)
      val newPremisesBCAB = (b ->: c) :: indexedPremises.filterNot(_._2 == i).map(_._1)
      RuleResult[T]("->L4", List(sequent.copy(premises = newPremisesBCAB, goal = a ->: b), sequent.copy(premises = newPremisesCD)), { proofTerms ⇒
        // This rule expects two different proof terms.
        val Seq(termCD, termBCAB) = proofTerms
        val CurriedE(termCDheads, termCDbody) = termCD
        val CurriedE(termBCABheads, termBCABbody) = termCD
        val implPremiseVarABC = sequent.premiseVars(i) // (A ⇒ B) ⇒ C

        val oldPremisesWithoutImplPremise = sequent.premiseVars.zipWithIndex.filterNot(_._2 == i).map(_._1)

        // Using the lemma |-  ((A ⇒ B) ⇒ C) ⇒ B ⇒ C, construct the proof of the original sequent.
        val varB = PropE(sequent.freshVar(), b)
        val varDummyA = PropE(sequent.freshVar(), a)
        val valueConstAB = CurriedE(List(varDummyA), varB)
        val valueBC = CurriedE(List(varB), AppE(implPremiseVarABC, valueConstAB))
        val valueAB = TermExpr.applyToVars(termBCAB, valueBC :: oldPremisesWithoutImplPremise)
        val valueC = AppE(implPremiseVarABC, valueAB)
        val result = TermExpr.applyToVars(termCD, valueC :: oldPremisesWithoutImplPremise)
        sequent.constructResultTerm(result)
      }
      )
    }
  }
  )

  def nonInvertibleRulesForSequent[T](sequent: Sequent[T]): Seq[ForwardRule[T]] = {
    // Generate all +Rn rules if the sequent has a disjunction goal.
    (sequent.goal match {
      case DisjunctT(terms) ⇒ terms.indices.map(ruleDisjunctionAtRight[T])
      case _ ⇒ Seq[ForwardRule[T]]()
    }) ++ Seq(
          ruleImplicationAtLeft4[T]
    )
  }

}