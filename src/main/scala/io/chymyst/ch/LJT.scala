package io.chymyst.ch

/*
The calculus LJT as presented by Galmiche and Larchey-Wendling (1998).

Axioms:
-------

+ (G*, X) |- X  -- axiom Id -- here X is atomic, although the same rule would be valid for non-atomic X, we do not allow that because in that way we avoid needless duplication of derivations.
+ G* |- 1  -- axiom T -- here 1 represents the Unit type (possibly a named Unit).
- (G*, 0) |- A  -- axiom F -- we will not use it. Instead, we will treat `Nothing` as just another type parameter. (We will treat `Any` in this way, too.)

Invertible unambiguous rules:
-----------------------------

Unambiguous rules either can be applied to premises in only one way, or they cannot be applied.
Therefore, applying these rules in any order will not yield alternative proof terms.

Rules that do not change the goal:

+ (G*, A & B) |- C when (G*, A, B) |- C  -- rule &L. Deletes premise, adds two premises.
+ (G*, (A & B) ⇒ C) |- D when (G*, A ⇒ B ⇒ C) |- D  -- rule ->L2. Deletes premise, adds premise.
+ (G*, (A + B) ⇒ C) |- D when (G*, A ⇒ C, B ⇒ C) |- D  - rule ->L3. Deletes premise, adds two premises.

+ (G*, A + B) |- C when (G*, A) |- C and (G*, B) |- C  -- rule +L. Duplicates the context G*. Generates two new sequents. Deletes premise, adds premise in each new sequent.

Rules that change the goal:

+ G* |- A ⇒ B when (G*, A) |- B  -- rule ->R. Replaces goal, adds premise.

+ G* |- A & B when G* |- A and G* |- B  -- rule &R. Duplicates the context G*. Replaces goal, generates two new sequents.

Invertible ambiguous rules:
---------------------------

Ambiguous rules can be applied to the premises in several different ways, possibly generating alternative proof terms.

+ (G*, X, X ⇒ A) |- B when (G*, X, A) |- B  -- rule ->L1 -- here X is atomic, although the same rule would be valid for non-atomic X.

Non-invertible rules:
---------------------

+ G* |- A + B when G* |- A  -- rule +R1
+ G* |- A + B when G* |- B  -- rule +R2
+ (G*, (A ⇒ B) ⇒ C) |- D when (G*, C) |- D and (G*, B ⇒ C) |- A ⇒ B  -- rule ->L4

Additional rules for named conjunctions:

+ G* |- Named(A, B) when G* |- (A & B)  -- rule _&R
+ G*, Named(A, B) |- C when G*, (A & B) |- C  -- rule _&L
+ G*, Named(A, B) ⇒ C |- D when G*, (A & B) ⇒ C |- D  -- rule _->L2
 */

object LJT {

  def invertibleRules: Seq[ForwardRule] = Seq(
    ruleImplicationAtRight
    , ruleNamedConjunctionAtLeft
    , ruleConjunctionAtLeft
    , ruleNamedConjunctionAtRight // Conjunction at right duplicates the context but we have inconsistent results if named conjunctions are treated differently from non-named ones.
    , ruleConjunctionAtRight
    , ruleImplicationAtLeft2
    , ruleImplicationAtLeft3
    , ruleImplicationWithNamedConjunctionAtLeft
    // The following rules are tried later because they duplicate the context G*.
    , ruleDisjunctionAtLeft
  )

  def invertibleAmbiguousRules: Seq[ForwardRule] = Seq() // Seq(ruleImplicationAtLeft1)

  def nonInvertibleRulesForSequent(sequent: Sequent): Seq[ForwardRule] = {
    // Generate all +Rn rules if the sequent has a disjunction goal.
    (sequent.goal match {
      case DisjunctT(_, _, terms) ⇒ terms.indices.map(ruleDisjunctionAtRight)
      case _ ⇒ Seq[ForwardRule]()
    }) ++ Seq(ruleImplicationAtLeft1, ruleImplicationAtLeft4) // This is the same for all sequents.
  }

  private def omitPremise[C](indexedPremises: Seq[(C, Int)], index: Int): List[C] = indexedPremises.filterNot(_._2 == index).map(_._1).toList

  private[ch] def followsFromAxioms(sequent: Sequent): (Seq[TermExpr], Seq[TermExpr]) = {
    // The LJT calculus has three axioms. We use the Id axiom and the T axiom only, because the F axiom is not useful for code generation.
    // We return the proof terms separately for each axiom.
    val fromIdAxiom: Seq[TermExpr] = sequent.premiseVars
      .zip(sequent.premises)
      // Find premises that are equal to the goal.
      .filter(_._2 == sequent.goal) // && sequent.goal.isAtomic) // There is no harm in applying this rule also to non-atomic terms. No useful alternative proofs would be lost due to that.
      .map { case (premiseVar, _) ⇒
      // Generate a new term x1 ⇒ x2 ⇒ ... ⇒ xN ⇒ xK with fresh names. Here `xK` is one of the variables, selecting the premise that is equal to the goal.
      // At this iteration, we already selected the premise that is equal to the goal.
      sequent.constructResultTerm(premiseVar)
    }

    val fromTAxiom: Seq[TermExpr] = sequent.goal match {
      case unitT: UnitT ⇒ Seq(sequent.constructResultTerm(UnitE(unitT)))
      case _ ⇒ Seq()
    }
    (fromIdAxiom, fromTAxiom)
  }

  // (G*, A + B) |- C when (G*, A) |- C and (G*, B) |- C  -- rule +L -- duplicates the context G*
  // This rule does not need to be multiplexed. It can return a single RuleResult if it is applicable.
  private def ruleDisjunctionAtLeft = ForwardRule(name = "+L", { sequent ⇒
    val indexedPremises = sequent.premises.zipWithIndex
    indexedPremises.collectFirst { case (DisjunctT(_, _, heads), index) ⇒ (heads, index) } match {
      case Some((heads, i)) ⇒
        val premisesWithoutAB = omitPremise(indexedPremises, i)
        val newSequents = heads.map { h ⇒ sequent.copy(premises = h :: premisesWithoutAB) }
        Seq(RuleResult("+L", newSequents, { TermExprs ⇒
          // This rule expects several different proof terms.
          val thePremiseVarAB = sequent.premiseVars(i) // of type A+B

          val oldPremisesWithoutI: List[VarE] = omitPremise(sequent.premiseVars.zipWithIndex, i)
          val freshVarsAB = heads.map(VarE(sequent.freshVar(), _)).toList
          // Each part of the disjunction is matched with a function of the form fv ⇒ TermExpr(fv, other_premises).
          val subTerms = TermExprs.zip(freshVarsAB)
            .map { case (pt, fv) ⇒ (fv.t, CurriedE(List(fv), TermExpr.applyCurried(pt, fv :: oldPremisesWithoutI))) }
            .sortBy(_._1.prettyPrint).map(_._2) // Sort the clauses by type expression.
        // At this point, `subTerms` can be reordered at will since these are mutually exclusive and exhaustive cases in a disjunction.
        val result = MatchE(thePremiseVarAB, subTerms.toList)
          sequent.constructResultTerm(result)
        }
        )
        )
      case None ⇒ Seq()
    }
  }
  )

  final case class UniRuleLogic(additionalPremises: Seq[TypeExpr], produceAdditionalTerms: (Sequent, VarE) ⇒ List[TermExpr])

  // Uniform rules: delete one premise, add one or more new premises, do not change the goal, generate only one new sequent.

  // Helper method for constructing uniform rules.
  // Those rules are of the form  (G*, P) |- C when (G*, P1, P2, ..., Pn) |- C
  // Those rules do not need to be multiplexed. They return a single RuleResult if applicable.
  private def uniformRule(ruleName: String)(ruleLogic: PartialFunction[TypeExpr, UniRuleLogic]) =
    ForwardRule(ruleName, { sequent ⇒
      val indexedPremises = sequent.premises.zipWithIndex
      indexedPremises.collectFirst { case (p, i) if ruleLogic.isDefinedAt(p) ⇒ (ruleLogic(p), i) } match {
        case Some((UniRuleLogic(additionalPremises, produceAdditionalTerms), i)) ⇒
          val newPremises: List[TypeExpr] = additionalPremises.toList ++ omitPremise(indexedPremises, i)
          Seq(RuleResult(ruleName, List(sequent.copy(premises = newPremises)), { proofTerms ⇒
            val proofTerm = proofTerms.head // This rule expects one proof term for the sequent (G*, P1, P2, ..., Pn) |- C.

            val thePremiseVar = sequent.premiseVars(i) // The premise variable P.

            val additionalValues: List[TermExpr] = produceAdditionalTerms(sequent, thePremiseVar) // The terms P1, P2, ..., Pn.

            val oldPremisesWithoutI: List[VarE] = omitPremise(sequent.premiseVars.zipWithIndex, i)
            val result = TermExpr.applyCurried(proofTerm, additionalValues ++ oldPremisesWithoutI)
            sequent.constructResultTerm(result) // The proof term for the sequent (G*, P1, P2, ..., Pn) |- C.
          }
          )
          )

        case None ⇒ Seq()
      }
    }
    )

  // (G*, A & B) |- C when (G*, A, B) |- C  -- rule &L
  private def ruleConjunctionAtLeft = uniformRule("&L") {
    case ConjunctT(heads) ⇒ UniRuleLogic(heads, (sequent, premiseVar) ⇒ heads.indices.map(ProjectE(_, premiseVar)).toList)
  }

  // (G*, (A & B) ⇒ C) |- D when (G*, A ⇒ B ⇒ C) |- D  -- rule ->L2
  private def ruleImplicationAtLeft2 = uniformRule("->L2") {
    case ConjunctT(heads) #-> argC ⇒ UniRuleLogic(Seq(heads.reverse.foldLeft(argC) { case (prev, h) ⇒ h ->: prev }), { (sequent, premiseVar) ⇒
      val freshVarsAB = heads.map(VarE(sequent.freshVar(), _)).toList
      val func_A_B_to_C = CurriedE(freshVarsAB, AppE(premiseVar, ConjunctE(freshVarsAB)))
      List(func_A_B_to_C)
    })
  }

  // (G*, (A + B) ⇒ C) |- D when (G*, A ⇒ C, B ⇒ C) |- D  - rule ->L3
  private def ruleImplicationAtLeft3 = uniformRule("->L3") {
    case (disjunctT@DisjunctT(_, _, heads)) #-> argC ⇒ UniRuleLogic(heads.map(_ ->: argC), { (sequent, premiseVar) ⇒
      val freshVarsAB = heads.map(VarE(sequent.freshVar(), _))
      val functionsAC_BC = freshVarsAB.zipWithIndex.map { case (fv, ind) ⇒
        CurriedE(List(fv), AppE(premiseVar, DisjunctE(ind, heads.length, fv, disjunctT)))
      }
      functionsAC_BC.toList
    })
  }

  // G* |- A ⇒ B when (G*, A) |- B  -- rule ->R
  private def ruleImplicationAtRight = ForwardRule(name = "->R", sequent ⇒
    sequent.goal match {
      case a #-> b ⇒ // The new sequent is (G*, A) |- B
        val newSequent = sequent.copy(premises = a :: sequent.premises, goal = b)
        Seq(RuleResult("->R", Seq(newSequent), { TermExprs ⇒
          // This rule expects only one sub-proof term, and it must be a function.
          TermExprs.head match {
            // `TermExprs.head` is the proof of (G*, A) |- B, and we need a proof of G* |- A ⇒ B.
            // `TermExprs.head` must be of the form a ⇒ x ⇒ ... ⇒ y ⇒ ... ⇒ z ⇒ f(a, x, y, ..., z),
            // where f is some term depending on (a, x, y, ..., z), and the type B is Y ⇒ ... ⇒ Z ⇒ F.
            case CurriedE(args, f) ⇒
              // We need to construct x ⇒ ... ⇒ a ⇒ y ⇒ ... ⇒ z ⇒ f instead.
              // Note that sequent.premises.length is the number of implications in x ⇒ ... before ⇒ a.
              val newHeads = args.drop(1).take(sequent.premises.length) ++ Seq(args.head) ++ args.drop(sequent.premises.length + 1)
              CurriedE(newHeads, f)
            case _ ⇒ throw new Exception(s"Internal error: proof term $TermExprs must be a function") // This case is never reached.
          }
        }))
      case _ ⇒ Seq()
    })

  // (G*, X, X ⇒ A) |- B when (G*, X, A) |- B  -- rule ->L1
  // Note: there may be several ways of choosing X and X ⇒ A having the same types,
  // which lead to equivalent derivations - if so, we have an ambiguous implementation, which is an error.
  // We can signal this error early since this rule is invertible.
  private def ruleImplicationAtLeft1 = ForwardRule(name = "->L1", { sequent ⇒
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
      val newPremises = implPremiseA :: omitPremise(indexedPremises, implPremiseI)
      val newSequent = sequent.copy(premises = newPremises)
      RuleResult("->L1", Seq(newSequent), { proofTerms ⇒
        // This rule expects only one sub-proof term.
        val proofTerm = proofTerms.head
        // This term is of the type A => G* \ { X ⇒ A } => B.
        // We need to build the term G* => B, knowing that X is in G* at index atomicPremiseI, and X ⇒ A at index implPremiseI.
        // The arguments of TermExpr is the list (A, P1, P2, ..., X, ... PN) of some variables. We need to use sequent.premiseVars instead, or else sequent.constructResultTerm won't work.
        // That is, we need to apply the term CurriedE(heads, f) to our sequent's premise vars, slightly reordered.
        val implPremiseVar = sequent.premiseVars(implPremiseI) // X ⇒ A

        val atomicPremiseVar = sequent.premiseVars(atomicPremiseI) // X

        val valueA = AppE(implPremiseVar, atomicPremiseVar)
        // The list of vars is (A, P1, P2, ..., X, ... PN)
        val oldPremisesWithoutImplPremiseA = omitPremise(sequent.premiseVars.zipWithIndex, implPremiseI)
        val result = TermExpr.applyCurried(proofTerm, valueA :: oldPremisesWithoutImplPremiseA)
        sequent.constructResultTerm(result)
      })
    }
  }
  )

  // G* |- A & B when G* |- A and G* |- B  -- rule &R -- duplicates the context G*
  private def ruleConjunctionAtRight = ForwardRule(name = "&R", sequent ⇒
    sequent.goal match {
      case conjunctType: ConjunctT ⇒ Seq(RuleResult("&R", conjunctType.terms.map(t ⇒ sequent.copy(goal = t)), { TermExprs ⇒
        // This rule takes any number of proof terms.
        sequent.constructResultTerm(ConjunctE(TermExprs.map(sequent.substituteInto)))
      })
      )
      case _ ⇒ Seq()
    }
  )

  // G* |- Named(A, B) when G* |- (A & B)  -- rule _&R
  private def ruleNamedConjunctionAtRight = ForwardRule(name = "_&R", sequent ⇒
    sequent.goal match {
      case nct@NamedConjunctT(constructor, _, accessors, wrapped)
        if accessors.nonEmpty || wrapped.isEmpty ⇒ // Avoid applying rule _&R to a named unit that is not a case object. e.g. NamedConjunctT("name", Nil, Nil, List(UnitT("...")))
        val unwrapped = wrapped match {
          case Nil ⇒ // empty wrapper means a named Unit as a case object
            UnitT(constructor)
          case _ ⇒ ConjunctT(wrapped)
        }
        Seq(RuleResult("_&R", Seq(sequent.copy(goal = unwrapped)), { TermExprs ⇒
          // This rule takes one proof term.
          val TermExpr = TermExprs.head
          val resultTerms: Seq[TermExpr] = sequent.substituteInto(TermExpr) match {
            // Wrapped Unit or wrapped single term.
            case _ if nct.caseObjectName.isDefined ⇒ Nil
            //            case term if nct.accessors.length == 1 ⇒ Seq(term) // This breaks several things, since we are not creating a ProjectE().
            // Wrapped conjunction having at least one part.
            // The term will eventually evaluate to a conjunction.
            case term ⇒ nct.accessors.indices.map { i ⇒ ProjectE(i, term) }
          }
          sequent.constructResultTerm(NamedConjunctE(resultTerms, nct))
        })
        )
      case _ ⇒ Seq()
    }
  )

  // G*, Named(A, B) |- C when G*, A, B |- C  -- rule _&L
  private def ruleNamedConjunctionAtLeft = uniformRule("_&L") {
    case NamedConjunctT(constructor, _, accessors, wrapped)
      if accessors.nonEmpty || wrapped.isEmpty ⇒ // Avoid applying rule _&L to a named unit that is not a case object. e.g. NamedConjunctT("name", Nil, Nil, List(UnitT("...")))
      val unwrapped = wrapped match {
        case Nil ⇒ // empty wrapper means a named Unit as a case object
          List(UnitT(constructor))
        case _ ⇒ wrapped
      }
      UniRuleLogic(unwrapped, { (sequent, premiseVar) ⇒
        // Need to produce the terms termsAB : A, B, ..., given premiseVar : Named(A, B).
        val termsAB: List[TermExpr] = wrapped match {
          case Nil ⇒ // empty wrapper means a named Unit as a case object
            List(UnitE(UnitT(constructor)))
          case _ ⇒ // wrapper is not empty, so some terms are present
            accessors.indices.map(ProjectE(_, premiseVar)).toList
        }
        termsAB
      })
  }

  // G*, Named(A, B) ⇒ C |- D when G*, (A & B) ⇒ C |- D  -- rule _->L2
  private def ruleImplicationWithNamedConjunctionAtLeft = uniformRule("_->L2") {
    case (nct@NamedConjunctT(constructor, _, accessors, wrapped)) #-> argC ⇒
      val unwrapped = ConjunctT(wrapped match {
        case Nil ⇒ // empty wrapper means a named Unit as a case object
          List(UnitT(constructor))
        case _ ⇒ wrapped
      })
      UniRuleLogic(Seq(unwrapped ->: argC), { (sequent, premiseVar) ⇒
        val fv = VarE(sequent.freshVar(), unwrapped)
        // Need to produce a term termAB_C : (A & B) ⇒ C, given premiseVar : Named(A, B) ⇒ C.
        // Use the free variable fv : A & B. We first construct namedAB : Named(A, B) using fv, and then apply premiseVar to it.
        // The resulting term is fv ⇒ premiseVar namedAB
        val namedAB: TermExpr = wrapped match {
          case Nil ⇒ NamedConjunctE(Seq(fv), nct)
          case _ ⇒ NamedConjunctE(accessors.indices.map(ProjectE(_, fv)), nct)
        }
        val termAB_C = CurriedE(List(fv), AppE(premiseVar, namedAB))
        List(termAB_C)
      })
  }

  // G* |- A + B when G* |- A  -- rule +R1
  // Generate all such rules for any disjunct.
  private def ruleDisjunctionAtRight(indexInDisjunct: Int) = ForwardRule(name = s"+R$indexInDisjunct", sequent ⇒
    sequent.goal match {
      case disjunctType: DisjunctT ⇒
        val mainExpression = disjunctType.terms(indexInDisjunct)
        Seq(RuleResult(s"+R$indexInDisjunct", List(sequent.copy(goal = mainExpression)), { TermExprs ⇒
          // This rule expects a single proof term.
          val TermExpr = TermExprs.head
          TermExpr match {
            case CurriedE(heads, body) ⇒ // The goal had some premises.
              CurriedE(heads, DisjunctE(indexInDisjunct, disjunctType.terms.length, body, disjunctType))
            case _ ⇒ // The goal has no premises.
              DisjunctE(0, disjunctType.terms.length, TermExpr, disjunctType)
          }
        })
        )
      case _ ⇒ Seq() // This case is never reached.
    }
  )

  // (G*, (A ⇒ B) ⇒ C) |- D when (G*, C) |- D and (G*, B ⇒ C) |- A ⇒ B  -- rule ->L4
  // This rule needs a lemma:  |-  ((A ⇒ B) ⇒ C) ⇒ B ⇒ C
  private def ruleImplicationAtLeft4 = ForwardRule(name = "->L4", { sequent ⇒
    val indexedPremises = sequent.premises.zipWithIndex
    for {
      premiseABCi ← indexedPremises.collect { case ((headA #-> headB) #-> bodyC, ind) ⇒ (headA, headB, bodyC, ind) }
    } yield {
      val (a, b, c, i) = premiseABCi
      val premisesWithoutI = indexedPremises.filterNot(_._2 == i).map(_._1)
      val newPremisesCD = c :: premisesWithoutI
      val newPremisesBCAB = (b ->: c) :: premisesWithoutI
      RuleResult("->L4", List(sequent.copy(premises = newPremisesBCAB, goal = a ->: b), sequent.copy(premises = newPremisesCD)), { TermExprs ⇒
        // This rule expects two different proof terms.
        val Seq(termBCAB, termCD) = TermExprs

        val implPremiseVarABC = sequent.premiseVars(i) // (A ⇒ B) ⇒ C

        val oldPremisesWithoutImplPremise = omitPremise(sequent.premiseVars.zipWithIndex, i)

        // Using the lemma |-  ((A ⇒ B) ⇒ C) ⇒ B ⇒ C, construct the proof of the original sequent.
        val varB = VarE(sequent.freshVar(), b)
        val varDummyA = VarE(sequent.freshVar(), a)
        val valueConstAB = CurriedE(List(varDummyA), varB)
        val valueBC = CurriedE(List(varB), AppE(implPremiseVarABC, valueConstAB))
        val valueAB = TermExpr.applyCurried(termBCAB, valueBC :: oldPremisesWithoutImplPremise)
        val valueC = AppE(implPremiseVarABC, valueAB)
        val result = TermExpr.applyCurried(termCD, valueC :: oldPremisesWithoutImplPremise)
        sequent.constructResultTerm(result)
      }
      )
    }
  }
  )

}
