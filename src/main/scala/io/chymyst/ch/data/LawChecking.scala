package io.chymyst.ch.data

import io.chymyst.ch._

object LawChecking {

  def checkFlattenAssociativity(fmap: TermExpr, flatten: TermExpr): Boolean = {
    // fmap ftn . ftn = ftn . ftn
    val lhs = flatten :@@ flatten
    val rhs = (fmap :@ flatten) :@@ flatten
    lhs equiv rhs
  }

  def checkPureFlattenLaws(fmap: TermExpr, pure: TermExpr, flatten: TermExpr): Boolean = {
    // pure . ftn = id
    val pf = (pure :@@ flatten).simplify.prettyRename // pf: F[A] ⇒ F[A]
    val faType = pf.t.asInstanceOf[#->].head // This should fail if pf is not a function.
    val x = VarE("x", faType)
    val idFA = x =>: x

    // fmap pure . ftn = id
    val fpf = (fmap :@ pure) :@@ flatten
    (pf equiv idFA) && (fpf equiv idFA)
  }

}
