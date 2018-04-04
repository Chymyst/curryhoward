package io.chymyst.ch.data

import io.chymyst.ch._

object SymbolicLawChecking {

  def checkFlattenAssociativity(fmap: TermExpr, flatten: TermExpr): Boolean = {
    // fmap ftn . ftn = ftn . ftn
    val lhs = flatten :@@ flatten
    val rhs = (fmap :@ flatten) :@@ flatten
    //        println(s"check associativity laws for flatten = ${flatten.prettyPrint}:\n\tlhs = ${lhs.simplify.prettyRenamePrint}\n\trhs = ${rhs.simplify.prettyRenamePrint}")
    TermExpr.extEquals(lhs, rhs)
  }

  def checkPureFlattenLaws(fmap: TermExpr, pure: TermExpr, flatten: TermExpr): Boolean = {
    // pure . ftn = id
    val pf = (pure :@@ flatten).simplify.prettyRename // pf: F[A] ⇒ F[A]
    val faType = pf.t.asInstanceOf[#->].head // This should fail if pf is not a function.
    val x = VarE("x", faType)
    val idFA = x =>: x

    // fmap pure . ftn = id
    val fpf = (fmap :@ pure) :@@ flatten

    //    println(s"check identity laws for pure = ${pure.prettyPrint} and flatten = ${flatten.prettyPrint}:\n\tlhs1 = ${pf.simplify.prettyPrint}\n\trhs1 = ${idFA.simplify.prettyPrint}\n\tlhs2 = ${fpf.simplify.prettyPrint}\n\trhs2 = ${idFA.simplify.prettyPrint}")
    TermExpr.extEquals(pf, idFA) && TermExpr.extEquals(fpf, idFA)
  }

}
