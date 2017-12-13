package io.chymyst.ch

import io.chymyst.ch.TermExpr.ProofTerm

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox

// TODO:
/*  Priority is given in parentheses.

- support named conjunctions (case classes) explicitly (1) and support disjunctions on that basis
- implement Option and Either in inhabited terms (2)
- reverse the direction of CurriedE because we want to reuse argument lists more (0)
- implement all rules of the LJT calculus (1)
- check unused arguments and sort results accordingly (3)
- only output the results with smallest number of unused arguments (3)
- implement uncurried functions (6)
- make sure Unit works (2)
- support natural syntax def f[T](x: T): T = implement (3)
- use c.Type instead of String (3)
- use blackbox macros instead of whitebox if possible (5)
- use a special subclass of Function1 that also carries symbolic information about the lambda-term (6)
- add more error messages: print alternative lambda-terms when we refuse to implement (5)
- use a symbolic evaluator to simplify the lambda-terms (5)
- support sealed traits / case classes (5)

 Release as a separate open-source project after (1)-(4) are done.
 */

object CurryHowardMacros {

  private[ch] def testType[T]: (String, String) = macro testTypeImpl[T]

  private[ch] val basicTypes = List("Int", "String", "Boolean", "Float", "Double", "Long", "Symbol", "Char")

  private val basicRegex = s"(?:scala.|java.lang.)*(${basicTypes.mkString("|")})".r

  // TODO: use c.Type instead of String
  def matchType(c: whitebox.Context)(t: c.Type): TypeExpr[String] = {
    // Could be the weird type [X, Y] => (type expression), or it could be an actual tuple type.
    // `finalResultType` seems to help here.
    val args = t.finalResultType.typeArgs
    val typeParams = t.typeParams // This is nonempty only for the weird types mentioned above.

    t.typeSymbol.fullName match {
      case name if name matches "scala.Tuple[0-9]+" ⇒ ConjunctT(args.map(matchType(c))) //s"(${args.map(matchType(c)).mkString(", ")})"
      case "scala.Function1" ⇒ :->(matchType(c)(args.head), matchType(c)(args(1))) // s"${matchType(c)(args(0))} ..=>.. ${matchType(c)(args(1))}"
      case "scala.Option" ⇒ DisjunctT(Seq(UnitT("Unit"), matchType(c)(args.head))) //s"(1 + ${matchType(c)(args.head)})"
      case "scala.util.Either" ⇒ DisjunctT(Seq(matchType(c)(args.head), matchType(c)(args(1)))) //s"(${matchType(c)(args(0))} + ${matchType(c)(args(1))})"
      case "scala.Any" ⇒ OtherT("_")
      case "scala.Nothing" ⇒ NothingT("Nothing")
      case "scala.Unit" ⇒ UnitT("Unit")
      case basicRegex(name) ⇒ BasicT(name)
      case _ if args.isEmpty && t.baseClasses.map(_.fullName) == Seq("scala.Any") ⇒ TP(t.toString)
      case _ if args.isEmpty ⇒ OtherT(t.toString)
      case _ ⇒ ConstructorT(t.toString)
    }
  }

  def reifyParam(c: whitebox.Context)(term: PropE[String]): c.Tree = {
    import c.universe._
    term match {
      case PropE(name, tExpr) ⇒
        val tpt = tExpr match {
          case _: NothingT[String] ⇒ tq""
          // TODO: Stop using String as type parameter T, use c.Type instead
          case TP(nameT) ⇒
            val tpn = TypeName(nameT)
            tq"$tpn"
        }
        val termName = TermName("t_" + name)
        val param = q"val $termName: $tpt"
        param
    }
  }

  def reifyTerms(c: whitebox.Context)(termExpr: TermExpr[String], paramTerms: Map[PropE[String], c.Tree]): c.Tree = {
    import c.universe._

    termExpr match {
      case p@PropE(name, typeName) =>
        val tn = TermName("t_" + name)
        q"$tn"
      case AppE(head, arg) => q"${reifyTerms(c)(head, paramTerms)}(${reifyTerms(c)(arg, paramTerms)})"
      case CurriedE(heads, body) ⇒ heads.foldLeft(reifyTerms(c)(body, paramTerms)) { case (prevTree, paramE) ⇒
        val param = paramTerms(paramE)
        q"($param ⇒ $prevTree)"
      }
      case UnitE(_) => q"()"
      case ConjunctE(terms) ⇒ q"(..${terms.map(t ⇒ reifyTerms(c)(t, paramTerms))})"
      case DisjunctE(index, total, term, tExpr) ⇒
        ???
    }
  }

  def testTypeImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[(String, String)] = {
    import c.universe._

    val typeT: c.Type = c.weakTypeOf[T]
    val enclosingType = c.internal.enclosingOwner.typeSignature

    val s1 = matchType(c)(typeT.resultType).toString
    val s2 = matchType(c)(enclosingType).toString

    c.Expr[(String, String)](q"($s1,$s2)")
  }

  def ofType[T]: T = macro ofTypeImpl[T]

  def implement[T]: T = macro inhabitImpl[T]

  // TODO: can we replace this with blackbox? Probably, as long as `def f3[X, Y]: X ⇒ Y ⇒ X = ofType` does not work with whitebox anyway.
  def ofTypeImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    val typeT: c.Type = c.weakTypeOf[T]
    inhabitInternal(c)(typeT)
  }

  def inhabitImpl[T](c: whitebox.Context): c.Tree = {
    val typeT = c.internal.enclosingOwner.typeSignature
    inhabitInternal(c)(typeT)
  }

  def inhabitInternal(c: whitebox.Context)(typeT: c.Type): c.Tree = {
    import c.universe._
    val typeStructure: TypeExpr[String] = matchType(c)(typeT)
    val termFound: TermExpr[String] = ITP(typeStructure) match {
      case Nil ⇒
        c.error(c.enclosingPosition, s"type $typeStructure cannot be inhabited")
        null
      case List(term) ⇒ term
      case list ⇒
        c.error(c.enclosingPosition, s"type $typeStructure can be inhabited in ${list.length} different ways")
        null
    }

    println(s"DEBUG: Term found: $termFound, propositions: ${TermExpr.propositions(termFound)}")
    val paramTerms: Map[PropE[String], c.Tree] = TermExpr.propositions(termFound).toSeq.map(p ⇒ p → reifyParam(c)(p)).toMap
    val result = reifyTerms(c)(termFound, paramTerms)
    println(s"DEBUG: returning code: ${showCode(result)}")
    result
  }
}

object ITP {
  def explode[T](src: Seq[Seq[T]]): Seq[Seq[T]] = {
    src.foldLeft[Seq[Seq[T]]](Seq(Seq())) { case (prevSeqSeq, newSeq) ⇒
      for {
        prev ← prevSeqSeq
        next ← newSeq
      } yield prev :+ next
    }
  }

  def apply(typeStructure: TypeExpr[String]): List[TermExpr[String]] = findProofs(typeStructure)

  private[ch] val freshVar = new FreshIdents(prefix = "x")

  def findProofs[T](typeStructure: TypeExpr[T]): List[TermExpr[T]] = {
    val mainSequent = Sequent[T](List(), typeStructure, freshVar)
    val proofs: Seq[ProofTerm[T]] = LJT.findProofTerms(mainSequent)
    proofs.toList
  }
}
