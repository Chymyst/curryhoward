package io.chymyst.ch

import scala.collection.immutable
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox

/* http://stackoverflow.com/questions/17494010/debug-scala-macros-with-intellij

How to debug Scala macros:

You need to start your sbt in debug mode and then connect the idea debugger remotely:

start sbt with: sbt -jvm-debug 5005

create a "Remote" "Run/Debug Config" in idea (defaults to port 5005 )

Run the remote­debug config in idea. That will connect it to your running sbt. Then you can set breakpoints in your macro code and when running compile in sbt, idea should stop at the breakpoint.

note: to re­run compile after a successful compile you need either to clean or change some code
 */

/*
Done:
+ finish the implicational fragment (0)
+ implement all rules of the LJT calculus (1)
+ make sure Unit works (2)
+ better output of parentheses for type expressions
+ check unused arguments and sort results accordingly (3)
+ only output the results with smallest number of unused arguments, if that is unique (3)
+ add more error messages: print alternative lambda-terms when we refuse to implement (5)
+ use a symbolic evaluator to simplify the lambda-terms (5)

 */

// TODO:
/*  Priority is given in parentheses.
- implement Option and Either in inhabited terms (2)
- support named conjunctions (case classes) explicitly (3) and support disjunctions on that basis
- support sealed traits / case classes (5)
- add documentation using the `tut` plugin (3)
- support natural syntax def f[T](x: T): T = implement (3)
- use c.Type instead of String for correct code generation (3)?? Probably impossible since we can't reify types directly from reflection results, - need to use names.
- probably can simplify data structures by eliminating [T]
- use blackbox macros instead of whitebox if possible (5)
- implement uncurried functions and multiple argument lists (6)
- use a special subclass of Function1 that also carries symbolic information about the lambda-term (6)

- implement a new API of the form `val a: Int = from(x, y, z)` or `val a = to[Int](x, y, z)`, equivalent to

```scala
def f[T,X,Y,Z]: X => Y => Z => T = implement
val a: Int = f[Int, X, Y, Z](x, y, z)
```

 Release as a separate open-source project after (0)-(4) are done.
 */

object CurryHowardMacros {

  private val debug = false

  private[ch] def testType[T]: (String, String) = macro testTypeImpl[T]

  private[ch] val basicTypes = List("Int", "String", "Boolean", "Float", "Double", "Long", "Symbol", "Char")

  private val basicRegex = s"(?:scala.|java.lang.)*(${basicTypes.mkString("|")})".r

  // TODO: use c.Type instead of String -- ??? Not sure this is ever going to work.
  private def matchType(c: whitebox.Context)(t: c.Type): TypeExpr[String] = {
    // Could be the weird type [X, Y] => (type expression).
    // `finalResultType` seems to help here.
    val args = t.finalResultType.typeArgs

    import c.universe._

    //    val typeParams = t.typeParams // This is nonempty only for the weird types mentioned above.
    //    val valParamLists = t.paramLists
    // use something like t.paramLists(0)(0).typeSignature and also .isImplicit to extract types of function parameters
    t.typeSymbol.name.decodedName.toString match {
      case name if name matches "(scala\\.)?Tuple[0-9]+" ⇒ ConjunctT(args.map(matchType(c))) //s"(${args.map(matchType(c)).mkString(", ")})"
      case "scala.Function1" | "Function1" ⇒ matchType(c)(args.head) ->: matchType(c)(args(1)) // s"${matchType(c)(args(0))} → ${matchType(c)(args(1))}"
      case "scala.Option" | "Option" ⇒
        val argType = matchType(c)(args.head)
        DisjunctT("Option", List(argType), Seq(
          NamedConjunctT("None", List(NothingT("Nothing")), List(), UnitT("None")),
          NamedConjunctT("Some", List(argType), List("value"), matchType(c)(args.head))
        )) //s"(1 + ${matchType(c)(args.head)})"
      case "scala.util.Either" | "Either" ⇒
        val leftType = matchType(c)(args.head)
        val rightType = matchType(c)(args(1))
        DisjunctT("Either", List(leftType, rightType), Seq(
          NamedConjunctT("Left", List(leftType), List("value"), matchType(c)(args.head)),
          NamedConjunctT("Right", List(rightType), List("value"), matchType(c)(args(1)))
        )) //s"(${matchType(c)(args(0))} + ${matchType(c)(args(1))})"
      case "scala.Any" | "Any" ⇒ OtherT("_")
      case "scala.Nothing" | "Nothing" ⇒ NothingT("Nothing")
      case "scala.Unit" | "Unit" ⇒ UnitT("Unit")
      case basicRegex(name) ⇒ BasicT(name)
      case _ if args.isEmpty && t.baseClasses.map(_.fullName) == Seq("scala.Any") ⇒ TP(t.toString)
      case fullName if t.typeSymbol.isClass ⇒
        if (t.typeSymbol.asClass.isCaseClass) {
          // Detect case classes.
          // Detect all parts of the case class.
          val parts: List[(String, TypeExpr[String])] = t.decls
            .collect { case s: MethodSymbol if s.isCaseAccessor ⇒ (s.name.decodedName.toString, matchType(c)(s.typeSignature.resultType)) }
            .toList
          NamedConjunctT(fullName, args.map(matchType(c)), parts.map(_._1), ConjunctT(parts.map(_._2)))
        } else {
          val subclasses = t.typeSymbol.asClass.knownDirectSubclasses.toList.sortBy(_.name.decodedName.toString) // Otherwise the set is randomly ordered.
          if ((t.typeSymbol.asClass.isTrait || t.typeSymbol.asClass.isAbstract) &&
            subclasses.nonEmpty &&
            subclasses.forall(s ⇒ s.asClass.isCaseClass || s.asClass.isModuleClass) // `case object` is a "module class".
          ) {
            // Detect traits with case classes.
            val parts = subclasses.map(s ⇒ matchType(c)(s.asType.toType)) // Note: s.typeSignature does not work correctly here!
            DisjunctT(fullName, args.map(matchType(c)), parts)
          } else if (args.isEmpty) OtherT(fullName)
          else ConstructorT(t.toString)
        }
      case _ ⇒ ConstructorT(t.toString) // Sometimes we get <none> as the type symbol's name... Not sure what to do in that case.
    }
  }

  // This is used to put types on all function arguments within reifyParam().
  private def reifyType(c: whitebox.Context)(typeExpr: TypeExpr[String]): c.Tree = {
    import c.universe._

    def makeTypeName(nameT: String): c.universe.Tree = {
      val tpn = TypeName(nameT)
      tq"$tpn"
    }

    typeExpr match {
      case head #-> body ⇒ tq"(${reifyType(c)(head)}) ⇒ ${reifyType(c)(body)}"
      // TODO: Stop using String as type parameter T, use c.Type instead
      // TODO: make match exhaustive on tExpr, by using c.Type instead of String
      case TP(nameT) ⇒ makeTypeName(nameT)
      case BasicT(nameT) ⇒ makeTypeName(nameT)
      case OtherT(nameT) ⇒ makeTypeName(nameT)
      case NamedConjunctT(constructor, tParams, accessors, wrapped) ⇒
        val constructorT = makeTypeName(constructor)
        if (tParams.isEmpty) constructorT else {
          val tParamsTrees = tParams.map(tp ⇒ reifyType(c)(tp))
          tq"$constructorT[..$tParamsTrees]"
        }
      case NothingT(nameT) ⇒ makeTypeName(nameT)
      case UnitT(nameT) ⇒ makeTypeName(nameT)
      case ConjunctT(terms) ⇒ // Assuming this is a tuple type.
        val tpts = terms.map(t ⇒ reifyType(c)(t))
        tq"(..$tpts)"
      case _ ⇒ tq""
      //      case DisjunctT(terms) =>
    }
  }

  // Prepare the tree for a function parameter with the specified type.
  private def reifyParam(c: whitebox.Context)(term: PropE[String]): c.Tree = {
    import c.universe._
    term match {
      case PropE(name, typeExpr) ⇒
        val tpt = reifyType(c)(typeExpr)
        val termName = TermName("t_" + name.toString)
        val param = q"val $termName: $tpt"
        param
    }
  }

  private def reifyTerms(c: whitebox.Context)(termExpr: TermExpr[String], paramTerms: Map[PropE[String], c.Tree]): c.Tree = {
    import c.universe._

    termExpr match {
      case p@PropE(name, typeName) =>
        val tn = TermName("t_" + name.toString)
        q"$tn"
      case AppE(head, arg) => q"${reifyTerms(c)(head, paramTerms)}(${reifyTerms(c)(arg, paramTerms)})"

      // If `heads` = List(x, y, z) and `body` = b then the code must be x => y => z => b
      case CurriedE(heads, body) ⇒ heads.reverse.foldLeft(reifyTerms(c)(body, paramTerms)) { case (prevTree, paramE) ⇒
        val param = paramTerms(paramE) // Look up the parameter in the precomputed table.
        q"($param ⇒ $prevTree)"
      }
      case UnitE(_) => q"()"
      case ConjunctE(terms) ⇒ q"(..${terms.map(t ⇒ reifyTerms(c)(t, paramTerms))})"
      case NamedConjunctE(terms, tExpr) ⇒
        val constructorE = q"${TermName(tExpr.constructor)}"
        q"$constructorE[..${tExpr.tParams.map(t ⇒ reifyType(c)(t))}](..${terms.map(t ⇒ reifyTerms(c)(t, paramTerms))})"
      case ProjectE(index, term) ⇒
        val accessor = TermName(term.accessor(index))
        q"${reifyTerms(c)(term, paramTerms)}.$accessor"
      case DisjunctE(index, total, term, tExpr) ⇒ q"${reifyTerms(c)(term, paramTerms)}" // A disjunct term is always a NamedConjunctE, so we just reify that.
      case MatchE(term, cases) ⇒
        val disjunctTypesAndCases = term.tExpr.asInstanceOf[DisjunctT[String]].terms.zip(cases)
        val casesTrees: Seq[c.Tree] = disjunctTypesAndCases.map { case (disjunctT, disjunctCase) ⇒ ??? } // cq"$pat => $expr" where pat = pq"Constructor(..$varNames)"
        q"${reifyTerms(c)(term, paramTerms)} match { case ..$casesTrees }"
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


  // TODO: can we replace this with blackbox? Probably, as long as `def f3[X, Y]: X ⇒ Y ⇒ X = ofType` does not work with whitebox anyway.
  def ofTypeImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    val typeT: c.Type = c.weakTypeOf[T]
    inhabitInternal(c)(typeT)
  }

  def inhabitImpl[T](c: whitebox.Context): c.Tree = {
    val typeT = c.internal.enclosingOwner.typeSignature
    inhabitInternal(c)(typeT)
  }

  private def inhabitInternal(c: whitebox.Context)(typeT: c.Type): c.Tree = {
    import c.universe._
    type TExprType = String // (String, c.Type)
    val typeStructure: TypeExpr[TExprType] = matchType(c)(typeT)
    // TODO Check that there aren't repeated types among the curried arguments, print warning.
    TheoremProver(typeStructure) match {
      case (Nil, _) ⇒
        c.error(c.enclosingPosition, s"type $typeStructure cannot be implemented")
        q"null" // Avoid other spurious errors, return a valid tree here.
      case (List(termFound), count) ⇒
        if (count > 1) c.warning(c.enclosingPosition, s"type $typeStructure has $count implementations (laws need checking?)")
        //        println(s"DEBUG: Term found: $termFound, propositions: ${TermExpr.propositions(termFound)}")
        c.info(c.enclosingPosition, s"Returning term: ${termFound.prettyPrint}", force = true)
        val paramTerms: Map[PropE[String], c.Tree] = TermExpr.propositions(termFound).toSeq.map(p ⇒ p → reifyParam(c)(p)).toMap
        val result = reifyTerms(c)(termFound, paramTerms)

        //        val resultType = tq"${typeT.finalResultType}"
        //        val resultWithType = q"$result: $resultType" // this does not work
        if (debug) println(s"DEBUG: returning code: ${showCode(result)}")

        // use resultWithType? Doesn't seem to work.
        result

      case (list, _) ⇒
        c.error(c.enclosingPosition, s"type $typeStructure can be implemented in ${list.length} different ways: ${list.map(_.prettyPrint).mkString("; ")}")
        q"null"
    }


  }
}
