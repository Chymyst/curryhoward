package io.chymyst.ch

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
+ support named conjunctions (case classes) explicitly (3) and support disjunctions on that basis
+ support sealed traits / case classes (5)
+ implement Option and Either in inhabited terms (2)

+ add documentation using the `tut` plugin (3)
+ support natural syntax def f[T](x: T): T = implement (3)
 */

// TODO:
/*  Priority is given in parentheses.
- use c.Type instead of String for correct code generation (3)?? Probably impossible since we can't reify types directly from reflection results, - need to use names.
- probably can simplify data structures by eliminating [T]
- use blackbox macros instead of whitebox if possible (5) ?? Seems to prevent the " = implement" and "= ofType[]" syntax from working.
- implement uncurried functions and multiple argument lists (6)
- use a special subclass of Function1 that also carries symbolic information about the lambda-term (6)
- support recursive types such as List (right now this crashes with stack overflow)
- support type aliases (right now they are broken)

- implement a new API of the form `val a: Int = from(x, y, z)` or `val a = to[Int](x, y, z)`, equivalent to

```scala
def f[T,X,Y,Z]: X => Y => Z => T = implement
val a: Int = f[Int, X, Y, Z](x, y, z)
```

 Release as a separate open-source project after (0)-(4) are done.
 */

class CurryHowardMacros(val c: whitebox.Context) {

  import c.universe._

  private val debug = false

  private val basicRegex = s"(?:scala.|java.lang.)*(${CurryHowardMacros.basicTypes.mkString("|")})".r

  // TODO: use c.Type instead of String -- ??? Not sure this is ever going to work.
  private def matchType(t: c.Type, tMap: Map[c.Name, c.Type] = Map()): TypeExpr[String] = {

    // Could be the weird type [X, Y] => (type expression).
    // `finalResultType` seems to help here.
    val args = t.finalResultType.typeArgs.map(s ⇒ tMap.getOrElse(s.typeSymbol.name.decodedName, s))
    // Very verbose.
    //    if (debug) if (tMap.nonEmpty) println(s"DEBUG: matchType on $t with $tMap obtained args = $args")
    val matchedTypeArgs = args.map(s ⇒ matchType(s))
    //    if (debug) if (tMap.nonEmpty) println(s"DEBUG: matchType obtained matchedTypeArgs = ${matchedTypeArgs.map(_.prettyPrint)}")
    //    val typeParams = t.typeParams // This is nonempty only for the weird types mentioned above.
    //    val valParamLists = t.paramLists // typeU.paramLists(0)(0).name.decodedName is "x" as TermName; also typeU.finalResultType
    // use something like t.paramLists(0)(0).typeSignature and also .isImplicit to extract types of function parameters
    t.typeSymbol.name.decodedName.toString match {
      case name if name matches "(scala\\.)?Tuple[0-9]+" ⇒ ConjunctT(matchedTypeArgs)
      case "scala.Function1" | "Function1" ⇒ matchType(args.head) ->: matchType(args(1))
      case "scala.Any" | "Any" ⇒ OtherT("_")
      case "scala.Nothing" | "Nothing" ⇒ NothingT("Nothing")
      case "scala.Unit" | "Unit" ⇒ UnitT("Unit")
      case basicRegex(name) ⇒ BasicT(name)
      case _ if args.isEmpty && t.baseClasses.map(_.fullName) == Seq("scala.Any") ⇒ TP(t.toString)
      //      case fullName if t.typeSymbol.isType && t.typeSymbol.asType.isAliasType ⇒ ??? // Not sure this helps.
      case fullName if t.typeSymbol.isClass ⇒
        val typeMap: Map[c.Name, c.Type] = t.typeSymbol.asClass.typeParams.map(_.name.decodedName).zip(args).toMap

        if (t.typeSymbol.asClass.isModuleClass) { // `case object` is a "module class", but also a "case class".
          NamedConjunctT(fullName, Nil, Nil, Nil)
        } else if (t.typeSymbol.asClass.isCaseClass) {
          // Case class.

          // Need to assign type parameters to the accessors.

          //  t.decls.toList(0).asMethod.typeSignature.resultType gives A, t.typeSymbol.asClass.typeParams(0) ==  t.decls.toList(0).asMethod.typeSignature.resultType.typeSymbol
          /* t.typeSymbol.asClass.typeParams gives List(A, B)*/
          val (accessors, typeExprs) = t.decls
            .collect { case s: MethodSymbol if s.isCaseAccessor ⇒
              val accessorType = s.typeSignature.resultType
              val substitutedType = typeMap.getOrElse(accessorType.typeSymbol.name.decodedName, accessorType)
              (s.name.decodedName.toString, matchType(substitutedType))
            }
            .toList.unzip
          val wrapped = typeExprs match {
            case Nil ⇒ List(UnitT(fullName)) // Case class with no arguments is represented by a named Unit.
            case _ ⇒ typeExprs // Case class with some arguments.
          }
          NamedConjunctT(fullName, matchedTypeArgs, accessors, wrapped)
        } else {
          // Possibly a disjunction type.

          // Note: `Either` is a "type" rather than a class, even though isClass() returns true.
          // traits / case classes are classes, but knownDirectSubclasses does not work for both cases.
          // Prepare a type symbol that works for both cases.
          val typeSymbol = if (t.typeSymbol.isType) t.typeSymbol.asType.toType.typeSymbol else t.typeSymbol
          val subclasses = typeSymbol
            .asClass.knownDirectSubclasses
            .toList.sortBy(_.name.decodedName.toString) // Otherwise the set is randomly ordered.
          // Each subclass must be a case class or case object, otherwise we do not process the type t.
          if ((typeSymbol.asClass.isTrait || typeSymbol.asClass.isAbstract) &&
            subclasses.nonEmpty &&
            subclasses.forall { s ⇒
              val resultClass = s.typeSignature.resultType.typeSymbol.asClass
              resultClass.isCaseClass || resultClass.isModuleClass // `case object` is a "module class".
            }
          ) {
            // Detected a trait with case classes.

            // Note: s.typeSignature does not work correctly here! Need s.asType.toType
            subclasses.map(s ⇒ matchType(s.asType.toType, typeMap)) match {
              case part :: Nil ⇒ part // A single case class implementing a trait.
              case parts ⇒ DisjunctT(fullName, matchedTypeArgs, parts) // Several case classes implementing a trait.
            }
          } else if (args.isEmpty) OtherT(fullName)
          else ConstructorT(t.toString)
        }
      case _ ⇒ ConstructorT(t.toString) // Sometimes we get <none> as the type symbol's name... Not sure what to do in that case.
    }
  }

  // This is used to put types on all function arguments within reifyParam().
  private def reifyType(typeExpr: TypeExpr[String]): c.Tree = {

    def makeTypeName(nameT: String): c.universe.Tree = {
      val tpn = TypeName(nameT)
      tq"$tpn"
    }

    typeExpr match {
      case head #-> body ⇒ tq"(${reifyType(head)}) ⇒ ${reifyType(body)}"
      // TODO: Stop using String as type parameter T, use c.Type instead
      // TODO: make match exhaustive on tExpr, by using c.Type instead of String
      case TP(nameT) ⇒ makeTypeName(nameT)
      case BasicT(nameT) ⇒ makeTypeName(nameT)
      case OtherT(nameT) ⇒ makeTypeName(nameT)
      case NamedConjunctT(constructor, tParams, accessors, wrapped) ⇒
        val constructorT = makeTypeName(constructor)
        val constructorWithTypeParams = if (tParams.isEmpty) constructorT else {
          val tParamsTrees = tParams.map(reifyType)
          tq"$constructorT[..$tParamsTrees]"
        }
        if (typeExpr.caseObjectName.isDefined) {
          tq"$constructorT.type" // case object
        } else constructorWithTypeParams
      case NothingT(nameT) ⇒ makeTypeName(nameT)
      case UnitT(nameT) ⇒ makeTypeName(nameT)
      case ConjunctT(terms) ⇒ // Assuming this is a tuple type.
        val tpts = terms.map(reifyType)
        tq"(..$tpts)"
      case DisjunctT(constructor, tParams, terms) ⇒
        val constructorT = makeTypeName(constructor)
        if (tParams.isEmpty) constructorT else {
          val tParamsTrees = tParams.map(reifyType)
          tq"$constructorT[..$tParamsTrees]"
        }
      case _ ⇒ tq""
    }
  }

  // Prepare the tree for a function parameter with the specified type.
  private def reifyParam(term: PropE[String]): c.Tree = term match {
    case PropE(name, typeExpr) ⇒
      val tpt = reifyType(typeExpr)
      val termName = TermName(name.toString)
      val param = q"val $termName: $tpt"
      param
  }

  private def reifyTerm(termExpr: TermExpr[String], paramTerms: Map[PropE[String], c.Tree]): c.Tree = {
    termExpr match {
      case p@PropE(name, typeName) =>
        val tn = TermName(name.toString)
        q"$tn"
      case AppE(head, arg) => q"${reifyTerm(head, paramTerms)}(${reifyTerm(arg, paramTerms)})"

      // If `heads` = List(x, y, z) and `body` = b then the code must be x => y => z => b
      case CurriedE(heads, body) ⇒ heads.reverse.foldLeft(reifyTerm(body, paramTerms)) { case (prevTree, paramE) ⇒
        val param = paramTerms(paramE) // Look up the parameter in the precomputed table.
        q"($param ⇒ $prevTree)"
      }
      case UnitE(_) => q"()"
      case ConjunctE(terms) ⇒ q"(..${terms.map(t ⇒ reifyTerm(t, paramTerms))})"
      case NamedConjunctE(terms, tExpr) ⇒
        val constructorE = q"${TermName(tExpr.constructor)}[..${tExpr.tParams.map(reifyType)}]"
        if (terms.isEmpty)
          constructorE // avoid spurious parameter lists for case objects
        else
          q"$constructorE(..${terms.map(t ⇒ reifyTerm(t, paramTerms))})"
      case ProjectE(index, term) ⇒
        val accessor = TermName(term.accessor(index))
        q"${reifyTerm(term, paramTerms)}.$accessor"
      case DisjunctE(index, total, term, tExpr) ⇒ q"${reifyTerm(term, paramTerms)}" // A disjunct term is always a NamedConjunctE, so we just reify that.
      case MatchE(term, cases) ⇒
        // Each term within `cases` is always a CurriedE because it is of the form fv ⇒ proofTerm(fv, other_premises).
        val casesTrees: Seq[c.Tree] = cases.map {
          case CurriedE(PropE(fvName, fvType) :: _, body) ⇒
            // cq"$pat => $expr" where pat = pq"Constructor(..$varNames)"
            val pat = fvType.caseObjectName match {
              case Some(constructor) ⇒ pq"_ : ${TermName(constructor)}.type"
              case None ⇒ pq"${TermName(fvName)} : ${reifyType(fvType)}"
            }
            cq"$pat => ${reifyTerm(body, paramTerms)}"
          case cc ⇒ throw new Exception(s"Internal error: `case` term ${cc.prettyPrint} must be a function")
        }

        q"${reifyTerm(term, paramTerms)} match { case ..$casesTrees }"
    }
  }

  def testReifyTypeImpl[U: c.WeakTypeTag]: c.Expr[TypeExpr[String]] = {
    val typeU: c.Type = c.weakTypeOf[U]
    val result = matchType(typeU.resultType)
    if (debug) println(s"DEBUG: reified result from type $typeU is ${result.prettyPrint}")

    implicit def liftedNamedConjuct: Liftable[NamedConjunctT[String]] = Liftable[NamedConjunctT[String]] {
      case NamedConjunctT(constructor, tParams, accessors, wrapped) ⇒ q"_root_.io.chymyst.ch.NamedConjunctT($constructor, Seq(..$tParams), Seq(..$accessors), $wrapped)"
    }

    implicit def liftedPropE: Liftable[PropE[String]] = Liftable[PropE[String]] {
      case PropE(name, tExpr) ⇒ q"_root_.io.chymyst.ch.PropE($name, $tExpr)"
    }

    implicit def liftedTypeExpr: Liftable[TypeExpr[String]] = Liftable[TypeExpr[String]] {
      case DisjunctT(constructor, tParams, terms) ⇒ q"_root_.io.chymyst.ch.DisjunctT($constructor, Seq(..$tParams), Seq(..$terms))"
      case ConjunctT(terms) ⇒ q"_root_.io.chymyst.ch.ConjunctT(Seq(..$terms))"
      case #->(head, body) ⇒ q"_root_.io.chymyst.ch.#->($head, $body)"
      case NothingT(name) ⇒ q"_root_.io.chymyst.ch.NothingT($name)"
      case UnitT(name) ⇒ q"_root_.io.chymyst.ch.UnitT($name)"
      case TP(name) ⇒ q"_root_.io.chymyst.ch.TP($name)"
      case OtherT(name) ⇒ q"_root_.io.chymyst.ch.OtherT($name)"
      case BasicT(name) ⇒ q"_root_.io.chymyst.ch.BasicT($name)"
      case NamedConjunctT(constructor, tParams, accessors, wrapped) ⇒ q"_root_.io.chymyst.ch.NamedConjunctT($constructor, List(..$tParams), List(..$accessors), $wrapped)"
      case ConstructorT(name) ⇒ q"_root_.io.chymyst.ch.ConstructorT($name)"
    }

    implicit def liftedTermExpr: Liftable[TermExpr[String]] = Liftable[TermExpr[String]] {
      case PropE(name, tExpr) ⇒ q"_root_.io.chymyst.ch.PropE($name, $tExpr)"
      case AppE(head, arg) ⇒ q"_root_.io.chymyst.ch.AppE($head, $arg)"
      case CurriedE(heads, body) ⇒ q"_root_.io.chymyst.ch.CurriedE(List(..$heads), $body)"
      case UnitE(tExpr) ⇒ q"_root_.io.chymyst.ch.UnitE($tExpr)"
      case NamedConjunctE(terms, tExpr) ⇒ q"_root_.io.chymyst.ch.NamedConjunctE(Seq(..$terms), $tExpr)"
      case ConjunctE(terms) ⇒ q"_root_.io.chymyst.ch.ConjunctE(Seq(..$terms))"
      case ProjectE(index, term) ⇒ q"_root_.io.chymyst.ch.ProjectE($index, $term)"
      case MatchE(term, cases) ⇒ q"_root_.io.chymyst.ch.MatchE($term, List(..$cases))"
      case DisjunctE(index, total, term, tExpr) ⇒ q"_root_.io.chymyst.ch.DisjunctE($index, $total, $term, $tExpr)"
    }

    c.Expr[TypeExpr[String]](q"$result")
  }

  def testReifyTermsImpl[U: c.WeakTypeTag]: c.Tree = {
    val typeU: c.Type = c.weakTypeOf[U]
    val result = TheoremProver.findProofs(matchType(typeU.resultType))._1

    implicit def liftedNamedConjuct: Liftable[NamedConjunctT[String]] = Liftable[NamedConjunctT[String]] {
      case NamedConjunctT(constructor, tParams, accessors, wrapped) ⇒ q"_root_.io.chymyst.ch.NamedConjunctT($constructor, Seq(..$tParams), Seq(..$accessors), $wrapped)"
    }

    implicit def liftedPropE: Liftable[PropE[String]] = Liftable[PropE[String]] {
      case PropE(name, tExpr) ⇒ q"_root_.io.chymyst.ch.PropE($name, $tExpr)"
    }

    implicit def liftedTypeExpr: Liftable[TypeExpr[String]] = Liftable[TypeExpr[String]] {
      case DisjunctT(constructor, tParams, terms) ⇒ q"_root_.io.chymyst.ch.DisjunctT($constructor, Seq(..$tParams), Seq(..$terms))"
      case ConjunctT(terms) ⇒ q"_root_.io.chymyst.ch.ConjunctT(Seq(..$terms))"
      case #->(head, body) ⇒ q"_root_.io.chymyst.ch.#->($head, $body)"
      case NothingT(name) ⇒ q"_root_.io.chymyst.ch.NothingT($name)"
      case UnitT(name) ⇒ q"_root_.io.chymyst.ch.UnitT($name)"
      case TP(name) ⇒ q"_root_.io.chymyst.ch.TP($name)"
      case OtherT(name) ⇒ q"_root_.io.chymyst.ch.OtherT($name)"
      case BasicT(name) ⇒ q"_root_.io.chymyst.ch.BasicT($name)"
      case NamedConjunctT(constructor, tParams, accessors, wrapped) ⇒ q"_root_.io.chymyst.ch.NamedConjunctT($constructor, List(..$tParams), List(..$accessors), $wrapped)"
      case ConstructorT(name) ⇒ q"_root_.io.chymyst.ch.ConstructorT($name)"
    }

    implicit def liftedTermExpr: Liftable[TermExpr[String]] = Liftable[TermExpr[String]] {
      case PropE(name, tExpr) ⇒ q"_root_.io.chymyst.ch.PropE($name, $tExpr)"
      case AppE(head, arg) ⇒ q"_root_.io.chymyst.ch.AppE($head, $arg)"
      case CurriedE(heads, body) ⇒ q"_root_.io.chymyst.ch.CurriedE(List(..$heads), $body)"
      case UnitE(tExpr) ⇒ q"_root_.io.chymyst.ch.UnitE($tExpr)"
      case NamedConjunctE(terms, tExpr) ⇒ q"_root_.io.chymyst.ch.NamedConjunctE(Seq(..$terms), $tExpr)"
      case ConjunctE(terms) ⇒ q"_root_.io.chymyst.ch.ConjunctE(Seq(..$terms))"
      case ProjectE(index, term) ⇒ q"_root_.io.chymyst.ch.ProjectE($index, $term)"
      case MatchE(term, cases) ⇒ q"_root_.io.chymyst.ch.MatchE($term, List(..$cases))"
      case DisjunctE(index, total, term, tExpr) ⇒ q"_root_.io.chymyst.ch.DisjunctE($index, $total, $term, $tExpr)"
    }

    q"$result"
  }

  def testTypeImpl[T: c.WeakTypeTag]: c.Expr[(String, String)] = {
    val typeT: c.Type = c.weakTypeOf[T]
    val enclosingType = c.internal.enclosingOwner.typeSignature

    val s1 = matchType(typeT.resultType).prettyPrint
    val s2 = matchType(enclosingType).prettyPrint

    c.Expr[(String, String)](q"($s1,$s2)")
  }

  // TODO: can we replace this with blackbox? So far this only works with whitebox.
  // Obtain one implementation of the type U, which must be given as the type parameter.
  // This function does not attempt to detect the left-hand side type,
  // and so does not give rise to the "recursive value must have type" error.
  def ofTypeImpl[U: c.WeakTypeTag]: c.Tree = {
    val typeU: c.Type = c.weakTypeOf[U]
    inhabitOneInternal(typeU)
  }

  // Obtain one implementation of the type U. Detect the type U as given on the left-hand side.
  def inhabitImpl[U]: c.Tree = {
    val typeU = c.internal.enclosingOwner.typeSignature
    // Detect whether we are given a function with arguments.
    typeU.resultType.paramLists match {
      case Nil ⇒ inhabitOneInternal(typeU)
      case lists ⇒
        val givenVars = lists.flatten.map(s ⇒ PropE(s.name.decodedName.toString, matchType(s.typeSignature)))
        val resultType = matchType(typeU.finalResultType)
        val typeStructure = givenVars.map(_.tExpr).reverse.foldLeft(resultType) { case (prev, t) ⇒ t ->: prev }
        inhabitInternal(typeStructure) match {
          case Right(term) ⇒
            val termFound = givenVars.foldLeft(term) { case (prev, v) ⇒ AppE(prev, v) }.simplify
            c.info(c.enclosingPosition, s"Returning term: ${termFound.prettyPrintWithParentheses(0)}", force = true)
            val paramTerms: Map[PropE[String], c.Tree] = TermExpr.propositions(termFound).toSeq.map(p ⇒ p → reifyParam(p)).toMap
            val result = reifyTerm(termFound, paramTerms)
            if (debug) println(s"DEBUG: returning code: ${showCode(result)}")
            result
          case Left(errorMessage) ⇒
            c.error(c.enclosingPosition, errorMessage)
            q"null"
        }

    }
  }

  def allOfTypeImpl[U: c.WeakTypeTag]: c.Tree = {
    val typeU: c.Type = c.weakTypeOf[U]
    inhabitAllInternal(typeU)
  }

  private def inhabitOneInternal(typeT: c.Type): c.Tree = {
    val typeStructure: TypeExpr[String] = matchType(typeT)
    inhabitInternal(typeStructure) match {
      case Right(termFound) ⇒
        c.info(c.enclosingPosition, s"Returning term: ${termFound.prettyPrintWithParentheses(0)}", force = true)
        val paramTerms: Map[PropE[String], c.Tree] = TermExpr.propositions(termFound).toSeq.map(p ⇒ p → reifyParam(p)).toMap
        val result = reifyTerm(termFound, paramTerms)
        //        val resultType = tq"${typeT.finalResultType}"
        //        val resultWithType = q"$result: $resultType" // this does not work
        if (debug) println(s"DEBUG: returning code: ${showCode(result)}")
        result
      case Left(errorMessage) ⇒
        c.error(c.enclosingPosition, errorMessage)
        q"null"
    }

  }

  private def inhabitInternal(typeStructure: TypeExpr[String]): Either[String, TermExpr[String]] = {
    // TODO Check that there aren't repeated types among the curried arguments, print warning.
    TheoremProver.findProofs(typeStructure) match {
      case (Nil, _) ⇒
        Left(s"type ${typeStructure.prettyPrint} cannot be implemented")
      case (List(termFound), allTerms) ⇒
        val count = allTerms.length
        if (count > 1) c.warning(c.enclosingPosition, s"type ${typeStructure.prettyPrint} has $count implementations (laws need checking?):\n ${allTerms.map(_.prettyPrint).mkString(";\n ")}.")
        //        println(s"DEBUG: Term found: $termFound, propositions: ${TermExpr.propositions(termFound)}")
        Right(termFound)
      case (list, _) ⇒
        Left(s"type ${typeStructure.prettyPrint} can be implemented in ${list.length} equivalent ways:\n ${list.map(_.prettyPrint).mkString(";\n ")}.")
    }
  }

  private def inhabitAllInternal(typeT: c.Type): c.Tree = {
    type TExprType = String // (String, c.Type)
    val typeStructure: TypeExpr[TExprType] = matchType(typeT)
    val terms = TheoremProver.findProofs(typeStructure)._1.map { termFound ⇒
      c.info(c.enclosingPosition, s"Returning term: ${termFound.prettyPrint}", force = true)
      val paramTerms: Map[PropE[TExprType], c.Tree] = TermExpr.propositions(termFound).toSeq.map(p ⇒ p → reifyParam(p)).toMap
      val result = reifyTerm(termFound, paramTerms)
      if (debug) println(s"DEBUG: returning code: ${showCode(result)}")
      // use resultWithType? Doesn't seem to work.
      result
    }
    q"Seq(..$terms)"
  }
}

object CurryHowardMacros {
  private[ch] val basicTypes = List("Int", "String", "Boolean", "Float", "Double", "Long", "Symbol", "Char")

  private[ch] def testType[U]: (String, String) = macro CurryHowardMacros.testTypeImpl[U]

  private[ch] def testReifyType[U]: TypeExpr[String] = macro CurryHowardMacros.testReifyTypeImpl[U]

  private[ch] def testReifyTerm[U]: List[TermExpr[String]] = macro CurryHowardMacros.testReifyTermsImpl[U]
}