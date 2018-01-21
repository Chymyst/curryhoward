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

/*  Priority is given in parentheses.
- use c.Type and c.Tree instead of String for correct code generation (3)?? Probably impossible since we can't reify types directly from reflection results, - need to use names.
- probably can simplify data structures by eliminating [T], VarName, and ProofTerm
- make sure the alpha-conversion is correct on type parameters! This seems to be a deeper problem.
- think about supporting recursive types beyond the palliative measures implemented currently
- conjunction / disjunction with permuted order of results should be disfavored, prefer original order. This should help Option[Option[T]] or (T, T)
- use blackbox macros instead of whitebox if possible (5) ?? Seems to prevent the " = implement" and "= ofType[]" syntax from working.
- implement uncurried functions and multiple argument lists (6)?? Not sure this helps. We can already do this with `implement`.
- use a special subclass of Function1 that also carries symbolic information about the lambda-term (6)

 */

// TODO: can we replace this with blackbox? So far this only works with whitebox.
class Macros(val c: whitebox.Context) {

  import c.universe._

  private def debug = Macros.options contains "macros"

  private def showReturningTerm = Macros.options contains "terms"

  /** Convert a Scala `Type` value into an expression tree of type TypeExpr[String].
    * This function performs further reflection steps recursively in order to detect sealed traits / case classes
    * or other nested types.
    *
    * There are two cases when this function is called: 1) to build a TypeExpr from a given Scala type expression,
    * 2) recursively, to build a TypeExpr from c.Type information obtained via reflection - this is used for case class
    * accessors as well as for case classes discovered by reflection on a trait. In the second case, we may need to rename
    * some type parameters or replace them by other type expressions.
    *
    * @param t         A `Type` value obtained via reflection.
    * @param tMap      A map of known substitutions for type parameters.
    * @param typesSeen Types of case classes or sealed traits that are possibly being recursed on.
    * @return Type expression corresponding to the type `t`, with correctly assigned type parameters.
    */
  private def buildTypeExpr(t: c.Type, tMap: Seq[(String, TypeExpr[String])] = Seq(), typesSeen: Set[String] = Set()): TypeExpr[String] = {

    val typeName = t.typeSymbol.name.decodedName.toString

    val typesSeenNow = typesSeen + typeName

    // Could be the weird type [X, Y] => (type expression).
    // `finalResultType` seems to help here.
    val matchedTypeArgs = t.finalResultType.typeArgs
      .map(s ⇒ tMap.toMap.getOrElse(s.typeSymbol.name.decodedName.toString, buildTypeExpr(s, Seq(), typesSeen))) // `typesSeen` prevents infinite loops when  a type parameter is the same as the recursive type.
    // Very verbose; enable only when debugging the renaming of type parameters.
    //    if (debug) println(s"DEBUG: buildTypeExpr on $t with $tMap obtained args = ${t.finalResultType.typeArgs.mkString("; ")}")
    //    if (debug) println(s" DEBUG: buildTypeExpr obtained matchedTypeArgs = ${matchedTypeArgs.map(_.prettyPrint).mkString("; ")}")

    //    val typeParams = t.typeParams // This is nonempty only for the weird types mentioned above.
    //    val valParamLists = t.paramLists // typeU.paramLists(0)(0).name.decodedName is "x" as TermName; also typeU.finalResultType
    // use something like t.paramLists(0)(0).typeSignature and also .isImplicit to extract types of function parameters

    typeName match {
      case name if name matches "(scala\\.)?Tuple[0-9]+" ⇒ ConjunctT(matchedTypeArgs)
      case "scala.Function1" | "Function1" ⇒ matchedTypeArgs.head ->: matchedTypeArgs(1)
      case "scala.Any" | "Any" ⇒ BasicT("_")
      case "scala.Nothing" | "Nothing" ⇒ NothingT("Nothing")
      case "scala.Unit" | "Unit" ⇒ UnitT("Unit")
      case _ if matchedTypeArgs.isEmpty && t.baseClasses.map(_.fullName) == Seq("scala.Any") ⇒ TP(t.toString)
      //      case fullName if t.typeSymbol.isType && t.typeSymbol.asType.isAliasType ⇒ ??? // Not sure `isAliasType()` ever helps. Use .dealias() on a Type?
      case _ if typesSeen contains typeName ⇒ RecurseT(typeName, matchedTypeArgs)
      case _ if t.typeSymbol.isClass ⇒
        // A type constructor, a case class, a sealed trait / abstract class with case classes, or other class.
        val typeMap: Seq[(String, TypeExpr[String])] = t.typeSymbol.asClass.typeParams
          .map(_.name.decodedName.toString)
          .zip(matchedTypeArgs)

        // t.tpeCache.baseTypeSeqCache contains BTS(... BaseClass[U] ...)
        // t.typeSymbol.asClass.knownDirectSubclasses.toList(0).typeSignature.baseType(t.typeSymbol.asClass.knownDirectSubclasses.toList(0).typeSignature.baseClasses(5)) is BaseClass[U]
        // Very verbose; enable only when debugging the renaming of type parameters.
        //        if (debug) println(s" DEBUG: buildTypeExpr on $t with $tMap obtained typeMap = ${typeMap.map(_._2.prettyPrint).mkString("; ")}")

        if (t.typeSymbol.asClass.isModuleClass) { // `case object` is a "module class", but also a "case class".
          NamedConjunctT(typeName, Nil, Nil, Nil) // This represents a case object.
        } else if (t.typeSymbol.asClass.isCaseClass) {

          // Detected a case class with zero or more accessors.

          // Need to assign type parameters to the accessors.

          //  t.decls.toList(0).asMethod.typeSignature.resultType gives A, t.typeSymbol.asClass.typeParams(0) ==  t.decls.toList(0).asMethod.typeSignature.resultType.typeSymbol
          /* t.typeSymbol.asClass.typeParams gives List(A, B)*/
          val (accessors, typeExprs) = t.decls
            .collect { case s: MethodSymbol if s.isCaseAccessor ⇒
              val accessorType = buildTypeExpr(s.typeSignature.resultType, Seq(), typesSeenNow)
              val substitutedType = TypeExpr.substNames(accessorType, typeMap.toMap)
              //              if (debug) println(s"  DEBUG: buildTypeExpr on $t with $tMap obtained substitutedType = ${substitutedType.prettyPrint} from accessorType = ${accessorType.prettyPrint}")

              (s.name.decodedName.toString, substitutedType)
            }
            .toList.unzip
          val wrapped = typeExprs match {
            case Nil ⇒ List(UnitT(typeName)) // Case class with no arguments is represented by an empty accessors list
            // and a non-empty type list with a single UnitT in it (a "named unit").
            case _ ⇒ typeExprs // Case class with some arguments.
          }
          NamedConjunctT(typeName, matchedTypeArgs, accessors, wrapped)
        } else {
          // Not a single case class or case object. Possibly a disjunction (trait extended with case classes).

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
            // Detected a trait extended with one or more case classes.

            // Note: s.typeSignature does not work correctly here! Need s.asType.toType
            subclasses.map { s ⇒
              val subclassType = buildTypeExpr(s.asType.toType, Seq(), typesSeenNow)
              // The type of the subclass maybe a type-Lambda, in which case we need to discover the correct variable substitution and perform it.
              if (subclassType.typeParams.nonEmpty) {
                // Discover the type parameters actually used when this subclass extends the parent trait.
                val baseType = s.typeSignature.baseType(t.typeSymbol.asClass)
                // Substitute these type parameter names with the actual types used by the parent trait (as given by typeMap).
                val substMap: Map[String, TypeExpr[String]] = baseType.typeArgs.map(_.typeSymbol.name.decodedName.toString).zip(typeMap.map(_._2)).toMap
                //                println(s"DEBUG: baseType = ${baseType.typeSymbol.name.decodedName.toString}, baseType.typeParams = ${baseType.typeArgs.map(_.typeSymbol.name.decodedName.toString)}, substMap = ${substMap.mapValues(_.prettyPrint)}")
                TypeExpr.substNames(subclassType, substMap)
              } else subclassType
            } match {
              case part :: Nil ⇒ part // A single case class implementing a trait. This is not a disjunction.
              case parts ⇒
                //                if (debug) println(s"  DEBUG: buildTypeExpr on $t with $tMap returning DisjunctT($fullName, ${matchedTypeArgs.map(_.prettyPrint)}), parts=${parts.map(_.prettyPrint)}")
                DisjunctT(typeName, matchedTypeArgs, parts) // Several case classes implementing a trait.
            }
          } else if (matchedTypeArgs.isEmpty) BasicT(typeName)
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
      case TP(nameT) ⇒ makeTypeName(nameT)
      case BasicT(nameT) ⇒ makeTypeName(nameT)
      case RecurseT(nameT, tParams) ⇒
        val constructorT = makeTypeName(nameT)
        val constructorWithTypeParams = if (tParams.isEmpty) constructorT else {
          val tParamsTrees = tParams.map(reifyType)
          tq"$constructorT[..$tParamsTrees]"
        }
        constructorWithTypeParams
      case NamedConjunctT(constructor, tParams, _, _) ⇒
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
      case DisjunctT(constructor, tParams, _) ⇒
        val constructorT = makeTypeName(constructor)
        if (tParams.isEmpty) constructorT else {
          val tParamsTrees = tParams.map(reifyType)
          tq"$constructorT[..$tParamsTrees]"
        }
      // TODO: reify this with type parameters
      case ConstructorT(name) ⇒ makeTypeName(name)
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

  private def reifyTerm(termExpr: TermExpr[String], paramTerms: Map[PropE[String], c.Tree], givenArgs: Map[PropE[String], c.Tree]): c.Tree = {
    // Shortcut for calling this function recursively with all the same arguments.
    def reifyTermShort(termExpr: TermExpr[String]): c.Tree = reifyTerm(termExpr, paramTerms, givenArgs)

    termExpr match {
      case p@PropE(name, _) ⇒ givenArgs.getOrElse(p, q"${TermName(name.toString)}")

      case AppE(head, arg) ⇒ q"${reifyTermShort(head)}(${reifyTermShort(arg)})"

      // If `heads` = List(x, y, z) and `body` = b then the code must be x => y => z => b
      case CurriedE(heads, body) ⇒ heads.reverse.foldLeft(reifyTermShort(body)) { case (prevTree, paramE) ⇒
        val param = paramTerms(paramE) // Look up the parameter in the precomputed table.
        q"($param ⇒ $prevTree)"
      }
      case UnitE(_) ⇒ q"()"
      case ConjunctE(terms) ⇒ q"(..${terms.map(t ⇒ reifyTermShort(t))})"
      case NamedConjunctE(terms, tExpr) ⇒
        val constructorE = q"${TermName(tExpr.constructor)}[..${tExpr.tParams.map(reifyType)}]"
        if (tExpr.isAtomic)
          constructorE // avoid spurious parameter lists for case objects
        else
          q"$constructorE(..${terms.map(t ⇒ reifyTermShort(t))})"
      case ProjectE(index, term) ⇒
        val accessor = TermName(term.accessor(index))
        q"${reifyTermShort(term)}.$accessor"
      case DisjunctE(_, _, term, _) ⇒ q"${reifyTermShort(term)}" // A disjunct term is always a NamedConjunctE, so we just reify that.
      case MatchE(term, cases) ⇒
        // Each term within `cases` is always a CurriedE because it is of the form fv ⇒ proofTerm(fv, other_premises).
        val casesTrees: Seq[c.Tree] = cases.map {
          // However, at this point a MatchE() could contain a CurriedE() with multiple arguments,
          // as a result of possibly simplifying nested CurriedE().
          case CurriedE(PropE(fvName, fvType) :: rest, body) ⇒
            // cq"$pat => $expr" where pat = pq"Constructor(..$varNames)"
            val pat = fvType.caseObjectName match {
              case Some(constructor) ⇒ pq"_ : ${TermName(constructor)}.type"
              case None ⇒ pq"${TermName(fvName)} : ${reifyType(fvType)}"
            }
            cq"$pat => ${reifyTermShort(if (rest.isEmpty) body else CurriedE(rest, body))}"
          case cc ⇒ throw new Exception(s"Internal error: `case` term ${cc.prettyPrint} must be a function")
        }

        q"${reifyTermShort(term)} match { case ..$casesTrees }"
    }
  }

  // This function is for testing only.
  def testReifyTypeImpl[U: c.WeakTypeTag]: c.Expr[TypeExpr[String]] = {
    val typeU: c.Type = c.weakTypeOf[U]
    val result = buildTypeExpr(typeU.resultType)
    if (debug) c.info(c.enclosingPosition, s"Recognized type from type $typeU is ${result.prettyPrint}", force = true)

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
      case RecurseT(name, tParams) ⇒ q"_root_.io.chymyst.ch.RecurseT($name, List(..$tParams))"
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

  /* Not used now.
    def testReifyTermsImpl[U: c.WeakTypeTag]: c.Tree = {
      val typeU: c.Type = c.weakTypeOf[U]
      val result = TheoremProver.findProofs(buildTypeExpr(typeU.resultType))._1

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
  */
  // This function is for testing buildTypeExpr().
  def testTypeImpl[T: c.WeakTypeTag]: c.Expr[(String, String)] = {
    val typeT: c.Type = c.weakTypeOf[T]
    val enclosingType = c.internal.enclosingOwner.typeSignature

    val s1 = buildTypeExpr(typeT.resultType).prettyPrint
    val s2 = buildTypeExpr(enclosingType).prettyPrint

    c.Expr[(String, String)](q"($s1,$s2)")
  }

  // Obtain one implementation of the type U. Detect the type U as given on the left-hand side.
  def inhabitImpl[U]: c.Tree = {
    val typeU = c.internal.enclosingOwner.typeSignature
    // Detect whether we are given a function with arguments.
    val result = typeU.resultType.paramLists match {
      case Nil ⇒ inhabitOneInternal(buildTypeExpr(typeU))()
      case lists ⇒
        val givenVars = lists.flatten.map(s ⇒ PropE(s.name.decodedName.toString, buildTypeExpr(s.typeSignature)))
        val resultType = buildTypeExpr(typeU.finalResultType)
        val typeStructure = givenVars.reverse.foldLeft(resultType) { case (prev, t) ⇒ t.tExpr ->: prev }
        inhabitOneInternal(typeStructure) { term ⇒ givenVars.foldLeft(term) { case (prev, v) ⇒ AppE(prev, v) } }
    }
    result
  }

  // Obtain one implementation of the type U, which must be given as the type parameter.
  // This function does not attempt to detect the left-hand side type,
  // and so does not give rise to the "recursive value must have type" error.
  def ofTypeImpl[U: c.WeakTypeTag]: c.Tree = ofTypeImplWithValues[U]()

  def ofTypeImplWithValues[U: c.WeakTypeTag](values: c.Expr[Any]*): c.Tree = {
    val typeUGiven: c.Type = c.weakTypeOf[U]
    val typeUT = buildTypeExpr(typeUGiven)

    // TODO: decide if we can still use `ofType` with auto-detection of left-hand side values.
    // If so, we can stop using two different names for these use cases.
    // This does not seem to work in a macro with arguments! (But it might for the version of `ofTypeImpl` without arguments.)
    // TODO: Perhaps we can support `implement` with a specified type argument and no auto-detection of enclosing owner's type.

    // We need to obtain the actual type of the given terms, rather than the `Any` type as specified in the type signature.
    val givenVars: Seq[(PropE[String], c.Tree)] = values.zipWithIndex
      // The given values will be marked as arguments with name `arg1`, `arg2`, etc.,
      // so that they do not mix with automatic variables in the generated closed term.
      .map { case (v, i) ⇒ (PropE(s"arg${i + 1}", buildTypeExpr(v.actualType)), v.tree) }
    val givenVarsAsArgs = givenVars.map(_._1)
    val typeStructure = givenVarsAsArgs.reverse.foldLeft(typeUT) { case (prev, t) ⇒ t.tExpr ->: prev }
    inhabitOneInternal(typeStructure, givenVars.toMap) { term ⇒ givenVarsAsArgs.foldLeft(term) { case (prev, v) ⇒ AppE(prev, v) } }
  }

  def allOfTypeImpl[U: c.WeakTypeTag]: c.Tree = allOfTypeImplWithValues[U]()

  def allOfTypeImplWithValues[U: c.WeakTypeTag](values: c.Expr[Any]*): c.Tree = {
    val typeU: c.Type = c.weakTypeOf[U]
    val typeUT: TypeExpr[String] = buildTypeExpr(typeU)
    val givenVars: Seq[(PropE[String], c.Tree)] = values.zipWithIndex
      .map { case (v, i) ⇒ (PropE(s"arg${i + 1}", buildTypeExpr(v.actualType)), v.tree) }
    val givenVarsAsArgs = givenVars.map(_._1)
    val typeStructure = givenVarsAsArgs.reverse.foldLeft(typeUT) { case (prev, t) ⇒ t.tExpr ->: prev }
    inhabitAllInternal(typeStructure, givenVars.toMap) { term ⇒ givenVarsAsArgs.foldLeft(term) { case (prev, v) ⇒ AppE(prev, v) } }
  }

  /** Construct a Scala code tree that implements a type expression tree.
    * The implementation is chosen according to the "least information loss" principle.
    * It is an error if several inequivalent implementations are obtained.
    * The implemented code is logged to the console if required.
    *
    * @param typeStructure A type expression to be implemented.
    * @param givenArgs     Available Scala values that can be used while implementing the type.
    * @param transform     A final transformation for the implemented expression (e.g. apply it to some given arguments).
    * @return A Scala expression tree for the implemented expression.
    *         Will return `null` if the theorem prover fails to find a single "best" implementation.
    */
  private def inhabitOneInternal(
    typeStructure: TypeExpr[String],
    givenArgs: Map[PropE[String], c.Tree] = Map()
  )(
    transform: TermExpr[String] ⇒ TermExpr[String] = identity
  ): c.Tree = {
    TheoremProver.inhabitInternal(typeStructure) match {
      case Right((messageOpt, foundTerm)) ⇒
        messageOpt.foreach(message ⇒ c.warning(c.enclosingPosition, message))
        returnTerm(transform(foundTerm), givenArgs)
      case Left(errorMessage) ⇒
        c.error(c.enclosingPosition, errorMessage)
        q"null"
    }

  }

  private def returnTerm(termFound: TermExpr[String], givenArgs: Map[PropE[String], c.Tree]): c.Tree = {
    val prettyTerm = if (showReturningTerm) termFound.toString else termFound.prettyPrintWithParentheses(0)
    c.info(c.enclosingPosition, s"Returning term: $prettyTerm", force = true)
    val paramTerms: Map[PropE[String], c.Tree] = TermExpr.propositions(termFound).map(p ⇒ p → reifyParam(p)).toMap
    val result = reifyTerm(termFound, paramTerms, givenArgs)
    if (debug) c.info(c.enclosingPosition, s"Returning code: ${showCode(result)}", force = true)

    result
  }

  private def inhabitAllInternal(
    typeStructure: TypeExpr[String],
    givenArgs: Map[PropE[String], c.Tree]
  )(
    transform: TermExpr[String] ⇒ TermExpr[String]
  ): c.Tree = {
    val terms = TheoremProver.findProofs(typeStructure)._1.map { foundTerm ⇒
      returnTerm(transform(foundTerm), givenArgs)
    }
    q"Seq(..$terms)"
  }
}

object Macros {
  /** The JVM system property `curryhoward.log` can be set to a comma-separated list of words.
    * Each word must be one of `prover`, `macros`, or `terms`.
    *
    * @return The set of options defined by the user.
    */
  private[ch] def options: Set[String] = Option(System.getProperty("curryhoward.log")).getOrElse("").split(",").toSet

  private[ch] def testType[U]: (String, String) = macro Macros.testTypeImpl[U]

  private[ch] def testReifyType[U]: TypeExpr[String] = macro Macros.testReifyTypeImpl[U]

  // Not used.
  //  private[ch] def testReifyTerm[U]: List[TermExpr[String]] = macro Macros.testReifyTermsImpl[U]
}
