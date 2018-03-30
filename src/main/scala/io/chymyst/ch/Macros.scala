package io.chymyst.ch

import scala.language.experimental.macros
import io.chymyst.ch.Helper.AnyOpsEquals

import scala.reflect.macros.whitebox // Does not work with blackbox.

/* http://stackoverflow.com/questions/17494010/debug-scala-macros-with-intellij

How to debug Scala macros: The previous instructions do not work with IntelliJ 2017.

You need to start your sbt in debug mode and then connect the idea debugger remotely:

start sbt with: sbt -jvm-debug 5005

If this does not work, use the long form:

SBT_OPTS="-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005" sbt

Then create a "Remote" "Run/Debug Config" in IntelliJ (defaults to port 5005)

Open terminal and start sbt. Run `test:compile` to make sure everything is compiled.

Run the remote­debug config in IntelliJ using "Debug" button. That will connect it to your running sbt. Then you can set breakpoints in your macro code.

Now make a small change in your code and run compile in sbt, - IntelliJ should stop at the breakpoint.
 */

/*
- streamline LJT so that named conjunctions are directly converted to other sequents, not going through ConjunctT first.
- make sure the alpha-conversion is correct on type parameters! This seems to be a deeper problem.
- think about supporting recursive types beyond the palliative measures implemented currently
- use blackbox macros instead of whitebox if possible (5) ?? Seems to prevent the " = implement" and "= ofType[]" syntax from working.
- fix MatchE.simplify so that Option[A] ⇒ Option[A] can be simplified to identity
- decide whether we can implement a single function implement[]() that looks up the LHS when the given type is Nothing.
 */

class Macros(val c: whitebox.Context) {

  import c.universe._

  private def debug = Macros.options contains "macros"

  private def verbose = Macros.options contains "verbose"

  private def showReturningTerm = Macros.options contains "terms"

  /** Convert a Scala `Type` value into an expression tree of type TypeExpr.
    * This function performs further reflection steps recursively in order to detect sealed traits / case classes
    * or other nested types.
    *
    * There are two cases when this function is called:
    * 1) to build a TypeExpr from a given Scala type expression,
    * 2) recursively, to build a TypeExpr from c.Type information obtained via reflection - this is used for case class
    * accessors as well as for case classes discovered by reflection on a trait.
    * In the second case, we may need to rename some type parameters or replace them by other type expressions.
    *
    * @param givenType A `Type` value obtained via reflection. Can be a type alias, a case class, or a type expression.
    * @param tMap      A map of known substitutions for type parameters.
    * @param typesSeen Types of case classes or sealed traits that are possibly being recursed on.
    * @return Type expression corresponding to the type `t`, with correctly assigned type parameters.
    */
  private def buildTypeExpr(givenType: c.Type, tMap: Seq[(String, TypeExpr)] = Seq(), typesSeen: Set[String] = Set()): TypeExpr = {

    val finalType = givenType.dealias

    val finalTypeSymbol = finalType.typeSymbol

    val typeName = finalTypeSymbol.name.decodedName.toString

    val typesSeenNow = typesSeen + typeName

    // Could be the weird type [X, Y] => (type expression).
    // `finalResultType` seems to help here.
    val matchedTypeArgs = finalType.finalResultType.typeArgs
      .map(s ⇒ tMap.toMap.getOrElse(s.typeSymbol.name.decodedName.toString, buildTypeExpr(s, Seq(), typesSeen))) // `typesSeen` prevents infinite loops when  a type parameter is the same as the recursive type.
    // Very verbose; enable only when debugging the renaming of type parameters.
    //    if (debug) println(s"DEBUG: buildTypeExpr on $t with $tMap obtained args = ${t.finalResultType.typeArgs.mkString("; ")}")
    //    if (debug) println(s" DEBUG: buildTypeExpr obtained matchedTypeArgs = ${matchedTypeArgs.map(_.prettyPrint).mkString("; ")}")

    //    val typeParams = t.typeParams // This is nonempty only for the weird types mentioned above.
    //    val valParamLists = t.paramLists // typeU.paramLists(0)(0).name.decodedName is "x" as TermName; also typeU.finalResultType
    // use something like t.paramLists(0)(0).typeSignature and also .isImplicit to extract types of function parameters

    typeName match {
      case "scala.Function1" | "Function1" ⇒ matchedTypeArgs.head ->: matchedTypeArgs(1)
      case name if name matches "(scala\\.)?Function[1-9][0-9]*" ⇒
        ConjunctT(matchedTypeArgs.slice(0, matchedTypeArgs.length - 1)) ->: matchedTypeArgs.last
      case "scala.Any" | "Any" ⇒ BasicT("_")
      case "scala.Nothing" | "Nothing" ⇒ NothingT("Nothing")
      case "scala.Unit" | "Unit" ⇒ UnitT("Unit")
      case _ if matchedTypeArgs.isEmpty && finalType.baseClasses.map(_.fullName) === List("scala.Any") ⇒ TP(finalType.toString)

      case _ if typesSeen contains typeName ⇒ RecurseT(typeName, matchedTypeArgs)
      case _ if finalTypeSymbol.isClass ⇒
        // A type constructor, a case class, a sealed trait / abstract class with case classes, or other class.
        val typeMap: Seq[(String, TypeExpr)] = finalTypeSymbol.asClass.typeParams
          .map(_.name.decodedName.toString)
          .zip(matchedTypeArgs)

        // t.tpeCache.baseTypeSeqCache contains BTS(... BaseClass[U] ...)
        // t.typeSymbol.asClass.knownDirectSubclasses.toList(0).typeSignature.baseType(t.typeSymbol.asClass.knownDirectSubclasses.toList(0).typeSignature.baseClasses(5)) is BaseClass[U]
        // Very verbose; enable only when debugging the renaming of type parameters.
        //        if (debug) println(s" DEBUG: buildTypeExpr on $t with $tMap obtained typeMap = ${typeMap.map(_._2.prettyPrint).mkString("; ")}")

        if (finalTypeSymbol.asClass.isModuleClass) { // `case object` is a "module class", but also a "case class".
          NamedConjunctT(typeName, Nil, Nil, Nil) // This represents a case object.
        } else if (finalTypeSymbol.asClass.isCaseClass) {

          // Detected a case class with zero or more accessors.

          // Need to assign type parameters to the accessors.

          val (accessors, typeExprs) = finalType.decls
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
          val typeSymbol = if (finalTypeSymbol.isType) finalTypeSymbol.asType.toType.typeSymbol else finalTypeSymbol
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
                val baseType = s.typeSignature.baseType(finalTypeSymbol.asClass)
                // Substitute these type parameter names with the actual types used by the parent trait (as given by typeMap).
                val substMap: Map[String, TypeExpr] = baseType.typeArgs.map(_.typeSymbol.name.decodedName.toString).zip(typeMap.map(_._2)).toMap
                //                println(s"DEBUG: baseType = ${baseType.typeSymbol.name.decodedName.toString}, baseType.typeParams = ${baseType.typeArgs.map(_.typeSymbol.name.decodedName.toString)}, substMap = ${substMap.mapValues(_.prettyPrint)}")
                TypeExpr.substNames(subclassType, substMap)
              } else subclassType
            } match {
              case part :: Nil ⇒ part // A single case class implementing a trait. This is not a disjunction.
              case parts ⇒
                //                if (debug) println(s"  DEBUG: buildTypeExpr on $t with $tMap returning DisjunctT($fullName, ${matchedTypeArgs.map(_.prettyPrint)}), parts=${parts.map(_.prettyPrint)}")
                DisjunctT(typeName, matchedTypeArgs, parts.asInstanceOf[List[NamedConjunctT]]) // Several case classes implementing a trait.
            }
          } else if (matchedTypeArgs.isEmpty) BasicT(typeName)
          else ConstructorT(typeName, matchedTypeArgs)
        }
      case _ ⇒ ConstructorT(finalType.toString, Nil) // Sometimes we get <none> as the type symbol's name... Not sure what to do in that case.
    }
  }

  // These implicits are required for generating type expressions and term expressions as values.
  object LiftedAST {
    implicit def liftedTypeExpr: Liftable[TypeExpr] = Liftable[TypeExpr] {
      case DisjunctT(constructor, tParams, terms) ⇒ q"_root_.io.chymyst.ch.DisjunctT($constructor, Seq(..$tParams), Seq(..$terms))"
      case ConjunctT(terms) ⇒ q"_root_.io.chymyst.ch.ConjunctT(Seq(..$terms))"
      case #->(head, body) ⇒ q"_root_.io.chymyst.ch.#->($head, $body)"
      case NothingT(name) ⇒ q"_root_.io.chymyst.ch.NothingT($name)"
      case UnitT(name) ⇒ q"_root_.io.chymyst.ch.UnitT($name)"
      case TP(name) ⇒ q"_root_.io.chymyst.ch.TP($name)"
      case RecurseT(name, tParams) ⇒ q"_root_.io.chymyst.ch.RecurseT($name, Seq(..$tParams))"
      case BasicT(name) ⇒ q"_root_.io.chymyst.ch.BasicT($name)"
      case NamedConjunctT(constructor, tParams, accessors, wrapped) ⇒ q"_root_.io.chymyst.ch.NamedConjunctT($constructor, List(..$tParams), List(..$accessors), List(..$wrapped))"
      case ConstructorT(name, tParams) ⇒ q"_root_.io.chymyst.ch.ConstructorT($name, List(..$tParams))"
    }

    implicit def liftedTermExpr: Liftable[TermExpr] = Liftable[TermExpr] {
      case VarE(name, tExpr) ⇒ q"_root_.io.chymyst.ch.VarE($name, $tExpr)"
      case AppE(head, arg) ⇒ q"_root_.io.chymyst.ch.AppE($head, $arg)"
      case CurriedE(heads, body) ⇒ q"_root_.io.chymyst.ch.CurriedE(List(..$heads), $body)"
      case UnitE(tExpr) ⇒ q"_root_.io.chymyst.ch.UnitE($tExpr)"
      case NamedConjunctE(terms, tExpr) ⇒ q"_root_.io.chymyst.ch.NamedConjunctE(Seq(..$terms), $tExpr)"
      case ConjunctE(terms) ⇒ q"_root_.io.chymyst.ch.ConjunctE(Seq(..$terms))"
      case ProjectE(index, term) ⇒ q"_root_.io.chymyst.ch.ProjectE($index, $term)"
      case MatchE(term, cases) ⇒ q"_root_.io.chymyst.ch.MatchE($term, List(..$cases))"
      case DisjunctE(index, total, term, tExpr) ⇒ q"_root_.io.chymyst.ch.DisjunctE($index, $total, $term, $tExpr)"
    }

    implicit def liftedSubtypeOfTypeExpr[S <: TypeExpr]: Liftable[S] = Liftable[S] {
      s ⇒ liftedTypeExpr(s)
    }

    implicit def liftedSubtypeOfTermExpr[S <: TermExpr]: Liftable[S] = Liftable[S] {
      s ⇒ liftedTermExpr(s)
    }
  }

  // This is used to put types on all function arguments within emitParamCode().
  private def emitTypeCode(typeExpr: TypeExpr): c.Tree = {

    def makeTypeNameWithTypeParams(constructor: String, tParams: Seq[TypeExpr]): c.universe.Tree = {
      val constructorT = makeTypeName(constructor)
      if (tParams.isEmpty) constructorT else {
        val tParamsTrees = tParams.map(emitTypeCode)
        tq"$constructorT[..$tParamsTrees]"
      }
    }

    def makeTypeName(nameT: String): c.universe.Tree = {
      val tpn = TypeName(nameT)
      tq"$tpn"
    }

    typeExpr match {
      // Special case for Java-style arg lists.
      case ConjunctT(terms) #-> body ⇒ tq"(..${terms.map(emitTypeCode)}) ⇒ ${emitTypeCode(body)}"
      case head #-> body ⇒ tq"(${emitTypeCode(head)}) ⇒ ${emitTypeCode(body)}"
      case TP(nameT) ⇒ makeTypeName(nameT)
      case BasicT(nameT) ⇒ makeTypeName(nameT)
      case RecurseT(constructor, tParams) ⇒ makeTypeNameWithTypeParams(constructor, tParams)

      case NamedConjunctT(constructor, tParams, _, _) ⇒
        val constructorT = makeTypeName(constructor)
        val constructorWithTypeParams = makeTypeNameWithTypeParams(constructor, tParams)
        if (typeExpr.caseObjectName.isDefined) {
          tq"$constructorT.type" // case object
        } else constructorWithTypeParams
      case NothingT(nameT) ⇒ makeTypeName(nameT)
      case UnitT(nameT) ⇒ makeTypeName(nameT)
      case ConjunctT(terms) ⇒ // Assuming this is a tuple type.
        val tpts = terms.map(emitTypeCode)
        tq"(..$tpts)"
      case DisjunctT(constructor, tParams, _) ⇒ makeTypeNameWithTypeParams(constructor, tParams)
      case ConstructorT(constructor, tParams) ⇒ makeTypeNameWithTypeParams(constructor, tParams)
    }
  }

  // Prepare the tree for a function parameter with the specified type.
  private def emitParamCode(term: VarE): c.Tree = term match {
    case VarE(name, typeExpr) ⇒
      val tpt = emitTypeCode(typeExpr)
      val termName = TermName(name.toString)
      val param = q"val $termName: $tpt"
      param
  }

  private def emitTermCode(termExpr: TermExpr, givenArgs: Map[VarE, c.Tree]): c.Tree = {
    // Shortcut for calling this function recursively with all the same arguments.
    def emitTermShort(termExpr: TermExpr): c.Tree = emitTermCode(termExpr, givenArgs)

    def conjunctSubstName(p: VarE, i: Int): String = s"${p.name}_$i"

    termExpr match {
      case p@VarE(name, _) ⇒ givenArgs.getOrElse(p, q"${TermName(name.toString)}")

      // A function applied to several arguments Java-style.
      case AppE(head, ConjunctE(terms)) ⇒ q"${emitTermShort(head)}(..${terms.map(emitTermShort)})"

      case AppE(head, arg) ⇒ q"${emitTermShort(head)}(${emitTermShort(arg)})"

      // If `heads` = List(x, y, z) and `body` = b then the generated code will be x ⇒ y ⇒ z ⇒ b.
      case CurriedE(heads, body) ⇒
        // If one of the heads is a ConjunctT, we need to do a replacement in the entire body.
        // It will be too late to do this once we start emitting code for the terms.
        val conjunctHeads = heads.collect { case p@VarE(_, ConjunctT(terms)) ⇒ (p, terms) }
        val replacedBody = conjunctHeads.foldLeft(body) {
          case (prev, (p, termTypes)) ⇒ TermExpr.subst(
            p,
            ConjunctE(termTypes.zipWithIndex.map { case (t, i) ⇒ VarE(conjunctSubstName(p, i), t) }),
            prev
          ).simplifyOnce(withEta = false)
        }.simplifyOnce(withEta = true)

        // Need to be careful to substitute arguments that are of type ConjunctT, because they represent Java-style arg groups.
        heads.reverse.foldLeft(emitTermShort(replacedBody)) { case (prevTree, paramE) ⇒
          conjunctHeads.find(_._1 === paramE) match {
            case Some((_, terms)) ⇒
              val args = terms.zipWithIndex.map { case (t, i) ⇒ emitParamCode(VarE(conjunctSubstName(paramE, i), t)) }
              q"((..$args) ⇒ $prevTree)"
            case None ⇒ q"(${emitParamCode(paramE)} ⇒ $prevTree)"
          }
        }

      case UnitE(_) ⇒ q"()"
      case ConjunctE(terms) ⇒ q"(..${terms.map(emitTermShort)})"
      case NamedConjunctE(terms, tExpr) ⇒
        val constructorE = q"${TermName(tExpr.constructor)}[..${tExpr.typeParams.map(emitTypeCode)}]"
        if (tExpr.isAtomic)
          constructorE // avoid spurious parameter lists for case objects
        else
          q"$constructorE(..${terms.map(emitTermShort)})"
      case ProjectE(index, term) ⇒
        val accessor = TermName(term.accessor(index))
        q"${emitTermShort(term)}.$accessor"
      case DisjunctE(_, _, term, _) ⇒ q"${emitTermShort(term)}" // A disjunct term is always a NamedConjunctE, so we just emit that code.
      case MatchE(term, cases) ⇒
        // Each term within `cases` is always a CurriedE because it is of the form fv ⇒ TermExpr(fv, other_premises).
        val casesTrees: Seq[c.Tree] = cases.map {
          // However, at this point a MatchE() could contain a CurriedE() with multiple arguments,
          // as a result of possibly simplifying nested CurriedE().
          case CurriedE(VarE(fvName, fvType) :: rest, body) ⇒
            // Generate cq"$pat => $expr" where pat = pq"Constructor(..$Strings)".
            val pat = fvType.caseObjectName match {
              case Some(constructor) ⇒ pq"${TermName(fvName)} : ${TermName(constructor)}.type" // used to be pq"_ : ${TermName(constructor)}.type"
              case None ⇒ pq"${TermName(fvName)} : ${emitTypeCode(fvType)}"
            }
            cq"$pat => ${emitTermShort(if (rest.isEmpty) body else CurriedE(rest, body))}"
          case cc ⇒ throw new Exception(s"Internal error: `case` term ${cc.prettyRenamePrint} must be a function")
        }

        q"${emitTermShort(term)} match { case ..$casesTrees }"
    }
  }

  def freshVarImpl[U: c.WeakTypeTag]: c.Expr[VarE] = {
    val typeU: c.Type = c.weakTypeOf[U]
    val typeUExpr = buildTypeExpr(typeU)
    if (debug) c.info(c.enclosingPosition, s"Built type expression ${typeUExpr.prettyPrint} from type $typeU", force = true)
    val leftSide = c.internal.enclosingOwner.name.decodedName.toString
    val ident = c.freshName(leftSide).replaceAll(" *\\$macro", "")
    import LiftedAST._
    c.Expr[VarE](q"VarE($ident, $typeUExpr)")
  }

  // This function is for testing buildTypeExpr().
  def testTypeImpl[T: c.WeakTypeTag]: c.Expr[(String, String)] = {
    val typeT: c.Type = c.weakTypeOf[T]
    val enclosingType = c.internal.enclosingOwner.typeSignature

    val s1 = buildTypeExpr(typeT).prettyPrint
    val s2 = buildTypeExpr(enclosingType).prettyPrint

    c.Expr[(String, String)](q"($s1,$s2)")
  }

  // Obtain one implementation of the type U. Detect the type U as given on the left-hand side.
  def inhabitImpl[U]: c.Tree = {
    val typeU = c.internal.enclosingOwner.typeSignature
    // Detect whether we are given a function with arguments.
    val result = typeU.paramLists match {
      case Nil ⇒ inhabitOneInternal(buildTypeExpr(typeU))()
      case lists ⇒
        val givenVars = lists.flatten.map(s ⇒ VarE(s.name.decodedName.toString, buildTypeExpr(s.typeSignature)))
        val resultType = buildTypeExpr(typeU.finalResultType)
        val typeStructure = givenVars.reverse.foldLeft(resultType) { case (prev, t) ⇒ t.t ->: prev }
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
    val givenVars: Seq[(VarE, c.Tree)] = values.zipWithIndex
      // The given values will be marked as arguments with name `arg1`, `arg2`, etc.,
      // so that they do not mix with automatic variables in the generated closed term.
      .map { case (v, i) ⇒ (VarE(s"arg${i + 1}", buildTypeExpr(v.actualType)), v.tree) }
    val givenVarsAsArgs = givenVars.map(_._1)
    val typeStructure = givenVarsAsArgs.reverse.foldLeft(typeUT) { case (prev, t) ⇒ t.t ->: prev }
    inhabitOneInternal(typeStructure, givenVars.toMap) { term ⇒
      givenVarsAsArgs.foldLeft(term) { case (prev, v) ⇒ AppE(prev, v) }
    }
  }

  def allOfTypeImpl[U: c.WeakTypeTag]: c.Tree = allOfTypeImplWithValues[U]()

  def allOfTypeImplWithValues[U: c.WeakTypeTag](values: c.Expr[Any]*): c.Tree = {
    val typeUT: TypeExpr = buildTypeExpr(c.weakTypeOf[U])
    val givenVars: Seq[(VarE, c.Tree)] = values.zipWithIndex
      .map { case (v, i) ⇒ (VarE(s"arg${i + 1}", buildTypeExpr(v.actualType)), v.tree) }
    val givenVarsAsArgs = givenVars.map(_._1)
    val typeStructure = givenVarsAsArgs.reverse.foldLeft(typeUT) { case (prev, t) ⇒ t.t ->: prev }
    inhabitAllInternal(typeStructure, givenVars.toMap) { term ⇒ givenVarsAsArgs.foldLeft(term) { case (prev, v) ⇒ AppE(prev, v) } }
  }

  def anyOfTypeImplWithValues[U: c.WeakTypeTag](values: c.Expr[Any]*): c.Tree = {
    val typeUT: TypeExpr = buildTypeExpr(c.weakTypeOf[U])
    val givenVars: Seq[(VarE, c.Tree)] = values.zipWithIndex
      .map { case (v, i) ⇒ (VarE(s"arg${i + 1}", buildTypeExpr(v.actualType)), v.tree) }
    val givenVarsAsArgs = givenVars.map(_._1)
    val typeStructure = givenVarsAsArgs.reverse.foldLeft(typeUT) { case (prev, t) ⇒ t.t ->: prev }
    inhabitAllInternal(typeStructure, givenVars.toMap, returnAllProofTerms = true) { term ⇒ givenVarsAsArgs.foldLeft(term) { case (prev, v) ⇒ AppE(prev, v) } }
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
    typeStructure: TypeExpr,
    givenArgs: Map[VarE, c.Tree] = Map()
  )(
    transform: TermExpr ⇒ TermExpr = identity
  ): c.Tree = {
    TheoremProver.inhabitInternal(typeStructure) match {
      case Right((messageOpt, foundTerm)) ⇒
        messageOpt.foreach(message ⇒ if (verbose) c.warning(c.enclosingPosition, message))
        returnTerm(transform(foundTerm), givenArgs)
      case Left(errorMessage) ⇒
        c.error(c.enclosingPosition, errorMessage)
        q"null"
    }

  }

  private def returnTerm(termFound: TermExpr, givenArgs: Map[VarE, c.Tree]): c.Tree = {
    import LiftedAST._
    val prettyTerm = if (showReturningTerm) termFound.toString else termFound.prettyPrintWithParentheses(0)
    if (verbose) c.info(c.enclosingPosition, s"Returning term: $prettyTerm", force = true)
    val resultCodeTree = emitTermCode(termFound, givenArgs)
    // Attach lambda-terms to functions when appropriate.
    val result = {
      lazy val wrapper = q"object Wrapper { val expr = $termFound }"
      termFound match {
        case CurriedE(VarE(_, ConjunctT(termTypes)) :: _, _) ⇒
          if (termTypes.length <= 3) {
            val functionName = TypeName(s"Function${termTypes.length}Lambda")
            val functionNameType = tq"$functionName"
            q"$wrapper; new $functionNameType($resultCodeTree, Wrapper.expr)"
          } else resultCodeTree
        case CurriedE(VarE(_, _) :: _, _) ⇒ q"$wrapper; new _root_.io.chymyst.ch.Function1Lambda($resultCodeTree, Wrapper.expr)"
        case _ ⇒ resultCodeTree
      }
    }
    if (debug) c.info(c.enclosingPosition, s"Returning code: ${showCode(result)}", force = true)
    result
  }

  private def inhabitAllInternal(
    typeStructure: TypeExpr,
    givenArgs: Map[VarE, c.Tree],
    returnAllProofTerms: Boolean = false
  )(
    transform: TermExpr ⇒ TermExpr
  ): c.Tree = {
    val (lowestScoreTerms, allTerms) = TheoremProver.findProofs(typeStructure)
    val foundTerms = if (returnAllProofTerms) {
      if (debug) allTerms.foreach { t ⇒ c.info(c.enclosingPosition, s"[DEBUG] Score: ${t.informationLossScore}\tTerm: ${t.prettyPrint}", force = true) }
      allTerms
    } else {
      lowestScoreTerms
    }
    val terms = foundTerms.map { foundTerm ⇒
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

  // These methods are for testing only.
  private[ch] def testType[U]: (String, String) = macro Macros.testTypeImpl[U]
}
