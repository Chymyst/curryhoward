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

// TODO:
/*  Priority is given in parentheses.
- use c.Type and c.Tree instead of String for correct code generation (3)?? Probably impossible since we can't reify types directly from reflection results, - need to use names.
- probably can simplify data structures by eliminating [T], VarName, and ProofTerm
- use blackbox macros instead of whitebox if possible (5) ?? Seems to prevent the " = implement" and "= ofType[]" syntax from working.
- implement uncurried functions and multiple argument lists (6)?? Not sure this helps. We can already do this with `implement`.
- use a special subclass of Function1 that also carries symbolic information about the lambda-term (6)

 */

// TODO: can we replace this with blackbox? So far this only works with whitebox.
class Macros(val c: whitebox.Context) {

  import c.universe._

  private def debug = Macros.options contains "macros"

  private def showReturningTerm = Macros.options contains "terms"

  private val basicRegex = s"(?:scala.|java.lang.)*(${Macros.basicTypes.mkString("|")})".r

  // TODO: use c.Type instead of String -- ??? Not sure this is ever going to work.
  private def matchType(t: c.Type, tMap: Map[String, TypeExpr[String]] = Map()): TypeExpr[String] = {

    // Could be the weird type [X, Y] => (type expression).
    // `finalResultType` seems to help here.
    val matchedTypeArgs = t.finalResultType.typeArgs.map(s ⇒ tMap.getOrElse(s.typeSymbol.name.decodedName.toString, matchType(s)))
    // Very verbose.
    //    if (debug) if (tMap.nonEmpty) println(s"DEBUG: matchType on $t with $tMap obtained args = $args")
    //    if (debug) if (tMap.nonEmpty) println(s"DEBUG: matchType obtained matchedTypeArgs = ${matchedTypeArgs.map(_.prettyPrint)}")

    //    val typeParams = t.typeParams // This is nonempty only for the weird types mentioned above.
    //    val valParamLists = t.paramLists // typeU.paramLists(0)(0).name.decodedName is "x" as TermName; also typeU.finalResultType
    // use something like t.paramLists(0)(0).typeSignature and also .isImplicit to extract types of function parameters
    t.typeSymbol.name.decodedName.toString match {
      case name if name matches "(scala\\.)?Tuple[0-9]+" ⇒ ConjunctT(matchedTypeArgs)
      case "scala.Function1" | "Function1" ⇒ matchedTypeArgs.head ->: matchedTypeArgs(1)
      case "scala.Any" | "Any" ⇒ OtherT("_")
      case "scala.Nothing" | "Nothing" ⇒ NothingT("Nothing")
      case "scala.Unit" | "Unit" ⇒ UnitT("Unit")
      case basicRegex(name) ⇒ BasicT(name)
      case _ if matchedTypeArgs.isEmpty && t.baseClasses.map(_.fullName) == Seq("scala.Any") ⇒ TP(t.toString)
      //      case fullName if t.typeSymbol.isType && t.typeSymbol.asType.isAliasType ⇒ ??? // Not sure this ever helps.
      case fullName if t.typeSymbol.isClass ⇒
        val typeMap: Map[String, TypeExpr[String]] = t.typeSymbol.asClass.typeParams
          .map(_.name.decodedName.toString)
          .zip(matchedTypeArgs)
          .toMap

        if (t.typeSymbol.asClass.isModuleClass) { // `case object` is a "module class", but also a "case class".
          NamedConjunctT(fullName, Nil, Nil, Nil)
        } else if (t.typeSymbol.asClass.isCaseClass) {
          // Case class.

          // Need to assign type parameters to the accessors.

          //  t.decls.toList(0).asMethod.typeSignature.resultType gives A, t.typeSymbol.asClass.typeParams(0) ==  t.decls.toList(0).asMethod.typeSignature.resultType.typeSymbol
          /* t.typeSymbol.asClass.typeParams gives List(A, B)*/
          val (accessors, typeExprs) = t.decls
            .collect { case s: MethodSymbol if s.isCaseAccessor ⇒
              val accessorType = matchType(s.typeSignature.resultType)
              val substitutedType = TypeExpr.substAll(accessorType, typeMap)
              (s.name.decodedName.toString, substitutedType)
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
          } else if (matchedTypeArgs.isEmpty) OtherT(fullName)
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

  private def reifyTerm(termExpr: TermExpr[String], paramTerms: Map[PropE[String], c.Tree], givenTerms: Map[PropE[String], c.Tree]): c.Tree = {
    def reifyT(termExpr: TermExpr[String]): c.Tree = reifyTerm(termExpr, paramTerms, givenTerms)

    termExpr match {
      case p@PropE(name, _) ⇒ givenTerms.getOrElse(p, q"${TermName(name.toString)}")

      case AppE(head, arg) ⇒ q"${reifyT(head)}(${reifyT(arg)})"

      // If `heads` = List(x, y, z) and `body` = b then the code must be x => y => z => b
      case CurriedE(heads, body) ⇒ heads.reverse.foldLeft(reifyT(body)) { case (prevTree, paramE) ⇒
        val param = paramTerms(paramE) // Look up the parameter in the precomputed table.
        q"($param ⇒ $prevTree)"
      }
      case UnitE(_) ⇒ q"()"
      case ConjunctE(terms) ⇒ q"(..${terms.map(t ⇒ reifyT(t))})"
      case NamedConjunctE(terms, tExpr) ⇒
        val constructorE = q"${TermName(tExpr.constructor)}[..${tExpr.tParams.map(reifyType)}]"
        if (tExpr.isAtomic)
          constructorE // avoid spurious parameter lists for case objects
        else
          q"$constructorE(..${terms.map(t ⇒ reifyT(t))})"
      case ProjectE(index, term) ⇒
        val accessor = TermName(term.accessor(index))
        q"${reifyT(term)}.$accessor"
      case DisjunctE(_, _, term, _) ⇒ q"${reifyT(term)}" // A disjunct term is always a NamedConjunctE, so we just reify that.
      case MatchE(term, cases) ⇒
        // Each term within `cases` is always a CurriedE because it is of the form fv ⇒ proofTerm(fv, other_premises).
        val casesTrees: Seq[c.Tree] = cases.map {
          case CurriedE(PropE(fvName, fvType) :: _, body) ⇒
            // cq"$pat => $expr" where pat = pq"Constructor(..$varNames)"
            val pat = fvType.caseObjectName match {
              case Some(constructor) ⇒ pq"_ : ${TermName(constructor)}.type"
              case None ⇒ pq"${TermName(fvName)} : ${reifyType(fvType)}"
            }
            cq"$pat => ${reifyT(body)}"
          case cc ⇒ throw new Exception(s"Internal error: `case` term ${cc.prettyPrint} must be a function")
        }

        q"${reifyT(term)} match { case ..$casesTrees }"
    }
  }

  def testReifyTypeImpl[U: c.WeakTypeTag]: c.Expr[TypeExpr[String]] = {
    val typeU: c.Type = c.weakTypeOf[U]
    val result = matchType(typeU.resultType)
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

  // Obtain one implementation of the type U. Detect the type U as given on the left-hand side.
  def inhabitImpl[U]: c.Tree = {
    val typeU = c.internal.enclosingOwner.typeSignature
    // Detect whether we are given a function with arguments.
    val result = typeU.resultType.paramLists match {
      case Nil ⇒ inhabitOneInternal(matchType(typeU))()
      case lists ⇒
        val givenVars = lists.flatten.map(s ⇒ PropE(s.name.decodedName.toString, matchType(s.typeSignature)))
        val resultType = matchType(typeU.finalResultType)
        val typeStructure = givenVars.reverse.foldLeft(resultType) { case (prev, t) ⇒ t.tExpr ->: prev }
        inhabitOneInternal(typeStructure) { term ⇒ givenVars.foldLeft(term) { case (prev, v) ⇒ AppE(prev, v) }.simplify() }
    }
    result
  }

  // Obtain one implementation of the type U, which must be given as the type parameter.
  // This function does not attempt to detect the left-hand side type,
  // and so does not give rise to the "recursive value must have type" error.
  def ofTypeImpl[U: c.WeakTypeTag]: c.Tree = ofTypeImplWithValues[U]()

  def ofTypeImplWithValues[U: c.WeakTypeTag](values: c.Expr[Any]*): c.Tree = {
    val typeUGiven: c.Type = c.weakTypeOf[U]
    val typeUT = matchType(typeUGiven)

    // TODO: decide if we can still use `ofType` with auto-detection of left-hand side values.
    // If so, we can stop using two different names for these use cases.
    // This does not seem to work in a macro with arguments! (But it might for the version of `ofTypeImpl` without arguments.)
    //    match {
    //      case NothingT(_) ⇒
    //        val typeU = c.internal.enclosingOwner.typeSignature.finalResultType
    //        matchType(typeU)
    //      case t ⇒ t
    //    }

    // We need to obtain the actual type of the given terms, rather than the `Any` type as specified in the type signature.
    val givenVars: Seq[(PropE[String], c.Tree)] = values.zipWithIndex
      .map { case (v, i) ⇒ (PropE(s"arg${i + 1}", matchType(v.actualType)), v.tree) }
    val givenVarsAsArgs = givenVars.map(_._1)
    val typeStructure = givenVarsAsArgs.reverse.foldLeft(typeUT) { case (prev, t) ⇒ t.tExpr ->: prev }
    inhabitOneInternal(typeStructure, givenVars.toMap) { term ⇒ givenVarsAsArgs.foldLeft(term) { case (prev, v) ⇒ AppE(prev, v) }.simplify() }
  }

  def allOfTypeImpl[U: c.WeakTypeTag]: c.Tree = allOfTypeImplWithValues[U]()

  def allOfTypeImplWithValues[U: c.WeakTypeTag](values: c.Expr[Any]*): c.Tree = {
    val typeU: c.Type = c.weakTypeOf[U]
    val typeUT: TypeExpr[String] = matchType(typeU)
    val givenVars: Seq[(PropE[String], c.Tree)] = values.zipWithIndex
      .map { case (v, i) ⇒ (PropE(s"arg${i + 1}", matchType(v.actualType)), v.tree) }
    val givenVarsAsArgs = givenVars.map(_._1)
    val typeStructure = givenVarsAsArgs.reverse.foldLeft(typeUT) { case (prev, t) ⇒ t.tExpr ->: prev }
    inhabitAllInternal(typeStructure, givenVars.toMap) { term ⇒ givenVarsAsArgs.foldLeft(term) { case (prev, v) ⇒ AppE(prev, v) }.simplify() }
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
    */
  private def inhabitOneInternal(
    typeStructure: TypeExpr[String],
    givenArgs: Map[PropE[String], c.Tree] = Map()
  )(
    transform: TermExpr[String] ⇒ TermExpr[String] = identity
  ): c.Tree = {
    TheoremProver.inhabitInternal(typeStructure) match {
      case Right((messageOpt, t)) ⇒
        messageOpt.foreach(message ⇒ c.warning(c.enclosingPosition, message))
        val termFound = transform(t)
        if (showReturningTerm) c.info(c.enclosingPosition, s"Returning term: ${termFound.prettyPrintWithParentheses(0)}", force = true)
        val paramTerms: Map[PropE[String], c.Tree] = TermExpr.propositions(termFound).map(p ⇒ p → reifyParam(p)).toMap
        val result = reifyTerm(termFound, paramTerms, givenArgs)
        if (debug) c.info(c.enclosingPosition, s"Returning code: ${showCode(result)}", force = true)
        result
      case Left(errorMessage) ⇒
        c.error(c.enclosingPosition, errorMessage)
        q"null"
    }

  }

  private def inhabitAllInternal(
    typeStructure: TypeExpr[String],
    givenTerms: Map[PropE[String], c.Tree]
  )(
    transform: TermExpr[String] ⇒ TermExpr[String]
  ): c.Tree = {
    val terms = TheoremProver.findProofs(typeStructure)._1.map { term ⇒
      val termFound = transform(term)
      if (showReturningTerm) c.info(c.enclosingPosition, s"Returning term: ${termFound.prettyPrint}", force = true)
      val paramTerms: Map[PropE[String], c.Tree] = TermExpr.propositions(termFound).map(p ⇒ p → reifyParam(p)).toMap
      val result = reifyTerm(termFound, paramTerms, givenTerms)
      if (debug) c.info(c.enclosingPosition, s"Returning code: ${showCode(result)}", force = true)

      result
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

  private[ch] val basicTypes = List("Int", "String", "Boolean", "Float", "Double", "Long", "Symbol", "Char")

  private[ch] def testType[U]: (String, String) = macro Macros.testTypeImpl[U]

  private[ch] def testReifyType[U]: TypeExpr[String] = macro Macros.testReifyTypeImpl[U]

  private[ch] def testReifyTerm[U]: List[TermExpr[String]] = macro Macros.testReifyTermsImpl[U]
}
