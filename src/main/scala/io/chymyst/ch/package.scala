package io.chymyst

import scala.language.experimental.macros

package object ch {
  /** Automatically implement an expression of a given type.
    * The type parameter `U` _must_ be specified.
    *
    * @tparam U Type of the expression to be implemented.
    * @return An automatically constructed expression of type `U`.
    */
  def ofType[U]: U = macro Macros.ofTypeImpl[U]

  /** Automatically implement an expression of a given type.
    * The type parameter `U` _does not_ need to be specified.
    * The required type parameter `U` and other available terms and type parameters will be detected automatically from the left-hand side.
    *
    * Example usage:
    *
    * {{{
    *   def f[A,B](x: A ⇒ B, y: B ⇒ A): A ⇒ A = implement
    * }}}
    *
    * @tparam U Type of the expression to be implemented. Specifying this type parameter is optional.
    *           If it is not specified, it will be detected automatically by examining the left-hand side of the definition.
    * @return An automatically constructed expression of type `U`.
    */
  def implement[U]: U = macro Macros.inhabitImpl[U]

  /** Automatically implement an expression of a given type, and return all inequivalent implementations
    * that have the lowest information loss score.
    * The type parameter `U` _must_ be specified.
    *
    * @tparam U Type of the expression to be implemented.
    * @return A sequence of all possible inequivalent automatically constructed expressions of type `U`.
    */
  def allOfType[U]: Seq[U] = macro Macros.allOfTypeImpl[U]

  /** Automatically implement an expression of a given type using given values, and return all inequivalent implementations
    * that have the lowest information loss score.
    * The type parameter `U` _must_ be specified.
    *
    * @tparam U Type of the expression to be implemented.
    * @param values Zero or more expressions that may be used while implementing the type.
    * @return A sequence of all possible inequivalent automatically constructed expressions of type `U`.
    */
  def allOfType[U](values: Any*): Seq[U] = macro Macros.allOfTypeImplWithValues[U]

  /** Automatically implement an expression of a given type, using given values.
    * The type parameter `U` _must_ be specified.
    *
    * Example usage:
    *
    * {{{
    *   val x: A ⇒ B = ...
    *   val y: B ⇒ C = ...
    *   val z = ofType[A ⇒ C](x, y)
    * }}}
    *
    * @param values Available values that can be used while constructing the expression.
    * @tparam U Type of the expression to be implemented.
    * @return An automatically constructed expression of type `U`.
    */
  def ofType[U](values: Any*): U = macro Macros.ofTypeImplWithValues[U]

  /** Obtain the lambda-term from an enriched expression that results from `ofType`.
    * Will throw an exception if the expression is not obtained from `ofType` or `allOfType`.
    *
    * @param functionValue An expression that was automatically produced by `ofType` or `allOfType`.
    */
  implicit class WithLambdaTerm(val functionValue: Any) extends AnyVal {
    def lambdaTerm: TermExpr = functionValue match {
      case g: Function0Lambda[_] ⇒ g.lambdaTerm
      case g: Function1Lambda[_, _] ⇒ g.lambdaTerm
      case g: Function2Lambda[_, _, _] ⇒ g.lambdaTerm
      case g: Function3Lambda[_, _, _, _] ⇒ g.lambdaTerm
      case _ ⇒ throw new Exception(s"Called `.lambdaTerm` on an expression $functionValue that has no attached lambda-term")
    }
  }

  /** Provide syntax for term operations.
    */
  implicit class WithAlphaConversion(val termExpr: TermExpr) extends AnyVal {
    /** Provide :@ syntax for term application with automatic alpha-conversions.
      */
    def :@(terms: TermExpr*): TermExpr = termExpr.applyWithAlpha(terms: _*)

    /** Provide syntax for function composition.
      */
    def andThen(otherTerm: TermExpr): TermExpr = (termExpr.t, otherTerm.t) match {
      case (#->(head1, body1), #->(head2, body2)) ⇒
        if (head2 == body1) {
          val var1 = VarE(TheoremProver.freshVar(), head1)
          var1 =>: otherTerm(termExpr(var1))
        } else throw new Exception(s"Call to `.andThen` is invalid because the function types (${termExpr.t} and ${otherTerm.t}) do not match")
      case _ ⇒ throw new Exception(s"Call to `.andThen` is invalid because the type of one of the arguments (${termExpr.t} and ${otherTerm.t}) is not of a function type")
    }
  }

  /** Create a new fresh variable term of given type.
    *
    * @tparam X Type expression that will be assigned to the new variable.
    * @return A new variable.
    */
  def freshVar[X]: VarE = macro Macros.freshVarImpl[X]

}

// Note: for some reason, a macro with arguments cannot properly infer types.
// Cannot have e.g. `val x: Int => Int = implement()` even with an empty list of arguments.
// It infers `Nothing` as the _correct_ type of the expression `x` for some reason!
// However `val x: Int => Int = implement` works, when `implement` is a zero-argument method. This is very odd.
