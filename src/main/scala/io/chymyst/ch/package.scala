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
    *   def f[A,B](x: A => B, y: B => A): A => A = implement
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

  /** Automatically implement an expression of a given type using given values, and return all inequivalent implementations
    * regardless of their information loss score.
    * The type parameter `U` _must_ be specified.
    *
    * @tparam U Type of the expression to be implemented.
    * @param values Zero or more expressions that may be used while implementing the type.
    * @return A sequence of all possible inequivalent automatically constructed expressions of type `U`.
    */
  def anyOfType[U](values: Any*): Seq[U] = macro Macros.anyOfTypeImplWithValues[U]

  /** Automatically implement an expression of a given type, using given values.
    * The type parameter `U` _must_ be specified.
    *
    * Example usage:
    *
    * {{{
    *   val x: A => B = ...
    *   val y: B => C = ...
    *   val z = ofType[A => C](x, y)
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
      case g: Function0Lambda[_] => g.lambdaTerm
      case g: Function1Lambda[_, _] => g.lambdaTerm
      case g: Function2Lambda[_, _, _] => g.lambdaTerm
      case g: Function3Lambda[_, _, _, _] => g.lambdaTerm
      case _ => throw new Exception(s"Called `.lambdaTerm` on an expression $functionValue that has no attached lambda-term")
    }
  }

  /** Create a new fresh variable term of given type.
    *
    * @tparam X Type expression that will be assigned to the new variable.
    * @return A new variable.
    */
  def freshVar[X]: VarE = macro Macros.freshVarImpl[X]

  /** Construct a lambda-calculus type expression of a specified Scala type.
    *
    * Example usage:
    *
    * {{{
    *   val tInt = typeExpr[Int]
    *   def idAType[A] = typeExpr[A => A]
    * }}}
    *
    * @tparam U The Scala type for which the type expression is requested. Can use type parameters.
    * @return A [[TypeExpr]] corresponding to the given Scala type.
    */
  def typeExpr[U]: TypeExpr = macro Macros.typeExprImpl[U]
}

// Note: for some reason, a macro with arguments cannot properly infer types.
// Cannot have e.g. `val x: Int => Int = implement()` even with an empty list of arguments.
// It infers `Nothing` as the _correct_ type of the expression `x` for some reason!
// However `val x: Int => Int = implement` works, when `implement` is a zero-argument method. This is very odd.
