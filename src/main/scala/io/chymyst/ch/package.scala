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

  /** Obtain a list of lambda-terms implementing a given type expression.
    *
    * @tparam U The type expression to be implemented.
    * @return A list of abstract syntax trees representing the (typed) lambda-terms,
    *         each implementing the given type `U`.
    */
  def lambdaTerms[U]: List[TermExpr[String]] = macro Macros.testReifyTermsImpl[U]
}

// Note: for some reason, a macro with arguments cannot properly infer types.
// Cannot have e.g. `val x: Int => Int = implement()` even with an empty list of arguments.
// It infers `Nothing` as the _correct_ type of the expression `x` for some reason!
// However `val x: Int => Int = implement` works, when `implement` is a zero-argument method. This is very odd.
