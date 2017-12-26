package io.chymyst

import scala.language.experimental.macros

package object ch {

  def ofType[U]: U = macro CurryHowardMacros.ofTypeImpl[U]

  def implement[U]: U = macro CurryHowardMacros.inhabitImpl[U]

  def implement1[U]: U = macro CurryHowardMacros.inhabitImpl1[U]

  def allOfType[U]: Seq[U] = macro CurryHowardMacros.allOfTypeImpl[U]

  def toType[U](values: Any*): U = macro CurryHowardMacros.toTypeImpl[U]
}
