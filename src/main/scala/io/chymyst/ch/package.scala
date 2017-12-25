package io.chymyst

import scala.language.experimental.macros

package object ch {

  def ofType[U]: U = macro CurryHowardMacros.ofTypeImpl[U]

  def implement[U]: U = macro CurryHowardMacros.inhabitImpl[U]
  def allOfType[U]: Seq[U] = macro CurryHowardMacros.allOfTypeImpl[U]
}
