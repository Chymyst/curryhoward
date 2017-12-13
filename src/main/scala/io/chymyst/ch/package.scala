package io.chymyst

import io.chymyst.ch.CurryHowardMacros.{inhabitImpl, ofTypeImpl}

import scala.language.experimental.macros

package object ch {

  def ofType[T]: T = macro ofTypeImpl[T]

  def implement[T]: T = macro inhabitImpl[T]
}
