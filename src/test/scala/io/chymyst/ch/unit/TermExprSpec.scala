package io.chymyst.ch.unit

import io.chymyst.ch._
import org.scalatest.{FlatSpec, Matchers}

class TermExprSpec extends FlatSpec with Matchers {

  behavior of "TermExpr"

  val termExpr1 = CurriedE(List(PropE("x2", TP(3)), PropE("x3", TP(2)), PropE("x4", TP(1))), PropE("x3", TP(2)))
  val termExpr2 = CurriedE(List(PropE("x2", TP(3)), PropE("x3", TP(2)), PropE("x4", TP(1))), PropE("x1", TP(2)))
  val termExpr3 = CurriedE(List(PropE("x1", TP(2))), termExpr2)

  it should "rename one variable" in {
    termExpr1.renameVar("x2", "y2") shouldEqual CurriedE(List(PropE("y2", TP(3)), PropE("x3", TP(2)), PropE("x4", TP(1))), PropE("x3", TP(2)))
    termExpr1.renameVar("x3", "y3") shouldEqual CurriedE(List(PropE("x2", TP(3)), PropE("y3", TP(2)), PropE("x4", TP(1))), PropE("y3", TP(2)))
    termExpr2.renameVar("x1", "y1") shouldEqual CurriedE(List(PropE("x2", TP(3)), PropE("x3", TP(2)), PropE("x4", TP(1))), PropE("y1", TP(2)))
    termExpr3.renameVar("x1", "y1") shouldEqual CurriedE(List(PropE("y1", TP(2))), CurriedE(List(PropE("x2", TP(3)), PropE("x3", TP(2)), PropE("x4", TP(1))), PropE("y1", TP(2))))
  }

  it should "rename multiple variables" in {
    termExpr1.renameAllVars(Seq("x2", "x3", "x4"), Seq("y2", "y3", "y4")) shouldEqual CurriedE(List(PropE("y2", TP(3)), PropE("y3", TP(2)), PropE("y4", TP(1))), PropE("y3", TP(2)))
  }

  it should "detect free variables" in {
    termExpr1.freeVars shouldEqual Set()
    termExpr2.freeVars shouldEqual Set("x1")
    termExpr3.freeVars shouldEqual Set()
  }
}
