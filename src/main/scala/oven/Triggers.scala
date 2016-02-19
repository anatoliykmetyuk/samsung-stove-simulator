package oven

import subscript.language
import subscript.Predef._

import subscript.objectalgebra._

object Triggers {
  val plateSelected    = new Switch
  val heatLevelChanged = new ValueTrigger[Int]

  val shutdownCommand  = new Trigger
  val shutdown         = new Trigger
}