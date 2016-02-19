package oven

import subscript.language
import subscript.Predef._
import subscript.objectalgebra._

import subscript.swing.Scripts._

import scala.swing._
import scala.swing.BorderPanel.Position._

import java.awt.Color
import javax.swing.BorderFactory

import Triggers._

case class Plate(name: String) extends SSProcess {
  lazy val plate = new Label(name) {
    val margin = 10

    opaque = true
    background = plateBackground

    border = BorderFactory.createCompoundBorder(
      BorderFactory.createLineBorder(Color.YELLOW)
    , BorderFactory.createEmptyBorder(margin, margin, margin, margin)
    )
  }

  lazy val displayBtn = new Button(name) {enabled = false}
  lazy val display    = new Label("0")

  val selected
    , unselected
    , heatChanged
    , heatReactionFinished
    , shutdownConfirmed = new Trigger

  private var _heat = 0

  def heat_=(x: Int) = {_heat = x; heatChanged.trigger}
  def heat = _heat

  def displayText = {
    val txt = _heat.toString
    if (txt.size < 2) s"0$txt" else txt
  }

  def plateBackground = new Color(math.max(0, math.min(heat * 10, 255)), 0, 100)

  script..
    live = uiControl && reactions

    uiControl =
      selected
      heatLevelChanged ~~(x: Int)~~> let heat += x ... / unselected
      ...

    reactions =;&&
      heatChanged @gui: {!display.text = displayText; plate.background = plateBackground!} heatReactionFinished.trigger ...
      shutdownCommand let heat = 0; let display.visible = true; heatReactionFinished shutdownConfirmed.trigger
}

case class HeatLevelButton(lbl: String, change: Int) extends SSProcess {
  val btn = new Button(lbl) {enabled = false}

  script live = btn heatLevelChanged.push: change
}