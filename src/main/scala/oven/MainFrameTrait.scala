package oven

import subscript.language
import subscript.Predef._

import subscript.swing._
// import subscript.swing.Scripts._

import scala.swing._
import scala.swing.BorderPanel.Position._

trait MainFrameTrait extends FrameProcess {
  title       = "Samsung Oven"
  location    = new Point(300, 300)
  maximumSize = new java.awt.Dimension(1000, 600) 

  val plateLines = 2
  val platesModel = for (i <- 1 to plateLines * 2) yield Plate(s"Plate $i")

  // Plates
  val plates = new GridPanel(2, plateLines) {contents ++= platesModel.map(_.plate)}

  // Plate controls
  val plateControls = new GridPanel(2, plateLines * 2) {contents ++=
    platesModel.map(_.displayBtn)
      .zip(platesModel.map(_.display))
      .flatMap(_.productIterator.asInstanceOf[Iterator[Component]])
  }

  // Global controls
  val onOffBtn = new Button("On/Off") {enabled = false}
  val lockBtn  = new Button("Lock"  ) {enabled = false}
  val bridge   = new Button("Bridge") {enabled = false}

  case class HeatLevelButton(lbl: String, change: Int => Int) extends Button(lbl) {
    enabled = false
  }

  val heatLevelBtns = Seq(
    "+"  -> {(_: Int) + 1 }
  , "-"  -> {(_: Int) - 1 }
  , "3"  -> {(_: Int) + 3 }
  , "6"  -> {(_: Int) + 6 }
  , "11" -> {(_: Int) + 11}
  ).map {case (lbl: String, dh: (Int => Int)) => HeatLevelButton(lbl, dh)}

  val globalControls = new GridPanel(1, 2 + heatLevelBtns.size)  {contents ++=
    heatLevelBtns ++ Seq(bridge, lockBtn, onOffBtn)
  }

  // All controls
  val controls = new GridPanel(1, 2) {contents ++= Seq(plateControls, globalControls)}

  // Everything together
  val oven = new BorderPanel {
    layout(plates  ) = Center
    layout(controls) = South
  }

  contents = oven
}