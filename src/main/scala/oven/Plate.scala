package oven

import subscript.language
import subscript.Predef._
import subscript.objectalgebra._

import subscript.swing.Scripts._

import scala.swing._
import scala.swing.BorderPanel.Position._

case class Plate(name: String) extends SSProcess {
  lazy val plate = new Label(name)

  lazy val displayBtn = new Button(name) {enabled = false}
  lazy val display    = new Label("00")

  script..
    live = displayBtn
           blink: display, time: 5000, delay: 250 / displayBtn
           let display.text = "00"

    blinkTimeDelay(label: Label, t: Long, d: Long) =
      [@gui: {!label.text = ""!} sleep: d @gui: {!label.text = "00"!} sleep: d ...] || sleep: t
}