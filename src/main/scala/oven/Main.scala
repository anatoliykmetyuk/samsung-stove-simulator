package oven

import subscript.language
import subscript.Predef._
import subscript.SubScriptApplication

import subscript.swing.Scripts._

import Util._


object Main extends SubScriptApplication {
  script live = new MainFrame
}

class MainFrame extends MainFrameTrait {
  script..
    live = [on work / off]...

    on  = onOffBtn
    off = onOffBtn

    work = forOp: "+", unit: (for (p <- platesModel) yield [p]) ...

}