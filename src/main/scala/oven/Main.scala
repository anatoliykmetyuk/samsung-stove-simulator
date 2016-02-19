package oven

import subscript.language
import subscript.Predef._
import subscript.SubScriptApplication

import subscript.swing._
import subscript.swing.Scripts._

import scala.swing._
import scala.swing.BorderPanel.Position._

import Util._
import Triggers._


object Main extends SubScriptApplication {
  script live = new MainFrame
}

class MainFrame extends MainFrameTrait with UtilScripts {
  script..
    live = [onOffBtn work / shutdown]...

    work =&& ui platesEngine

    ui =;&&
      forOp: "+", (for (p <- platesModel) yield [
        p.displayBtn

        p.selected.trigger
        plateSelected.switch: true

        {!!} [blink: p.display, {() => p.displayText}, time: 5000, delay: 500 ^false / heatLevelChanged ^true] ~~(again: Boolean)~~> while(again) / p.displayBtn
        @gui: let p.display.visible = true

        p.unselected.trigger
        plateSelected.switch: false
      ]) ...

      plateSelected forOp: "+", (for (btn <- heatLevelBtns) yield [btn]) ...

      onOffBtn shutdownCommand.trigger forOp: "&&", (for (p <- platesModel) yield [p.shutdownConfirmed]) shutdown.trigger
    
    platesEngine = forOp: "&&", (for (p <- platesModel) yield [p])
}

trait UtilScripts {script..
  blinkTimeDelay(label: Label, display: () => String, t: Long, d: Long) =
    [@gui: {!label.visible = true!} sleep: d @gui: {!label.visible = false!} sleep: d ...] || sleep: t

  noMessagesBeforeMe =
    var flag = false
    {!flag = here.scriptExecutor.msgQueue.collection.exists(_.node.index < here.index)!}      
    while(flag)
    sleep: 10
}