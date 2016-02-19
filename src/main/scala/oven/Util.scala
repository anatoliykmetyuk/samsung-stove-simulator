package oven

// Interface
import subscript.language
import subscript.Predef._

import subscript.objectalgebra._

// Internals
import subscript.vm._
import subscript.DSL._

import subscript.vm.model.callgraph._
import subscript.vm.model.template.concrete._
import subscript.vm.model.template._
import subscript.vm.model.template.TemplateNode.Child


object Util {

  def forPayload(scripts: Seq[Script[Any]], s: Script[Any], doYield: Boolean) = scripts
    .zipWithIndex
    .map {case (c, id) =>
      _at[CallGraphNode, Child](here => {
        implicit val there: CallGraphNode = here.there;
        if (doYield) {
          there.pass = id
          _double_caret(there, s)
        } else _caret(there, s)
      }).apply(_maybeCall("", (here: CallGraphTreeNode) => _maybeVarCall("c")))
    }

  def forScript(op: String, scripts: Seq[Script[Any]], doYield: Boolean) = subscript.DSL._script[Any](None, Symbol("sample")){(_node: subscript.vm.Script[Any]) =>
    implicit val script = _node
    _op(op)(forPayload(scripts, script, doYield): _*)
  }

  def forOpYield(op: String, scripts: Seq[Script[Any]]) = forScript(op, scripts, true )
  def forOpUnit (op: String, scripts: Seq[Script[Any]]) = forScript(op, scripts, false)

}