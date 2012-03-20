package AuDSLSemantics

import AuDSLGrammar._

/* DR. HARTMUT KRASEMANN, IT-ARCHITEKT
** User: hartmut  Date: 14.02.12  Time: 10:35
** 17.03.2012  20 LOC */

class AuDSLTransition(val targetName: String, val event: String, guards: List[String], actions: List[String]) {
  var model: AuDSL = _
  var target: AuDSLState = _
  def isTransition = true
  def executeActions {for (s <- actions) model.perform(s)}
  def guardsSucceedOn(anEvent:String):Boolean={(event==anEvent)&&guards.forall(g=>model.test(g))}
  override def toString = "Transition: "+event+"["+guards.toString+"] -> "+targetName+"["+actions.toString+"]"
  def writeTargetFor(state: AuDSLState) = {target = state.at(targetName) }
}

class AuDSLNoTransition extends AuDSLTransition("nil", "nil", List.empty, List.empty) {
  override def isTransition = false }