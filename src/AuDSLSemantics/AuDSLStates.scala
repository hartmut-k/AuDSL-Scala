package AuDSLSemantics

import AuDSLGrammar._

/* DR. HARTMUT KRASEMANN, IT-ARCHITEKT
** User: hartmut  Date: 14.02.12  Time: 10:35
** 17.03.2012  157 LOC */

abstract class AuDSLState(val name: String) {
  import scala.collection.mutable.Set
// accessing
  protected val children = Set.empty[AuDSLState]
  protected val transitions = Set.empty[AuDSLTransition]
  var log  = ""
  val entryActions = Set.empty[String]
  val exitActions = Set.empty[String]
  def enteredChild(): AuDSLState = {for (c <- children; if c.entered) return c; new AuDSLNoState }
  def at(aName: String): AuDSLState = {
    if (aName == name) return this
    for (c <- children; r = c.at(aName); if r.isState) return r
    new AuDSLNoState }    // no child "aName" found
// testing
  var entered = false
  def isState = true
  def isXor = false
// creating - parser
  private var eld : AuDSLState = _   // inhibit loop on construction
    def elder: AuDSLState = {if (eld != null) eld else new AuDSLNoState}
    def elder_=(aState: AuDSLState) {eld = aState}
  private var mdl : AuDSL = _
    def model: AuDSL = mdl
    def model_=(aModel: AuDSL) {  // must be set before use
      mdl = aModel
      for (t <- transitions) t.model_=(aModel)
      for (c <- children) c.model_=(aModel) }
  def addChild(child: AuDSLState) {children += child; child.elder_=(this)}
  def replaceChild(oldChild: AuDSLState, newChild: AuDSLState) {
    children -= oldChild; this.addChild(newChild) }
  def addTransition(transition: AuDSLTransition) {transitions += transition}
  def writeTargets {this writeTargetsIn(this)}
  def writeTargetsIn(aState: AuDSLState) {
    for (t <- transitions) t writeTargetFor(aState)
    for (c <- children) c writeTargetsIn(aState) }
  def unknownTargets: String = {
    val uT = this unknownTargetsR;
    if (uT != "") name+": "+uT else "" }
  def unknownTargetsR: String = {
    var res = ""
    for (t <- transitions; if !t.target.isState)
      res = res+"target "+t.targetName+" is unknown\n"
    for (c <- children) res = res+c.unknownTargetsR
    res }
  def illegalTransitions: String = {
    val ilT = this illegalTransitionsR;
    if (ilT != "") name+": "+ilT else "" }
  def illegalTransitionsR: String = {
    var res = ""
    for (t <- transitions; if !this.commonAncestorWith(t.target).isXor)
      res = res+"transition on "+t.event+" is illegal\n"
    for (c <- children) res = res+c.illegalTransitionsR
    res }
// processing
  def commonAncestorWith(aState: AuDSLState): AuDSLState = {
    val ancestors =  Set.empty[AuDSLState]
    var cand1 = aState   // first build the ancestor set:
    while (cand1 isState) {ancestors += cand1; cand1 = cand1 elder}
    var cand2 = this     // then find the ancestor:
    while (cand2.isState && !ancestors.contains(cand2)) {cand2 = cand2 elder}
    if (cand2.isXor) cand2 else new AuDSLNoState}
  def process(anEvent: String): Boolean = { // an event is processed in the bottommost level only
    var done = false
    this.logFor("received", anEvent)
    for (c <- children; if c.entered) done = c process(anEvent)
    if (!done) {var t = this.transitionFor(anEvent)
                if (t isTransition) {this switchTo(t.target); t executeActions; done=true}}
    done}
  def switchTo(aState: AuDSLState)  {
    val cA = this commonAncestorWith(aState);   // parser ensures that cA isXor
    this exitUpTo(cA); aState enterUpTo(cA) }
  def transitionFor(anEvent: String): AuDSLTransition = {
    val candidates1 = for (t <- transitions; if (t.event == anEvent)) yield t
    if (candidates1.size == 0) {this.logFor("no transition for", anEvent); return new AuDSLNoTransition}
    val candidates2 = for (t <- candidates1; if (t.guardsSucceedOn(anEvent))) yield t
    val c2s = candidates2.size
    if (c2s == 0) {this.logFor("guards failed for", anEvent); return new AuDSLNoTransition}
    this.logFor("!!"+c2s+"!! transition for", anEvent)
    candidates2.last }
// semantics
  def enter {
    for (a <- entryActions) model.perform(a)
    entered = true
    this enterChildren }
  def enterChildren: Unit // subclass responsibility
  def enterUpTo(aState: AuDSLState) {if (this != aState) {this enter; elder.enterUpTo(aState)}}
  def exit {
    this rememberHistory;
    this exitChildren;
    for (a <- exitActions) model.perform(a)
    entered = false}
  def exitChildren = {for (c <- children; if c.entered) c exit}
  def exitUpTo(aState: AuDSLState) {if (this != aState) {this exit; elder.exitUpTo(aState)} }
  def rememberHistory {} // default = do nothing
// printing
  def logFor(aString: String, anEvent: String) {log = log+" *** "+aString+" "+anEvent}
  override def toString() = this.toString(false) // print only entered states
  def render: String = "\n"+this.render(0)+"\n"
// private
  protected def printType: String      // subclass responsibility
  protected def toString(all: Boolean):String =  this.printType+this.toString(1, all)+"\n"
  def toString(i: Int, all: Boolean): String    // subclass responsibility
  protected def render(i: Int): String = {
    var out = this.tabs(i) + this.printType + this.name
    for (a <- entryActions) {out += this.tabs(i+1) + "onEntry: " + entryActions.toString}
    for (a <- exitActions)  {out += this.tabs(i+1) + "onExit:  " + exitActions.toString}
    for (t <- transitions)  {out += this.tabs(i+1) + t.toString}
    for (c <- children)     {out += c.render(i+1) } ; out}
  protected def tabs(indent: Int): String = {var out = "\n"; for (x <- 1 to indent) out += "\t"; out}
}

class AuDSLXorState(name: String, history: Boolean) extends AuDSLState(name) {
// accessing
  var firstChild: AuDSLState = new AuDSLNoState
  var historyChild: AuDSLState = new AuDSLNoState
// testing
  override def isXor = true
// creating - parser
  override def addChild(child: AuDSLState) = {if (children.size==0) firstChild=child; super.addChild(child)}
  override def replaceChild(oldChild: AuDSLState, newChild: AuDSLState) {
    super.replaceChild(oldChild: AuDSLState, newChild: AuDSLState)
    if (firstChild == oldChild) firstChild = newChild }
// semantics
  def enterChildren {
    if (!firstChild.isState || this.enteredChild.isState) return
    if (history && historyChild.isState) historyChild enter else firstChild enter }
  override def rememberHistory {if (history) historyChild = this enteredChild }
// private
  def toString(i: Int, all: Boolean) = {
    var out = this.name+"."
    for (c <- children; if (all|c.entered)) out += c.toString(i, all)
    out }
  def printType = "State: "
}

class AuDSLRegion(name: String) extends AuDSLState(name) {
// semantics
  def enterChildren = {for (c <- children) c enter}
// private
  def toString(i: Int, all: Boolean) = {
    var out = this.name+"."
    for (c <- children; if (all|c.entered)) out += tabs(i)+c.toString(i+1, all)
    out }
  def printType = "Region: "
}

class AuDSLNoState extends AuDSLRegion("nil") {
  override def isState = false
  override def printType = "NoState: " }