package AuDSLGrammar

import AuDSLSemantics._
import scala.util.parsing.combinator._

/* DR. HARTMUT KRASEMANN, IT-ARCHITEKT
** User: hartmut  Date: 14.02.12  Time: 10:35
** 17.03.2012  64 LOC */

trait AuDSL extends AuDSLParser {
  import scala.collection.mutable.Map
  val statecharts = Map.empty[String, AuDSLState]
  def statechartNamed(name: String) = statecharts(name)
  def perform(action: String) {this.getClass.getMethod(action).invoke(this)}
  def test(guard: String) = {this.getClass.getMethod(guard).invoke(this)==true}
  def create(aStateName: String): AuDSLState = {
    val m = this.getClass.getMethod(aStateName+"Spec")
    val spec = m.invoke(this).toString
    val sm = this.parseAll(AuState, spec) match {
      case Success(r,_) => r
      case NoSuccess(msg,input) => sys.error("AuDSL error: "+msg+" at "+input.pos) }
    sm.model_=(this) ;
    statecharts += (aStateName -> sm)
    sm }
}

trait AuDSLParser extends JavaTokenParsers {
  private def guards: Parser[List[String]]  = opt("["~>rep(ident)<~"]") ^^
    {case Some(g) => g; case None => List()}
  private def actions: Parser[List[String]] = opt("["~>(rep(ident)<~"]")) ^^
    {case Some(a) => a; case None => List()}
  private def transition: Parser[AuDSLTransition] = ident~guards~"->"~ident~actions ^^
    {case event~guards~"->"~target~actions => new AuDSLTransition(target, event, guards, actions)}
  private def onexit: Parser[List[String]]  = opt("onExit:"~"["~>rep(ident)<~"]") ^^
    {case Some(ex) => ex; case None => List()}
  private def onentry: Parser[List[String]] = opt("onEntry:"~"["~>rep(ident)<~"]") ^^
    {case Some(en) => en; case None => List()}
  private def history: Parser[Boolean] = opt("history") ^^ 
    {case Some(h) => true; case None => false}
  private def xortype: Parser[AuDSLState] = "("~>ident~history ^^ 
    {case i~h => new AuDSLXorState(i,h)}
  private def regiontype: Parser[AuDSLState] = "(r:"~>ident ^^ 
    (new AuDSLRegion(_))
  private def transitions: Parser[List[AuDSLTransition]] =  opt(rep(transition)) ^^
    {case Some(ts) => ts; case None => List()}
  private def states: Parser[List[AuDSLState]] =  opt(rep(state)) ^^
    {case Some(ss) => ss; case None => List()}
  private def state: Parser[AuDSLState] = (regiontype|xortype)~onentry~onexit~transitions~states<~")" ^^
    {case s~en~ex~ts~ss => 
      for (x <- ts) s.addTransition(x)
      for (x <- ss) s.addChild(x)
      s.entryActions ++= en
      s.exitActions ++= ex
      s }
  def AuState: Parser[AuDSLState] = rep(state) ^^
    {case head::tail =>
      {for (t <- tail) {
        val tbR = head.at(t.name)
        tbR.elder.replaceChild(tbR, t)}}
    head writeTargets;
    val errors = head.illegalTransitions + head.unknownTargets;
    if (!errors.isEmpty) sys.error("AuDSL error: "+errors) else head
    case Nil => new AuDSLNoState }
}