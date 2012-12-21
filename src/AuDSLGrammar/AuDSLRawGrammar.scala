package AuDSLGrammar

import AuDSLSemantics._
import scala.util.parsing.combinator._

/* DR. HARTMUT KRASEMANN, IT-ARCHITEKT
** User: hartmut  Date: 18.12.12  Time: 16:32
** 18.12.2012  30 LOC */

trait AuDSLRaw extends AuDSLRawParser {
  def parse(aDSLtext: String)  = {
      this.parseAll(AuState, aDSLtext) match {
      case Success(r,_) => r
      case NoSuccess(msg,input) => sys.error("AuDSL error: "+msg+" at "+input.pos)}}
  }

trait AuDSLRawParser extends JavaTokenParsers {
  private def guards: Parser[Any]      = opt("["~rep(ident)~"]")
  private def actions: Parser[Any]     = opt("["~(rep(ident)~"]"))
  private def transition: Parser[Any]  = ident~guards~"->"~ident~actions
  private def onexit: Parser[Any]      = opt("onExit:"~actions)
  private def onentry: Parser[Any]     = opt("onEntry:"~actions)
  private def history: Parser[Any]     = opt("history")
  private def xortype: Parser[Any]     = "("~ident~history
  private def regiontype: Parser[Any]  = "(r:"~ident
  private def transitions: Parser[Any] = opt(rep(transition))
  private def states: Parser[Any]      = opt(rep(state))
  private def state: Parser[Any]       = (regiontype|xortype)~onentry~onexit~transitions~states~")"
  def AuState: Parser[Any]             = state
}