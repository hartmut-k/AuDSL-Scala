import AuDSLGrammar._
import AuDSLSemantics._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/* DR. HARTMUT KRASEMANN, IT-ARCHITEKT
** User: hartmut  Date: 21.12.12  Time: 11:10 */

class AuDSLSm1Test extends FlatSpec with ShouldMatchers with AuDSL {
// prepare oven statechart app
  def action1  {action1Done=true}
  def action2  {action2Done=true}
  def enter1x2 {enter1x2Done=true}
  def exit1x2  {exit1x2Done=true}
  def isGuarded = {isGuardedCalled=true; true}
  def isGuardedByDefault = {isGuardedByDefaultCalled=true; true}
  def failGuard = false
  private var action1Done = false
  private var action2Done = false
  private var enter1x2Done = false
  private var exit1x2Done = false
  private var isGuardedCalled = false
  private var isGuardedByDefaultCalled = false
  private def resetVars {action1Done=false; action2Done=false; enter1x2Done=false
    exit1x2Done=false; isGuardedCalled=false; isGuardedByDefaultCalled=false}
  private def setup {sm.exit; sm.enter; resetVars}

  def statechart1Spec: String = """
  (root
		(top1
			zz -> top2 [action1 action2]
			(top1x1
				aa [isGuarded isGuardedByDefault] -> top1x2
				bb -> top2x1
				ss -> top1x2	)
			(top1x2
				onEntry:		[enter1x2]
				onExit:			[exit1x2]
				gg -> top2x2x1 ) )
		(top2
			history
			yy -> top1
			(top2x1
				cc -> top2x2)
			(top2x2
				history
				dd [failGuard] -> top2x1
				(top2x2x1
					ee -> top2x2x2)
				(top2x2x2
					ff -> top1x2))))
	""" ;
// set up and test the parser
  val sm: AuDSLState = this.create("statechart1", this.statechart1Spec)
// do the tests
  "Statechart1" should "be parsed from AuDSL text" in {
    sm.name should equal("root")
    println(sm render)
  }
  it should  "do a simple transition" in  {
    setup
    sm.entered should be(true)
    sm.at("top1").entered should be(true)
    sm.at("top1x1").entered should be(true)
    sm.at("top1x2").entered should be(false)
    sm.at("top2").entered should be(false)
    sm.process("ss")
    sm.entered should be(true)
    sm.at("top1").entered should be(true)
    sm.at("top1x2").entered should be(true)
    sm.at("top1x1").entered should be(false)
    sm.at("top2").entered should be(false)
  }
  it should  "do a complex transition" in {
    setup
    sm.process("bb")
    sm.entered should equal(true)
    sm.at("top2").entered should be(true)
    sm.at("top2x1").entered should be(true)
    sm.at("top1").entered should be(false)
    sm.at("top1x1").entered should be(false)
    sm.at("top1x2").entered should be(false)
    sm.at("top2x2").entered should be(false)
    sm.at("top2x2x1").entered should be(false)
    sm.at("top2x2x2").entered should be(false)
  }
  it should "do a transition chain correctly" in {
    setup
    sm.process("aa"); sm.process("gg"); sm.process("ee"); sm.process("ff")
    sm.entered should be(true)
    sm.at("top1").entered should be(true)
    sm.at("top1x2").entered should be(true)
    sm.at("top1x1").entered should be(false)
    sm.at("top2").entered should be(false)
    sm.at("top2x1").entered should be(false)
    sm.at("top2x2").entered should be(false)
    sm.at("top2x2x1").entered should be(false)
    sm.at("top2x2x2").entered should be(false)
  }
  it should "perform event actions" in {
    setup
    sm.process("zz")
    action1Done should be(true)
    action2Done should be(true)
    sm.at("root").entered should be(true)
    sm.at("top2").entered should be(true)
    sm.at("top1").entered should be(false)
  }
  it should  "perform guards, entry and exit actions" in  {
    setup
    sm.process("aa"); sm.process("gg")
    isGuardedCalled should be(true)
    isGuardedByDefaultCalled should be(true)
    enter1x2Done should be(true)
    exit1x2Done should be(true)
    sm.at("top2x2x1").entered should be(true)
  }
  it should  "obey a guard" in  {
    setup
    sm.process("zz"); sm.process("cc")
    sm.entered should equal(true)
    sm.at("top2").entered should be(true)
    sm.at("top2x2").entered should be(true)
    sm.at("top2x1").entered should be(false)
    sm.process("dd")
    sm.at("top2x2").entered should be(true)
    sm.at("top2x1").entered should be(false)
  }
  it should  "obey the history" in  {
    setup
    sm.process("aa"); sm.process("gg"); sm.process("ee")
    sm.at("top2x2x2").entered should be(true)
    sm.process("yy"); sm.process("zz")
    sm.at("top2").entered should be(true)
    sm.at("top2x2").entered should be(true)
    sm.at("top2x2x2").entered should be(true)
    sm.at("top2x2x1").entered should be(false)
    sm.at("top2x1").entered should be(false)
  }
  it should  "ignore the history on direct jumps" in {
    setup
    sm.process("zz"); sm.process("cc"); sm.process("ee")
    sm.at("top2x2x2").entered should be(true)
    sm.process("yy"); sm.process("bb")
    sm.at("top2").entered should be (true)
    sm.at("top2x2").entered should be(false)
    sm.at("top2x2x2").entered should be(false)
    sm.at("top2x2x1").entered should be(false)
    sm.at("top2x1").entered should be(true)
  }
  it should  "work without history" in {
    setup
    sm.process("aa"); sm.process("zz"); sm.process("yy")
    sm.entered should equal(true)
    sm.at("top1").entered should be(true)
    sm.at("top1x1").entered should be(true)
    sm.at("top1x2").entered should be(false)
    sm.at("top2").entered should be(false)
    }
}