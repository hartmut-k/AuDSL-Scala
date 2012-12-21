import AuDSLGrammar.AuDSL
import AuDSLSemantics.AuDSLState
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/* DR. HARTMUT KRASEMANN, IT-ARCHITEKT
** User: hartmut  Date: 21.12.12  Time: 20:49 */

class AuDSLSm2Test extends FlatSpec with ShouldMatchers with AuDSL {
// TestMachine2 carries a statemachine with regions, 6 levels deep.
// Used for tests of history and region exits
// states have been replaced during construction
  private def setup {sm.exit; sm.enter}

  def statechart2Spec: String = """
  (world
		(r: root1
			zz -> root2
			(top1)
			(top2
				history
				(top2x1
					c -> top2x2)
				(top2x2
					history
					d -> top2x1
					(top2x2x1
						ee -> top2x2x2)
					(top2x2x2))))
		( root2
			xx -> root1 ))
  (top1
		(r: top1x1)
		(top1x2))
	(r: top1x1
		aa -> top1x2
		yy -> root2
		(top1x1x1
			( top1x1x1x1
				a -> top1x1x1x2)
			( top1x1x1x2))
		( top1x1x2
			( top1x1x2x1)
			( top1x1x2x2)))
	""";
// set up and test the parser
  var sm: AuDSLState = this.create("statechart2", this.statechart2Spec)
// do the tests
  "Statechart2" should "be parsed from a multi state AuDSL text" in {
    sm.name should equal("world")
    println(sm render)
  }
  it should "obey the history" in {
    setup; sm.process("aa"); sm.process("c"); sm.process("ee")
    sm.entered should be(true)
    sm.at("root1").entered should be(true)
    sm.at("root2").entered should be(false)
    sm.at("top1").entered should be(true)
    sm.at("top1x1").entered should be(false)
    sm.at("top1x2").entered should be(true)
    sm.at("top2").entered should be(true)
    sm.at("top2x1").entered should be(false)
    sm.at("top2x2").entered should be(true)
    sm.at("top2x2x2").entered should be(true)
    sm.process("zz"); sm.process("xx")
    sm.entered should be(true)
    sm.at("top1").entered should be(true)
    sm.at("top2").entered should be(true)
    sm.at("top1x1").entered should be(true)  // here is no history
    sm.at("top1x2").entered should be(false)
    sm.at("top2x1").entered should be(false)
    sm.at("top2x2x2").entered should be(true) // here is history
  }
  it should "multi exit regions" in {
    sm = this.create("statechart2", this.statechart2Spec)   // we need a fresh sm without history track
    setup;
    sm.entered should be(true)
    sm.at("top1").entered should be(true)
    sm.at("top1x1").entered should be(true)
    sm.at("top2").entered should be(true)
    sm.at("top2x1").entered should be(true)
    sm.process("aa")
    sm.at("top1x1").entered should be(false)
    sm.at("top2").entered should be(true)
    sm.at("top1x2").entered should be(true)
    sm.process("zz")
    sm.entered should be(true)
    sm.at("root1").entered should be(false)
    sm.at("root2").entered should be(true)
    sm.at("top1").entered should be(false)
    sm.at("top2").entered should be(false)
    sm.at("top1x1").entered should be(false)
    sm.at("top2x1").entered should be(false)
    sm.at("top2x2x1").entered should be(false)
    sm.at("top1x2").entered should be(false)
    sm.at("top2x2").entered should be(false)
    sm.at("top2x2x2").entered should be(false)
  }
}