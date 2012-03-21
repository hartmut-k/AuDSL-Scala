import AuDSLGrammar._
import AuDSLSemantics._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FlatSpec, FunSuite}

/* DR. HARTMUT KRASEMANN, IT-ARCHITEKT
** User: hartmut  Date: 05.03.12  Time: 13:30 */

class AuDSLSimpleTest extends FlatSpec with ShouldMatchers with AuDSL {
// prepare a statechart
  val state = new AuDSLXorState("root", false)
  val son = new AuDSLXorState("son", false)
  val daughter = new AuDSLXorState("daughter", false)
  val kids = new AuDSLRegion("kids")
  val kidskids = new AuDSLRegion("kidskids")
  state.addChild(kids)
  kids.addChild(daughter)
  kids.addChild(son)
  son.addChild(kidskids)
// prepare a transition
  val transition = new AuDSLTransition("root", "toRoot", List("aGuard"), List("anAction"))
  kidskids.addTransition (transition)
  state writeTargets;
  state.model_=(this)
// prepare some application methods
  def anAction {println("** an Action called")}
  def aGuard = {println("** aGuard called"); true }
// do the tests
  "A simple Statechart" should "be constructed hierarchically" in  {
    state enter; println("** "+state.render)
    state.at("kidskids") should equal (kidskids)
    state.at("root") should equal (state)}
  it should  "be queried hierarchically" in {
    state.at("daughter") should equal (daughter)}
  "An Application(AuDSL)" should  "perform an action" in {
    this.perform("anAction") should  equal () }
  it should  "test a guard" in {
    this.test("aGuard") should  equal (true) }
  it should  "make a transition from kidskids to root" in  {
    state.at("kidskids").entered should  equal (true)
    state.process("toRoot")
    state.at("root").entered should  equal (true)
    state.at("kidskids").entered should  equal (false)
    state.at("son").entered should  equal (false)
    state.at("kids").entered should  equal (false) }
}