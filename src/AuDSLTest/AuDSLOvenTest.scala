import AuDSLGrammar._
import AuDSLSemantics._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/* DR. HARTMUT KRASEMANN, IT-ARCHITEKT
** User: hartmut  Date: 10.03.12  Time: 10:56 */

class AuDSLOvenTest extends FlatSpec with ShouldMatchers with AuDSL {
  // prepare oven statechart app
  def enableTimeSetting: Unit = {}
  def disableTimeSetting: Unit = {}
  def startTimer: Unit = {}
  def stopTimer: Unit = {}
  def doorIsClosed: Boolean = statechartNamed("oven").at("closed").entered
  private def setup {sm.exit; sm.enter}
  def ovenSpec: String = """
  (r: oven
		(heater
			(idle
				onEntry:		[enableTimeSetting]
				onExit:  			[disableTimeSetting]
				start [doorIsClosed ] -> cooking )
			(cooking
				onEntry:		[startTimer]
				onExit: 		[stopTimer]
				open -> idle
				finish -> idle ) )
		(door
			history
			(open
				close -> closed )
			(closed
				open -> open) ) )
  """ ;
// setup
  val sm: AuDSLState = this.create("oven")
  println(sm render)
  setup
// do the tests
  "For the Microwave oven a Statechart" should "parse the oven AuDSL text" in {
    sm.name should equal("oven") }
  it should  "run the demo" in {
    println(sm.toString)
    sm.process("open")  ; println("*** open door *** \n"+sm.toString)
    sm.process("start") ; println("*** start ***     \n"+sm.toString)
    sm.process("close") ; println("*** close door ***\n"+sm.toString)
    sm.process("start") ; println("*** start ***     \n"+sm.toString)
    sm.process("finish") ; println("*** stop ***     \n"+sm.toString)
  }
}
