import AuDSLGrammar._
import AuDSLSemantics._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/* DR. HARTMUT KRASEMANN, IT-ARCHITEKT
** User: hartmut  Date: 21.12.12  Time: 17:36 */

class AuDSLRawGrammarTest extends FlatSpec with ShouldMatchers with AuDSLRaw {
  def microwaveOven: String = """
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
				close -> closed)
			(closed
				open -> open) ) )
  """ ;
  // setup

  // do the tests
  "For the Microwave oven a Statechart" should "parse without an exception" in {
       this.parse(this.microwaveOven)
  }
}

