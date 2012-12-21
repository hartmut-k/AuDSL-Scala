import AuDSLGrammar._
import AuDSLSemantics._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/* DR. HARTMUT KRASEMANN, IT-ARCHITEKT
** User: hartmut  Date: 21.12.12  Time: 11:10 */

class AuDSLEmptySmTest extends FlatSpec with ShouldMatchers with AuDSL {
  // prepare oven statechart app
  def emptySpec: String = """   """;
  // setup
  val sm: AuDSLState = this.create("empty", this.emptySpec)
  println(sm render)
  // do the tests
  "For the empty Statechart" should "parse the empty AuDSL text" in {
    sm.name should equal("nil")
  }
}