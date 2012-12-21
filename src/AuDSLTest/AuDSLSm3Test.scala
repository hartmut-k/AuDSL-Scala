import AuDSLGrammar.AuDSL
import AuDSLSemantics.AuDSLState
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/* DR. HARTMUT KRASEMANN, IT-ARCHITEKT
** User: hartmut  Date: 21.12.12  Time: 18:42 */

class AuDSLSm3Test extends FlatSpec with ShouldMatchers with AuDSL {
  // TestMachine3
  // Used for tests of illegal transitions and target names
  def statechart3Spec: String = """
   (world
		(r: root1
			zz -> root2
			(top1
				(r: top1x1
					aa -> top1x2
					bb -> top2x1
					yy -> wrong1
				(top1x1x1
					( top1x1x1x1
						a -> top1x1x1x2)
					( top1x1x1x2))
						( top1x1x2
							( top1x1x2x1)
							( top1x1x2x2)))
				(top1x2
					gg -> top2x2x1))
			(top2))
		( root2 ) )

( root2
			xx -> root1 )
(top2
	history
	(top2x1
		c -> wrong2)
	(top2x2 ))

(top2x2
	history
	d -> top2x1
	(top2x2x1
		ee -> top2x2x2)
	(top2x2x2
		ff -> top1x2 ))
	""";
  // test the target error
  "Statechart3" should "detect target errors" in {
    val thrown = evaluating {this.create("statechart3", this.statechart3Spec)} should produce [RuntimeException]
    thrown.getMessage should include ("is unknown")
    thrown.getMessage should include ("wrong1")
    thrown.getMessage should include ("wrong2")
  }
  "Statechart3" should "detect transition errors" in {
    val thrown = evaluating {this.create("statechart3", this.statechart3Spec)} should produce [RuntimeException]
    thrown.getMessage should include ("is illegal")
    thrown.getMessage should include ("gg")
    thrown.getMessage should include ("bb")
    thrown.getMessage should include ("ff")
    thrown.getMessage should include ("yy")
    thrown.getMessage should include ("c")
  }
}