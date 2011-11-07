package srltk.domains.rlstandard
import srltk.common._
import srltk.domains._
import srltk.driver.SimpleDriver
import srltk.vis.StateViewer
import srltk.vis.ActivePlot
import srltk.vis.ValueFunctionVisualizer2D

object PuddleWorldTests {
  class Policy1(gamma: Double) extends TestPolicy(PuddleWorld.dd) {
   
    def V2D(x: Double, y: Double) = getValue(new PuddleWorldState(x, y))
    var i = 0
    var cache = new scala.collection.mutable.HashMap[(Double, Double), Double]
    //= new java.util.HashMap[(Double,Double), Double] 
    def getEpVal(s: PuddleWorldState): Double = {
      if (s.absorbing) 0
      else PuddleWorld.getReward(s.x, s.y) + gamma * getEpVal(s.successor(getAction(s)))
    }
    def reset() = ()
    override def getValue(s: SimState) = {
      var state = s.asInstanceOf[PuddleWorldState];
      if (cache.contains((state.x, state.y))) {
        cache((state.x, state.y))
      } else {
        val values = for (i <- 0 to 20)
          yield getEpVal(state)
        val v = values.foldLeft(0.0)(_ + _) / values.length
        cache((state.x, state.y)) = v
        v
      }
    }

    def getInitialState() = PuddleWorld.getInitial

    def getAction(s: SimState) = {
      val state = s.asInstanceOf[PuddleWorldState]
      val x = state.x
      val y = state.y
      //if(x < .5 && (y > .3&& y < .8))
      //  PuddleWorldAction.get("South")
      if ((y > 0.05 && (y * 10).toInt % 2 == 0) || x > .95)
        PuddleWorldAction.get("North");
      else
        PuddleWorldAction.get("East")

    }
  }

  //======================================================================
  //show this policy
  def main(args: Array[String]): Unit = {

    //show value function
    val p = new Policy1(1)
    val visualizer = new ValueFunctionVisualizer2D(PuddleWorld.bounds, p.V2D, false)

    val mc = new PuddleWorld()
    val driver = new SimpleDriver(mc, new Policy1(1))
    val sv = new StateViewer(500, 500, mc.renderer)
    sv.setLocation(500, 0)

    for (i <- 1 to 100000000) {
      val st: SimState = driver.step()._1
      sv.updateDisplay(st)
      Thread.sleep(20)
    }
  }

}
