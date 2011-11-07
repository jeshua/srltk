package srltk.domains.rlstandard
import srltk.common._
import srltk.domains._
import srltk.driver.SimpleDriver
import srltk.vis.StateViewer
import srltk.vis.ActivePlot
import srltk.vis.ValueFunctionVisualizer2D

object MountainCarTests {
  class Policy1(gamma: Double = .99) extends TestPolicy(MountainCar.dd) {
    def V2D(x: Double, y: Double) = getValue(new MountainCarState(x, y))
    var i = 0
    var cache = new scala.collection.mutable.HashMap[(Double, Double), Double]

    def getEpVal(s: MountainCarState): Double = {
      if (s.absorbing) 0
      else {
        val a = s.successor(getAction(s))
        -1 + gamma * getEpVal(a)
      }
    }
    def reset() = ()
    override def getValue(s: SimState) : Double = {
      var state = s.asInstanceOf[MountainCarState];
      val st = (state.x, state.xDot)
      if (cache.contains(st)) {
        try{
          cache(st)
        } catch {
          case _ => {cache.remove(st)
          getValue(s)}
        }
      } else {

        val values = for (i <- 0 to 20)
          yield getEpVal(state)

        val v = values.foldLeft(0.0)(_ + _) / values.length
        cache.update((state.x, state.xDot), v)
        v
      }
    }

    def getInitialState() = MountainCar.getInitial

    def getAction(s: SimState) = {
      val state = s.asInstanceOf[MountainCarState]
      val x = state.x
      val v = state.xDot

      if (GlobalRNG.nextDouble() < .2) new MountainCarAction(GlobalRNG.nextInt(3))
      else {
        if (v <= 0.0)
          new MountainCarAction(0)
        else
          new MountainCarAction(2)
      }
    }
  }

  //======================================================================
  //show this policy
  def main(args: Array[String]): Unit = {

    //show value function
    val p = new Policy1(.99)
    val visualizer =
      new ValueFunctionVisualizer2D(MountainCar.bounds,
        p.V2D, false)

    //animate policy

    val mc = new MountainCar()
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
