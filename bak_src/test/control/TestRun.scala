package srltk.test.control
import srltk.domains.rlstandard._
import srltk.algs.linear.learners.Sarsa
import srltk.driver.SimpleDriver
import srltk.vis.StateViewer
import srltk.vis.ActivePlot
import srltk.vis.ValueFunctionVisualizer2D
import util.control.Breaks._
import srltk.vis.MaxAVisualizer2D
import srltk.common._
import srltk.domains._
import srltk.algs._
import srltk.algs.linear._
import srltk.algs.linear.learners._
import srltk.algs.linear.agents._
import srltk.utils.Bounds2D
import srltk.vis.ValueFunctionVisualizer
import srltk.vis.ValueFunctionVisualizer3D

object TestRun {

  def apply(
    domain: SimDomain,
    agent: FeatAgent,
    timesteps: Option[Int],
    episodes: Option[Int],
    visuals: Boolean = true) {
    require(timesteps != None || episodes != None)
    val rng = new scala.util.Random()
    val driver = new SimpleDriver(domain, agent)

    //==================================================

    //record and show reward
    val plot =
      if (visuals) {
        val p = new ActivePlot()
        p.display()
        p.setLocation(500, 0)
        p
      } else null

    //see if we can display a 2d value function
    var updateVisuals: ((Int) => Unit) = (a : Int) => ()

    if (visuals && agent.isInstanceOf[HasQFunction] && domain.isInstanceOf[Domain2D]) {
      val aq = agent.asInstanceOf[FeatAgent with HasQFunction]
      val d2d = domain.asInstanceOf[Domain2D]
      val action = new IntAction(domain.domainDescription.numActions)
      def V2D(x: Double, y: Double) = {
        val o = d2d.createState(x, y).observation
        val v = for (i <- 0 until domain.domainDescription.numActions)
          yield aq.getQ(aq.toFeats(o), i);
        v.reduceLeft(scala.math.max(_, _))
      }
      def MaxA2D(x: Double, y: Double): Int = {
        val o = d2d.createState(x, y).observation
        aq.getMaxA(aq.toFeats(o));
      }
      val visualizer1 : ValueFunctionVisualizer = new ValueFunctionVisualizer3D(d2d.bounds, V2D)
      visualizer1.getFrame.setLocation(0,500)
      val visualizer2 = new MaxAVisualizer2D(d2d.bounds, MaxA2D, d2d.integerState)
      visualizer2.vis.setLocation(500,500)
      var calls = 0
      updateVisuals = (timestep : Int) => { 
    	calls = calls+1
        visualizer1.update();
        visualizer2.vis.repaint();
        if(calls == 2 || calls % 10 == 0)
          visualizer1.reRender()
    	  
      }
    }

    //++++++++++++++++++++

    def runEpisodes(maxEps: Int) {
      var timesteps: Int = 0
      var episodes = 0
      val step = 20
      var timestepSum : Double = 0
      breakable {
        for (i <- 1 to 10000000) {
          val data = driver.step()
          val reward = data._2.reward
          timesteps =
            if (data._5) {
              
              episodes = episodes + 1;
              if (visuals) {
                if(episodes % step == 0)
                {
                	plot.addPoint(episodes,timestepSum/step)
                	updateVisuals(timesteps)
                	timestepSum = 0
                } else {timestepSum += timesteps}
              } 
              0
            } else { timesteps + 1 }
          
          if (episodes > maxEps) break
        }
      }
    }

    //++++++++++++++++++++

    def runTimesteps(timesteps: Int) {
      val steps = 10000
      var sum = 0.0
      for (i <- 1 to timesteps) {
        val data = driver.step()

        val reward = data._2.reward
        if (i % steps == 0) {

          if (visuals) {
            plot.addPoint(i, sum / steps)
            updateVisuals(i)
          } //else println("Timestep: "+i)   

          sum = 0
          0
        } else { sum += reward }
      }      
    }
    //run the steps
    if (timesteps != None)
      runTimesteps(timesteps.get)
    else
      runEpisodes(episodes.get)
    //show the policy visually
    if (visuals) {

      if (agent.isInstanceOf[EGreedyAgent])
        agent.asInstanceOf[EGreedyAgent].disableExploration()

      val sv = new StateViewer(500, 500, domain.renderer)
      for (i <- 1 to 100000) {
        val data = driver.step()
        if (i % 1 == 0) {
          sv.updateDisplay(domain.state)
          Thread.sleep(20)
        }
      }
    }
  }
}
