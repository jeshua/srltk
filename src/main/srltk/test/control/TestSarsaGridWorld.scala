package srltk.test.control

import srltk.domains.rlstandard._
import srltk.algs.linear.agents.EGreedySarsa

object TestSarsaGridWorld {
  import GridWorld._

  def main(args: Array[String]): Unit = {

    val rng = new scala.util.Random()
    //create mountain car
    val domain = new GridWorld()
    val agent = new EGreedySarsa(GridWorld.dd,.01, .01, 0, .99, 0)
    TestRun(domain, agent,Some(1000),None);
  }
}
